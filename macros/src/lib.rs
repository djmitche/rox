extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use std::collections::HashMap;
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    spanned::Spanned,
    token::Comma,
    AngleBracketedGenericArguments, Expr, Field, FieldValue, Fields, FnArg, GenericArgument, Ident,
    Item, ItemEnum, ItemStruct, Pat, PatIdent, PatType, Path, PathArguments, PathSegment, Token,
    TraitBound, TraitBoundModifier, Type, TypeImplTrait, TypeParamBound,
};

struct Items(Vec<Item>);

impl Parse for Items {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let mut items = Vec::new();
        while !input.is_empty() {
            items.push(input.parse()?);
        }
        Ok(Items(items))
    }
}

/// Define a set of related types as an AST.
///
/// The caller must have `src::Src`, `ast::Node` and `ast::NodeRef` in scope.
///
/// Each type is outfitted with constructors. For structs, that is `new`, while for enums one
/// constructor is created for each variant, with a lower-case name. Constructors take a `Src`
/// as first argument, follwed by the fields in the struct or variant. Each field is defined as
/// `impl Into<T>`, and calls to `.into()` added.
///
/// Traits `Debug`, `PartialEq`, and `Eq` are derived for each type.
///
/// ## Visitor Pattern
///
/// A `Visitor` trait is defined with three methods for each type:
///
///  * `fn type_start(&mut self, val: &mut Type) -> Result<bool>`
///  * `fn type_recurse(&mut self, val: &mut Type) -> Result<()>`
///  * `fn type_end(&mut self, val: &mut Type) -> Result<()>`
///
/// This trait can be implemented for a type that will implement the visitor pattern over an AST.
/// Visting begins with a call to [`Node::traverse`], passing a reference to the visitor. This
/// begins a recursive descent into the AST. For each node, it first invokes `type_start`, then
/// `type_recurse`, then `type_end`. If `type_start` returns false, the other to methods are not
/// called. All three methods have default implementations, with `type_recurse` automatically
/// recursing into any component values of type [`Node`] or [`NodeRef`] or an `Option` or `Vec` of
/// those.

#[proc_macro]
pub fn ast(input: TokenStream) -> TokenStream {
    AstGenerator::default().generate(input)
}

#[derive(Default)]
struct AstGenerator {
    /// Output type definitions, by name.
    ///
    /// These will be output with the relevant derive macro invocations.
    out_type_defs: HashMap<Ident, proc_macro2::TokenStream>,

    /// Output impls for each type.
    ///
    /// These will be output following each type, wrapped in `impl Type`.
    out_type_impls: HashMap<Ident, proc_macro2::TokenStream>,

    /// Node<K> impls for each type.
    ///
    /// These will be output following each type impl, wrapped in `impl Node<Type>`.
    out_node_type_impls: HashMap<Ident, proc_macro2::TokenStream>,

    /// Visitor trait definition.
    ///
    /// This will be wrapped in `trait Visitor`.
    out_visitor_trait: proc_macro2::TokenStream,

    /// Items passed through without change
    out_passthrough: proc_macro2::TokenStream,
}

impl AstGenerator {
    fn generate(mut self, input: TokenStream) -> TokenStream {
        let items = syn::parse_macro_input!(input as Items);
        let mut result = proc_macro2::TokenStream::new();
        for item in items.0 {
            if let Err(e) = self.handle_ast_item(item) {
                result.extend(e.into_compile_error());
            }
        }

        // If there were errors, return them.
        if !result.is_empty() {
            return result.into();
        }

        for (name, tokens) in self.out_type_defs.drain() {
            result.extend(quote! {
                #[derive(Debug, PartialEq, Eq)]
                #tokens
            });

            if let Some(tokens) = self.out_type_impls.remove(&name) {
                result.extend(quote! {
                    #[allow(dead_code)]
                    impl #name {
                        #tokens
                    }
                });
            }

            if let Some(tokens) = self.out_node_type_impls.remove(&name) {
                result.extend(quote! {
                    #[allow(dead_code)]
                    impl Node<#name> {
                        #tokens
                    }
                });
            }
        }

        let tokens = self.out_visitor_trait;
        result.extend(quote! {
            #[allow(dead_code)]
            pub trait Visitor: Sized {
                #tokens
            }
        });

        result.extend(std::mem::take(&mut self.out_passthrough));
        result.into()
    }

    fn handle_ast_item(&mut self, item: Item) -> syn::Result<()> {
        match item {
            Item::Struct(item) => self.struct_item(item)?,
            Item::Enum(item) => self.enum_item(item)?,
            //Item::Trait(item) if item.ident.to_string() == "Visitor" => visitor_trait_item(item),
            _ => self.out_passthrough.extend(quote!(#item)),
        };
        Ok(())
    }

    fn struct_item(&mut self, item: ItemStruct) -> syn::Result<()> {
        let struct_name = &item.ident;
        let vis = &item.vis;

        self.out_type_defs.insert(
            struct_name.clone(),
            quote! {
                #item
            },
        );

        // Create a constructor.
        let mut methods = proc_macro2::TokenStream::new();
        let Fields::Named(named_fields) = &item.fields else {
            // TODO: support tuple fields
            panic!("expected named fields");
        };
        let args: Punctuated<FnArg, Comma> =
            named_fields.named.iter().map(fn_arg_impl_into).collect();
        let long_init: Punctuated<FieldValue, Comma> = named_fields
            .named
            .iter()
            .map(|f| {
                let name = f.ident.as_ref().map(|i| i.to_string()).unwrap();
                syn::parse_str::<FieldValue>(&format!("{name}: {name}.into()")).unwrap()
            })
            .collect();
        methods.extend(quote! {
            #[inline]
            #vis fn new(src: Src, #args) -> Node<#struct_name> {
                Node {
                    src,
                    inner: #struct_name { #long_init },
                }
            }
        });
        self.out_type_impls.insert(struct_name.clone(), methods);

        self.add_visitor_start(struct_name)?;

        // Create a <type>_recurse fn.
        let lower_name = struct_name.to_string().to_lowercase();
        let recurse = Ident::new(&format!("{lower_name}_recurse"), struct_name.span());
        let recurse_body = recurse_for_fields(&item.fields, |ident| quote! { (&mut val.#ident) })?;
        self.out_visitor_trait.extend(quote! {
            #[inline]
            fn #recurse(&mut self, val: &mut #struct_name) -> Result<()> {
                #recurse_body
                Ok(())
            }
        });

        self.add_visitor_end(struct_name)?;
        self.add_node_type_impl(struct_name)?;

        Ok(())
    }

    fn enum_item(&mut self, item: ItemEnum) -> syn::Result<()> {
        let enum_name = &item.ident;
        let vis = &item.vis;

        self.out_type_defs.insert(
            enum_name.clone(),
            quote! {
                #item
            },
        );

        // Create a constructor for each variant, returning `Node<K>`, and a `K_recurse` Visitor
        // method.
        let mut methods = proc_macro2::TokenStream::new();
        let mut recurse_match_arms = proc_macro2::TokenStream::new();

        for variant in &item.variants {
            let var_name = &variant.ident;
            let constr_name = lowercase(var_name);

            let recurse_body = recurse_for_fields(&variant.fields, |ident| quote! { #ident })?;

            match &variant.fields {
                Fields::Named(fields) => {
                    let args: Punctuated<FnArg, Comma> =
                        fields.named.iter().map(fn_arg_impl_into).collect();
                    let long_init: Punctuated<FieldValue, Comma> = fields
                        .named
                        .iter()
                        .map(|f| {
                            let name = f.ident.as_ref().map(|i| i.to_string()).unwrap();
                            syn::parse_str::<FieldValue>(&format!("{name}: {name}.into()")).unwrap()
                        })
                        .collect();
                    methods.extend(quote! {
                        #[inline]
                        #vis fn #constr_name(src: Src, #args) -> Node<#enum_name> {
                            Node {
                                src,
                                inner: #enum_name::#var_name { #long_init },
                            }
                        }
                    });
                    if !recurse_body.is_empty() {
                        let field_pats: Punctuated<PatIdent, Comma> = fields
                            .named
                            .iter()
                            .map(|f| pat_ident(f.ident.as_ref().unwrap().clone()))
                            .collect();
                        recurse_match_arms.extend(quote! {
                            #enum_name::#var_name { #field_pats } => {
                                #recurse_body
                            },
                        });
                    }
                }
                Fields::Unnamed(fields) => {
                    let constr_args: Punctuated<FnArg, Comma> = fields
                        .unnamed
                        .iter()
                        .enumerate()
                        .map(|(i, f)| {
                            let mut f = f.clone();
                            f.ident = Some(Ident::new(&format!("v{i}"), f.ty.span()));
                            fn_arg_impl_into(&f)
                        })
                        .collect();
                    let constr_fields: Punctuated<Expr, Comma> = fields
                        .unnamed
                        .iter()
                        .enumerate()
                        .map(|(i, _)| syn::parse_str::<Expr>(&format!("v{i}.into()")).unwrap())
                        .collect();
                    methods.extend(quote! {
                        #[inline]
                        #vis fn #constr_name(src: Src, #constr_args) -> Node<#enum_name> {
                            Node {
                                src,
                                inner: #enum_name::#var_name(#constr_fields),
                            }
                        }
                    });
                    if !recurse_body.is_empty() {
                        let match_args: Punctuated<PatIdent, Comma> = fields
                            .unnamed
                            .iter()
                            .enumerate()
                            .map(|(i, f)| pat_ident(Ident::new(&format!("v{i}"), f.ident.span())))
                            .collect();
                        recurse_match_arms.extend(quote! {
                            #enum_name::#var_name ( #match_args ) => {
                                #recurse_body
                            },
                        });
                    }
                }
                Fields::Unit => {
                    methods.extend(quote! {
                        #[inline]
                        #vis fn #constr_name(src: Src) -> Node<#enum_name> {
                            Node {
                                src,
                                inner: #enum_name::#var_name,
                            }
                        }
                    });
                }
            };
        }
        self.out_type_impls.insert(enum_name.clone(), methods);

        self.add_visitor_start(enum_name)?;
        let lower_name = enum_name.to_string().to_lowercase();
        let recurse = Ident::new(&format!("{lower_name}_recurse"), enum_name.span());
        self.out_visitor_trait.extend(quote! {
            #[inline]
            fn #recurse(&mut self, val: &mut #enum_name) -> crate::error::Result<()> {
                #[allow(unused_variables, unreachable_patterns)]
                match val {
                    #recurse_match_arms
                    _ => {}
                };
                Ok(())
            }
        });
        self.add_visitor_end(enum_name)?;

        self.add_node_type_impl(enum_name)?;

        Ok(())
    }

    fn add_visitor_start(&mut self, name: &Ident) -> syn::Result<()> {
        let lower_name = name.to_string().to_lowercase();
        let pre = Ident::new(&format!("{lower_name}_start"), name.span());
        self.out_visitor_trait.extend(quote! {
            #[inline]
            fn #pre(&mut self, val: &mut #name) -> crate::error::Result<bool> {
                Ok(true)
            }
        });
        Ok(())
    }

    fn add_visitor_end(&mut self, name: &Ident) -> syn::Result<()> {
        let lower_name = name.to_string().to_lowercase();
        let post = Ident::new(&format!("{lower_name}_end"), name.span());
        self.out_visitor_trait.extend(quote! {
            #[inline]
            fn #post(&mut self, val: &mut #name) -> Result<()> {
                Ok(())
            }
        });
        Ok(())
    }

    fn add_node_type_impl(&mut self, name: &Ident) -> syn::Result<()> {
        let mut node_methods = proc_macro2::TokenStream::new();

        let lower_name = name.to_string().to_lowercase();
        let pre = Ident::new(&format!("{lower_name}_start"), name.span());
        let recurse = Ident::new(&format!("{lower_name}_recurse"), name.span());
        let post = Ident::new(&format!("{lower_name}_end"), name.span());

        node_methods.extend(quote! {
            #[inline]
            pub fn traverse<V: Visitor>(&mut self, vis: &mut V) -> crate::error::Result<()> {
                if !vis.#pre(&mut self.inner)? {
                    return Ok(());
                }
                vis.#recurse(&mut self.inner)?;
                vis.#post(&mut self.inner)
            }
        });

        self.out_node_type_impls.insert(name.clone(), node_methods);
        Ok(())
    }
}

/// Build code to recurse into the given fields. If unnamed, they are assumed to have names v0,
/// v1, ..
fn recurse_for_fields<F: FnMut(&Ident) -> proc_macro2::TokenStream>(
    fields: &Fields,
    mut val_expr: F,
) -> syn::Result<proc_macro2::TokenStream> {
    let mut result = proc_macro2::TokenStream::new();
    match fields {
        Fields::Named(fields) => {
            for field in &fields.named {
                result.extend(recurse_for_value(
                    val_expr(field.ident.as_ref().unwrap()),
                    &field.ty,
                )?);
            }
        }
        Fields::Unnamed(fields) => {
            for (i, field) in fields.unnamed.iter().enumerate() {
                let name = Ident::new(&format!("v{i}"), field.ty.span());
                result.extend(recurse_for_value(val_expr(&name), &field.ty)?);
            }
        }
        Fields::Unit => {}
    }
    Ok(result)
}

/// Build code to recurse for a single value. This is where we special-case all of the Node
/// types and variants.
fn recurse_for_value(
    value: proc_macro2::TokenStream,
    ty: &Type,
) -> syn::Result<proc_macro2::TokenStream> {
    Ok(match TraversableType::from_type(ty) {
        Some(TraversableType::NodeK) | Some(TraversableType::NodeRefK) => {
            quote! { #value.traverse(self)?; }
        }
        Some(TraversableType::OptionNodeK) | Some(TraversableType::OptionNodeRefK) => {
            quote! { if let Some(v) = #value { v.traverse(self)?; } }
        }
        Some(TraversableType::VecNodeK) | Some(TraversableType::VecNodeRefK) => {
            quote! { for v in #value { v.traverse(self)?; } }
        }
        None => {
            // Not recursible.
            Default::default()
        }
    })
}

/// The various types we support traversing.
enum TraversableType {
    /// Node<..>
    NodeK,
    /// NodeRef<..>
    NodeRefK,
    /// Option<Node<..>>
    OptionNodeK,
    /// Option<NodeRef<..>>
    OptionNodeRefK,
    /// Vec<Node<..>>
    VecNodeK,
    /// Vec<NodeRef<..>>
    VecNodeRefK,
}

impl TraversableType {
    fn from_type(ty: &Type) -> Option<Self> {
        let Type::Path(ty) = ty else {
            return None;
        };
        let path = &ty.path;
        if path.leading_colon.is_some() || path.segments.len() != 1 {
            return None;
        }
        let type_name = &path.segments[0].ident;
        if type_name == "Node" {
            Some(TraversableType::NodeK)
        } else if type_name == "NodeRef" {
            Some(TraversableType::NodeRefK)
        } else if type_name == "Option" || type_name == "Vec" {
            let PathArguments::AngleBracketed(arguments) = &path.segments[0].arguments else {
                return None;
            };
            if arguments.args.len() != 1 {
                return None;
            }
            let GenericArgument::Type(ty) = &arguments.args[0] else {
                return None;
            };
            match TraversableType::from_type(ty) {
                Some(TraversableType::NodeK) if type_name == "Option" => {
                    Some(TraversableType::OptionNodeK)
                }
                Some(TraversableType::NodeRefK) if type_name == "Option" => {
                    Some(TraversableType::OptionNodeRefK)
                }
                Some(TraversableType::NodeK) if type_name == "Vec" => {
                    Some(TraversableType::VecNodeK)
                }
                Some(TraversableType::NodeRefK) if type_name == "Vec" => {
                    Some(TraversableType::VecNodeRefK)
                }
                _ => None,
            }
        } else {
            None
        }
    }
}

/// Create a `mut ref name` PatIdent.
fn pat_ident(name: Ident) -> PatIdent {
    PatIdent {
        attrs: Vec::new(),
        by_ref: None,
        mutability: None,
        ident: name,
        subpat: None,
    }
}

/// Return the lower-case version of an identifier.
fn lowercase(ident: &Ident) -> Ident {
    let name = ident.to_string().to_lowercase();
    Ident::new(&name, ident.span())
}

/// Given a field, make a FnArg for it using `impl Into<f.ty>`.
fn fn_arg_impl_into(f: &Field) -> FnArg {
    FnArg::Typed(PatType {
        attrs: Vec::new(),
        pat: Box::new(Pat::Ident(pat_ident(f.ident.clone().unwrap()))),
        colon_token: Token![:](f.span()),
        ty: Box::new(impl_into(&f.ty)),
    })
}

/// Turn `ty` into `impl Into<ty>`.
fn impl_into(ty: &Type) -> Type {
    Type::ImplTrait(TypeImplTrait {
        impl_token: Token![impl](ty.span()),
        bounds: [TypeParamBound::Trait(TraitBound {
            paren_token: None,
            modifier: TraitBoundModifier::None,
            lifetimes: None,
            path: Path {
                leading_colon: None,
                segments: [PathSegment {
                    ident: syn::parse_str("Into").unwrap(),
                    arguments: PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                        colon2_token: None,
                        lt_token: Token![<](ty.span()),
                        args: [GenericArgument::Type(ty.clone())].into_iter().collect(),
                        gt_token: Token![>](ty.span()),
                    }),
                }]
                .into_iter()
                .collect(),
            },
        })]
        .into_iter()
        .collect(),
    })
}
