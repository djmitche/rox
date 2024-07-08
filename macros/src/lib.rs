extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use std::collections::HashMap;
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    spanned::Spanned,
    token::Comma,
    AngleBracketedGenericArguments, Error, Expr, Field, FieldValue, Fields, FnArg, GenericArgument,
    Ident, Item, ItemEnum, ItemStruct, Pat, PatIdent, PatType, Path, PathArguments, PathSegment,
    Token, TraitBound, TraitBoundModifier, Type, TypeImplTrait, TypeParamBound,
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
            trait Visitor {
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
        let Fields::Named(fields) = item.fields else {
            panic!("expected named fields");
        };
        let args: Punctuated<FnArg, Comma> = fields.named.iter().map(fn_arg_impl_into).collect();
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
            #vis fn new(src: Src, #args) -> Node<#struct_name> {
                Node {
                    src,
                    inner: #struct_name { #long_init },
                }
            }
        });
        self.out_type_impls.insert(struct_name.clone(), methods);

        self.add_visitor_pre(struct_name)?;
        self.add_visitor_post(struct_name)?;
        self.add_node_type_impl(struct_name)?;

        // TODO: visitor recurse fn

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

        // Create a constructor for each variant, returning `Node<K>`.
        let mut methods = proc_macro2::TokenStream::new();
        for variant in &item.variants {
            let var_name = &variant.ident;
            let constr_name = lowercase(var_name);

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
                }
                Fields::Unnamed(fields) => {
                    let args: Punctuated<FnArg, Comma> = fields
                        .unnamed
                        .iter()
                        .enumerate()
                        .map(|(i, f)| {
                            let mut f = f.clone();
                            f.ident = Some(Ident::new(&format!("v{i}"), f.ty.span()));
                            fn_arg_impl_into(&f)
                        })
                        .collect();
                    let fields: Punctuated<Expr, Comma> = fields
                        .unnamed
                        .iter()
                        .enumerate()
                        .map(|(i, _)| syn::parse_str::<Expr>(&format!("v{i}.into()")).unwrap())
                        .collect();
                    methods.extend(quote! {
                        #[inline]
                        #vis fn #constr_name(src: Src, #args) -> Node<#enum_name> {
                            Node {
                                src,
                                inner: #enum_name::#var_name(#fields),
                            }
                        }
                    });
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

        // Build the foo_recurse function
        let mut match_body = proc_macro2::TokenStream::new();

        for variant in item.variants {
            let var_name = &variant.ident;
            match &variant.fields {
                Fields::Named(fields) => {
                    todo!(); // TODO
                }
                Fields::Unnamed(fields) => {
                    let args: Punctuated<Ident, Comma> = fields
                        .unnamed
                        .iter()
                        .enumerate()
                        .map(|(i, f)| Ident::new(&format!("v{i}"), f.ident.span()))
                        .collect();
                    match_body.extend(quote! {
                        // TODO: traverse the Node/NodeRef values.
                        #var_name ( #args ) => todo!(),
                    });
                }
                Fields::Unit => {}
            };
        }
        let lower_name = enum_name.to_string().to_lowercase();
        let recurse = Ident::new(&format!("{lower_name}_recurse"), enum_name.span());
        self.out_visitor_trait
            .extend(quote! {
                #[inline]
                fn #recurse(&mut self, val: &mut #enum_name) -> Result<()> {
                    #[allow(unused_variables, unreachable_patterns)]
                    match val {
                        #match_body
                        _ => {}
                    };
                    Ok(())
                }
            });

        self.add_visitor_pre(enum_name)?;
        self.add_visitor_post(enum_name)?;
        self.add_node_type_impl(enum_name)?;

        // TODO: visitor recurse fn

        Ok(())
    }

    fn add_visitor_pre(&mut self, name: &Ident) -> syn::Result<()> {
        let lower_name = name.to_string().to_lowercase();
        let pre = Ident::new(&format!("{lower_name}_pre"), name.span());
        self.out_visitor_trait
            .extend(quote! {
                #[inline]
                fn #pre(&mut self, val: &mut #name) -> Result<bool> {
                    Ok(true)
                }
            });
        Ok(())
    }

    fn add_visitor_post(&mut self, name: &Ident) -> syn::Result<()> {
        let lower_name = name.to_string().to_lowercase();
        let post = Ident::new(&format!("{lower_name}_post"), name.span());
        self.out_visitor_trait
            .extend(quote! {
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
        let pre = Ident::new(&format!("{lower_name}_pre"), name.span());
        let recurse = Ident::new(&format!("{lower_name}_recurse"), name.span());
        let post = Ident::new(&format!("{lower_name}_post"), name.span());

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

// Return the lower-case version of an identifier.
fn lowercase(ident: &Ident) -> Ident {
    let name = ident.to_string().to_lowercase();
    Ident::new(&name, ident.span())
}

// Given a field, make a FnArg for it using `impl Into<f.ty>`.
fn fn_arg_impl_into(f: &Field) -> FnArg {
    FnArg::Typed(PatType {
        attrs: Vec::new(),
        pat: Box::new(Pat::Ident(PatIdent {
            attrs: Vec::new(),
            by_ref: None,
            mutability: None,
            ident: f.ident.clone().unwrap(),
            subpat: None,
        })),
        colon_token: Token![:](f.span()),
        ty: Box::new(impl_into(&f.ty)),
    })
}

// Turn `ty` into `impl Into<ty>`.
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
