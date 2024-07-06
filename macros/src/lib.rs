extern crate proc_macro;
use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    spanned::Spanned,
    token::Comma,
    AngleBracketedGenericArguments, Error, Expr, Field, FieldValue, Fields, FnArg,
    GenericArgument, Ident, Item, ItemEnum, ItemStruct, Pat, PatIdent, PatType,
    Path, PathArguments, PathSegment, Token, TraitBound, TraitBoundModifier, Type, TypeImplTrait,
    TypeParamBound,
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
    let items = syn::parse_macro_input!(input as Items);
    let mut result = proc_macro2::TokenStream::new();
    for item in items.0 {
        result.extend(proc_macro2::TokenStream::from(
            ast_item(item).unwrap_or_else(Error::into_compile_error),
        ));
    }
    proc_macro::TokenStream::from(result)
}

fn ast_item(item: Item) -> syn::Result<proc_macro2::TokenStream> {
    match item {
        Item::Struct(item) => struct_item(item),
        Item::Enum(item) => enum_item(item),
        _ => Ok(proc_macro2::TokenStream::from(quote!(#item))),
    }
}

fn struct_item(item: ItemStruct) -> syn::Result<proc_macro2::TokenStream> {
    let mut result = proc_macro2::TokenStream::new();

    result.extend(proc_macro2::TokenStream::from(quote! {
        #[derive(Debug, PartialEq, Eq)]
        #item
    }));

    // Create a constructor.
    let struct_name = &item.ident;
    let vis = &item.vis;
    let mut methods = proc_macro2::TokenStream::new();
    let Fields::Named(fields) = item.fields else {
        panic!("expected named fields");
    };
    let args: Punctuated<FnArg, Comma> =
        fields.named.iter().map(|f| fn_arg_impl_into(&f)).collect();
    let long_init: Punctuated<FieldValue, Comma> = fields
        .named
        .iter()
        .map(|f| {
            let name = f.ident.as_ref().map(|i| i.to_string()).unwrap();
            syn::parse_str::<FieldValue>(&format!("{name}: {name}.into()")).unwrap()
        })
        .collect();
    methods.extend(proc_macro2::TokenStream::from(quote! {
        #vis fn new(src: Src, #args) -> Node<#struct_name> {
            Node {
                src,
                inner: #struct_name { #long_init },
            }
        }
    }));

    result.extend(proc_macro2::TokenStream::from(quote! {
        #[allow(dead_code)]
        impl #struct_name {
            #methods
        }
    }));
    Ok(result)
}

fn enum_item(item: ItemEnum) -> syn::Result<proc_macro2::TokenStream> {
    let mut result = proc_macro2::TokenStream::new();

    result.extend(proc_macro2::TokenStream::from(quote! {
        #[derive(Debug, PartialEq, Eq)]
        #item
    }));

    // Create a constructor for each variant, returning `Node<K>`.
    let enum_name = &item.ident;
    let vis = &item.vis;
    let mut methods = proc_macro2::TokenStream::new();
    for variant in item.variants {
        let var_name = &variant.ident;
        let constr_name = lowercase(&var_name);

        match &variant.fields {
            Fields::Named(fields) => {
                let args: Punctuated<FnArg, Comma> =
                    fields.named.iter().map(|f| fn_arg_impl_into(&f)).collect();
                let long_init: Punctuated<FieldValue, Comma> = fields
                    .named
                    .iter()
                    .map(|f| {
                        let name = f.ident.as_ref().map(|i| i.to_string()).unwrap();
                        syn::parse_str::<FieldValue>(&format!("{name}: {name}.into()")).unwrap()
                    })
                    .collect();
                methods.extend(proc_macro2::TokenStream::from(quote! {
                    #vis fn #constr_name(src: Src, #args) -> Node<#enum_name> {
                        Node {
                            src,
                            inner: #enum_name::#var_name { #long_init },
                        }
                    }
                }));
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
                methods.extend(proc_macro2::TokenStream::from(quote! {
                    #vis fn #constr_name(src: Src, #args) -> Node<#enum_name> {
                        Node {
                            src,
                            inner: #enum_name::#var_name(#fields),
                        }
                    }
                }));
            }
            Fields::Unit => {
                methods.extend(proc_macro2::TokenStream::from(quote! {
                    #vis fn #constr_name(src: Src) -> Node<#enum_name> {
                        Node {
                            src,
                            inner: #enum_name::#var_name,
                        }
                    }
                }));
            }
        };
    }
    result.extend(proc_macro2::TokenStream::from(quote! {
        #[allow(dead_code)]
        impl #enum_name {
            #methods
        }
    }));
    Ok(result)
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
