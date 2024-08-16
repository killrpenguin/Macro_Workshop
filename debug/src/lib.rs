#![allow(unused_imports, unused_variables)]
#![allow(dead_code)]

extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::Span;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use std::fmt;
use syn::{
    parse_macro_input,
    visit_mut::{self, VisitMut},
    DeriveInput,
};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let token_stream = syn::parse_macro_input!(input as DeriveInput);
    //    println!("{:#?}", &token_stream);
    let derived_dt_ident = &token_stream.ident;
    let named_fields = match &token_stream.data {
        syn::Data::Struct(syn::DataStruct { fields, .. }) => {
            if let syn::Fields::Named(syn::FieldsNamed { named, .. }) = fields {
                named
            } else {
                return syn_error(fields, "Invalid field.")
                    .err()
                    .unwrap()
                    .to_compile_error()
                    .into();
            }
        }
        syn::Data::Enum(syn::DataEnum { variants, .. }) => unimplemented!(),
        syn::Data::Union(syn::DataUnion { fields, .. }) => unimplemented!(),
    };
    let mut impl_generics = manage_generics(derived_dt_ident, token_stream.generics);
    for field in named_fields {
        if let syn::Type::Path(syn::TypePath { path, .. }) = &field.ty {
            if find_phantom_data_field(field) {
                //                println!("{:#?}", path.get_ident());
                impl_generics = None;
                break;
            }
        }
    }
    let debug = impl_debug(derived_dt_ident, named_fields, impl_generics);

    quote! {
        #debug
    }
    .into()
}

fn impl_debug(
    data_type_ident: &syn::Ident,
    named_fields: &syn::punctuated::Punctuated<syn::Field, syn::token::Comma>,
    generics: Option<TokenStream2>,
) -> TokenStream2 {
    let data_type_name = data_type_ident.to_string();

    let field_args = named_fields.iter().map(|field| {
        let field_ident = field.ident.as_ref().unwrap();
        let field_name = field_ident.to_string();
        let brac = debug_attr(field);
        if !field.attrs.is_empty() && brac.is_some() {
            return quote! { #field_name, &format_args!(#brac, self.#field_ident)  };
        } else {
            return quote! { #field_name, &self.#field_ident };
        }
    });
    let impl_line = if let Some(gens) = generics {
        gens
    } else {
        quote! { impl std::fmt::Debug for #data_type_ident }
    };
    quote! {
        use std::fmt;
        #impl_line {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
                f.debug_struct(#data_type_name)#(.field(#field_args))*.finish()
            }
        }
    }
}

fn manage_generics(
    data_type_ident: &syn::Ident,
    mut generics: syn::Generics,
) -> Option<TokenStream2> {
    if !generics.params.is_empty() {
        let mut mod_gens = ModifiyGenerics;
        mod_gens.visit_generics_mut(&mut generics);
        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();
        Some(quote! {
            impl #impl_generics std::fmt::Debug for #data_type_ident #ty_generics #where_clause
        })
    } else {
        None
    }
}

fn debug_attr(field: &syn::Field) -> Option<String> {
    if !field.attrs.is_empty() {
        field
            .attrs
            .iter()
            .map(|attr| {
                if !attr.meta.path().is_ident("debug") {
                    return None;
                }
                let meta_name_value = &attr.meta.require_name_value().ok().unwrap();
                if let syn::Expr::Lit(syn::ExprLit { lit, .. }) = &meta_name_value.value {
                    if let syn::Lit::Str(val) = &lit {
                        Some(val.value())
                    } else {
                        None
                    }
                } else {
                    None
                }
            })
            .collect()
    } else {
        return None;
    }
}

fn mk_gen_where_predicates(
    param_type: &syn::Ident,
) -> syn::punctuated::Punctuated<syn::WherePredicate, syn::token::Comma> {
    let mut predicates = syn::punctuated::Punctuated::new();

    let type_param_path = quote! { std::fmt::Debug }.into_traitbound_path();
    let type_param_bound = syn::TypeParamBound::Trait(syn::TraitBound {
        paren_token: None,
        modifier: syn::TraitBoundModifier::None,
        lifetimes: None,
        path: type_param_path,
    });

    let mut predicate_segments = syn::punctuated::Punctuated::new();
    predicate_segments.push_value(param_type.clone().into_path_seg());

    let mut bounds = syn::punctuated::Punctuated::new();
    bounds.push_value(type_param_bound);

    let where_predicate = syn::WherePredicate::Type(syn::PredicateType {
        lifetimes: None,
        bounded_ty: syn::Type::Path(syn::TypePath {
            qself: None,
            path: syn::Path {
                leading_colon: None,
                segments: predicate_segments,
            },
        }),
        colon_token: syn::token::Colon::default(),
        bounds,
    });
    predicates.push(where_predicate);
    predicates
}

fn find_phantom_data_field(field: &syn::Field) -> bool {
    false
}

#[derive(Default)]
struct ModifiyGenerics;

impl visit_mut::VisitMut for ModifiyGenerics {
    fn visit_generics_mut(&mut self, i: &mut syn::Generics) {
        let ident = i
            .params
            .iter()
            .filter_map(|param| {
                if let syn::GenericParam::Type(syn::TypeParam { ident, .. }) = param {
                    Some(ident)
                } else {
                    None
                }
            })
            .next();
        let predicates = mk_gen_where_predicates(ident.as_ref().unwrap());
        i.where_clause = Some(syn::WhereClause {
            where_token: syn::token::Where::default(),
            predicates,
        })
    }
}

trait IntoTraitBoundPath {
    fn into_traitbound_path(self) -> syn::Path;
}

impl IntoTraitBoundPath for TokenStream2 {
    fn into_traitbound_path(self) -> syn::Path {
        let segs: syn::punctuated::Punctuated<syn::PathSegment, syn::token::PathSep> =
            syn::punctuated::Punctuated::new();

        let mut segments = self.into_iter().fold(segs, |mut segs, item| {
            if item.to_string() != ":".to_string() {
                segs.push(syn::PathSegment {
                    ident: syn::Ident::new(&item.to_string(), Span::call_site()),
                    arguments: syn::PathArguments::None,
                });
                segs.push_punct(syn::token::PathSep::default());
            }
            return segs;
        });
        if segments.trailing_punct() {
            segments.pop_punct();
        }
        syn::Path {
            leading_colon: None,
            segments,
        }
    }
}

trait IntoPathSeg {
    fn into_path_seg(self) -> syn::PathSegment;
}

impl IntoPathSeg for syn::Ident {
    fn into_path_seg(self) -> syn::PathSegment {
        syn::PathSegment {
            ident: syn::Ident::new(&self.to_string(), self.span()),
            arguments: syn::PathArguments::None,
        }
    }
}

fn syn_error(
    tokens: impl ToTokens,
    msg: impl AsRef<str> + std::fmt::Display,
) -> syn::Result<syn::Error> {
    Err(syn::Error::new_spanned(tokens, msg))
}
