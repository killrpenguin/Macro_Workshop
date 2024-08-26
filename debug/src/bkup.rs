// syn::PathArguments::AngleBracketed is the common factor across test 5, 6 7 and 8.
// Todo!: Rewrite code around anglebrackets.

#![allow(unused_imports, unused_variables)]
#![allow(dead_code)]

extern crate proc_macro;

use ::std::fmt;
use proc_macro::TokenStream;
use proc_macro2::Span;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use std::fmt::Arguments;

use syn::AngleBracketedGenericArguments;
use syn::{
    visit_mut::{self, VisitMut},
    DeriveInput,
};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let mut token_stream = syn::parse_macro_input!(input as DeriveInput);
    //     println!("{:#?}", &token_stream);

    let derived_dt_ident = &token_stream.ident;
    let named_fields = match &token_stream.data {
        syn::Data::Struct(syn::DataStruct { fields, .. }) => {
            if let syn::Fields::Named(syn::FieldsNamed { named, .. }) = fields {
                named
            } else {
                return syn_error(fields, "Invalid field.")
                    .err()
                    .expect("Faild to unwrap in derive function.")
                    .to_compile_error()
                    .into();
            }
        }
        syn::Data::Enum(syn::DataEnum { variants, .. }) => unimplemented!(),
        syn::Data::Union(syn::DataUnion { fields, .. }) => unimplemented!(),
    };
    let debug = build_impl_debug(derived_dt_ident, named_fields, &mut token_stream.generics);
    quote! {
        #debug
    }
    .into()
}

fn build_impl_debug(
    data_type_ident: &syn::Ident,
    named_fields: &syn::punctuated::Punctuated<syn::Field, syn::token::Comma>,
    generic_data: &mut syn::Generics,
) -> TokenStream2 {
    let data_type_name = data_type_ident.to_string();
    let mut modify_generics = ModifiyGenerics;
    let phantom_ident = named_fields
        .iter()
        .find_map(|field| match is_phantom_field(field) {
            Some(val) => Some(val),
            None => None,
        });

    let trait_args = named_fields
        .iter()
        .find_map(|field| match is_trait_bound(field) {
            Some(val) => val.iter().find_map(|gen_arg| {
                if let syn::GenericArgument::Type(syn::Type::Path(syn::TypePath { path, .. })) =
                    &gen_arg
                {
                    println!("{:#?}", path);
                    let placeholder = &syn::Ident::new("apple", Span::call_site());
                    let where_predicate = build_where_predicates(placeholder, Some(&path.segments));
                    Some(where_predicate)
                } else {
                    None
                }
            }),
            None => None,
        });
    
    let impl_line = if !generic_data.params.is_empty() {
        if phantom_ident.is_none() {
            modify_generics.visit_generics_mut(generic_data);
        }

        let (impl_generics, ty_generics, where_clause) = generic_data.split_for_impl();
        quote! { impl #impl_generics std::fmt::Debug for #data_type_ident #ty_generics #where_clause }
    } else {
        quote! { impl std::fmt::Debug for #data_type_ident }
    };

    let field_args = named_fields.iter().map(|field| {
        let field_ident = field.ident.as_ref().expect("failed to unwrap field ident.");
        let field_name = field_ident.to_string();
        let brac = debug_attr_macro(field);
        if !field.attrs.is_empty() && brac.is_some() {
            return quote! { #field_name, &format_args!(#brac, self.#field_ident)  };
        } else {
            return quote! { #field_name, &self.#field_ident };
        }
    });

    quote! {
        #impl_line {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(#data_type_name)#(.field(#field_args))*.finish()
            }
        }
    }
}

fn debug_attr_macro(field: &syn::Field) -> Option<String> {
    if !field.attrs.is_empty() {
        field
            .attrs
            .iter()
            .map(|attr| {
                if !attr.meta.path().is_ident("debug") {
                    return None;
                }
                let meta_name_value = &attr
                    .meta
                    .require_name_value()
                    .ok()
                    .expect("This should never fail.");
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

fn is_phantom_field(field: &syn::Field) -> Option<syn::Ident> {
    if let syn::Type::Path(syn::TypePath { path, .. }) = &field.ty {
        for seg in path.segments.iter() {
            if seg.ident.to_string() == "PhantomData" {
                return Some(seg.ident.clone());
            } else {
                return None;
            }
        }
        return None;
    } else {
        None
    }
}

fn is_trait_bound(
    field: &syn::Field,
) -> Option<syn::punctuated::Punctuated<syn::GenericArgument, syn::token::Comma>> {
    if let syn::Type::Path(syn::TypePath { path, .. }) = &field.ty {
        if !path.segments.is_empty() {
            path.segments.iter().find_map(|seg| {
                if let syn::PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                    args,
                    ..
                }) = &seg.arguments
                {
                    Some(args.clone())
                } else {
                    None
                }
            })
        } else {
            None
        }
    } else {
        None
    }
}

fn build_where_predicates(
    gen_ident: &syn::Ident,
    path_segs: Option<&syn::punctuated::Punctuated<syn::PathSegment, syn::token::PathSep>>,
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
    if path_segs.is_some() {
        let segs = path_segs.unwrap();
        for seg in segs {
            predicate_segments.push_value(seg.clone());
            predicate_segments.push_punct(syn::token::PathSep::default());
        }
        if predicate_segments.trailing_punct() {
            predicate_segments.pop();
        }
    } else {
        predicate_segments.push_value(gen_ident.clone().into_path_seg());
    }
    
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
    predicates.push_value(where_predicate);
    predicates
}

#[derive(Default)]
struct ModifiyGenerics;

impl visit_mut::VisitMut for ModifiyGenerics {
    fn visit_generics_mut(&mut self, i: &mut syn::Generics) {
        let ident = i
            .params
            .iter()
            .filter_map(|param| {
                if let syn::GenericParam::Type(syn::TypeParam { ident, .. }) = &param {
                    Some(ident)
                } else {
                    None
                }
            })
            .next();

        let predicates = build_where_predicates(
            ident.as_ref().expect("Failed to unwrap where predicates."),
            None,
        );
        i.where_clause = Some(syn::WhereClause {
            where_token: syn::token::Where::default(),
            predicates,
        });
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
