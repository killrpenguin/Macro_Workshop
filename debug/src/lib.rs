#![allow(unused_imports, unused_variables)]
#![allow(dead_code)]

use ::std::fmt;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use syn::{visit_mut::VisitMut, AngleBracketedGenericArguments, Generics};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let mut token_stream = syn::parse_macro_input!(input as syn::DeriveInput);
    //    println!("{:#?}", &token_stream);

    let derived_ident = &token_stream.ident;
    if !token_stream.attrs.is_empty() {
        debug_bound(&token_stream.attrs);
    }
    let named_fields = match &token_stream.data {
        syn::Data::Struct(syn::DataStruct { fields, .. }) => {
            if let syn::Fields::Named(syn::FieldsNamed { named, .. }) = fields {
                named
            } else {
                return syn_error(fields, "Invalid field for derive macro.")
                    .err()
                    .unwrap()
                    .to_compile_error()
                    .into();
            }
        }
        _ => unimplemented!(),
    };

    let derived_generic = token_stream.generics.params.iter().find_map(|param| {
        if let syn::GenericParam::Type(syn::TypeParam { ident, .. }) = param {
            Some(ident)
        } else {
            None
        }
    });
    let impl_line = if let Some(generic_ident) = derived_generic {
        let generic_field = named_fields
            .iter()
            .find_map(|field| {
                if field.ty.compare_idents(generic_ident) {
                    Some(field)
                } else {
                    None
                }
            })
            .expect("This should never fail. Generic fields are searched recursively.");
        let predicate = generic_field
            .ty
            .into_where_predicate(&derived_generic.unwrap());
        if !generic_field.ty.is_named_ident("PhantomData") {
            token_stream.generics.make_where_with_predicates(predicate);
        }
        let (impl_generics, ty_generics, where_clause) = token_stream.generics.split_for_impl();
        quote::quote! { impl #impl_generics std::fmt::Debug for #derived_ident #ty_generics #where_clause }
    } else {
        quote::quote! { impl std::fmt::Debug for #derived_ident }
    };

    let debug_impl = build_debug_function(derived_ident, named_fields);

    quote::quote! {
        #impl_line
        #debug_impl
    }
    .into()
}

fn build_debug_function(
    derived_ident: &syn::Ident,
    named_fields: &syn::punctuated::Punctuated<syn::Field, syn::token::Comma>,
) -> TokenStream2 {
    let derived_name = derived_ident.to_string();

    let field_args = named_fields.iter().map(|field| {
        let field_ident = field.ident.as_ref().expect("failed to unwrap field ident.");
        let field_name = field_ident.to_string();
        let brac = debug_innert_attr(field);
        if !field.attrs.is_empty() && brac.is_some() {
            return quote::quote! { #field_name, &format_args!(#brac, self.#field_ident)  };
        } else {
            return quote::quote! { #field_name, &self.#field_ident };
        }
    });

    quote::quote! {
        {fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(#derived_name)#(.field(#field_args))*.finish()
            }
        }
    }
}

fn debug_innert_attr(field: &syn::Field) -> Option<String> {
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
                    .expect("This should never fail.");
                if let syn::Expr::Lit(syn::ExprLit {
                    lit: syn::Lit::Str(val),
                    ..
                }) = &meta_name_value.value
                {
                    Some(val.value())
                } else {
                    None
                }
            })
            .collect()
    } else {
        None
    }
}

fn debug_bound(attrs: &[syn::Attribute]) -> std::string::String {
    let bound_arg = attrs
        .iter()
        .find_map(|attr| {
            if attr.path().is_ident("debug") {
                if let Ok(val) = attr.parse_args::<TokenStream2>() {
                    Some(val)
                } else {
                    None
                }
            } else {
                None
            }
        })
        .unwrap();
    let r_val = bound_arg
        .into_iter()
        .find_map(|token| {
            if let proc_macro2::TokenTree::Literal(lit) = token {
                if let syn::Lit::Str(val) = syn::Lit::new(lit) {
                    Some(val.value())
                } else {
                    None
                }
            } else {
                None
            }
        })
        .expect("Expected proc_macro2::TokenTree::Literal. Found None.");

    r_val
        .as_str()
        .strip_suffix(": Debug")
        .expect("Strip suffic failed in debug_bound().")
        .to_string()
}

trait IntoPath {
    fn into_path(self) -> syn::Path;
}

impl IntoPath for std::string::String {
    fn into_path(self) -> syn::Path {
        let str_segments = self.rsplit("::");
        let mut segments = syn::punctuated::Punctuated::new();
        for seg in str_segments {
            segments.push_value(seg.to_string().into_path_seg());
            segments.push_punct(syn::token::PathSep::default());
        }
        if segments.trailing_punct() {
            segments.pop();
        }
        syn::Path {
            leading_colon: None,
            segments,
        }
    }
}

impl IntoPath for TokenStream2 {
    fn into_path(self) -> syn::Path {
        let segs: syn::punctuated::Punctuated<syn::PathSegment, syn::token::PathSep> =
            syn::punctuated::Punctuated::new();

        let mut segments = self.into_iter().fold(segs, |mut segs, item| {
            if item.to_string() != ":".to_string() {
                segs.push(syn::PathSegment {
                    ident: syn::Ident::new(&item.to_string(), proc_macro2::Span::call_site()),
                    arguments: syn::PathArguments::None,
                });
                segs.push_punct(syn::token::PathSep::default());
            }
            segs
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

impl IntoPathSeg for std::string::String {
    fn into_path_seg(self) -> syn::PathSegment {
        syn::PathSegment {
            ident: syn::Ident::new(&self, proc_macro2::Span::call_site()),
            arguments: syn::PathArguments::None,
        }
    }
}

impl IntoPathSeg for syn::Ident {
    fn into_path_seg(self) -> syn::PathSegment {
        syn::PathSegment {
            ident: syn::Ident::new(&self.to_string(), self.span()),
            arguments: syn::PathArguments::None,
        }
    }
}

trait GetPathes {
    fn get_type_path(&self) -> Option<&syn::TypePath>;
    fn get_path(&self) -> Option<&syn::Path>;
}

impl GetPathes for syn::AngleBracketedGenericArguments {
    fn get_path(&self) -> Option<&syn::Path> {
        return self.args.iter().find_map(|arg| {
            if let syn::GenericArgument::Type(syn::Type::Path(type_path)) = arg {
                Some(&type_path.path)
            } else {
                None
            }
        });
    }
    fn get_type_path(&self) -> Option<&syn::TypePath> {
        return self.args.iter().find_map(|arg| {
            if let syn::GenericArgument::Type(syn::Type::Path(path)) = arg {
                Some(path)
            } else {
                None
            }
        });
    }
}

trait RecursePathSeg {
    fn get_gen_arg_path(&self) -> &syn::Path;
    fn find_inner(&self) -> &syn::PathSegment;
}

impl RecursePathSeg for syn::PathSegment {
    fn find_inner(&self) -> &syn::PathSegment {
        match self.arguments {
            syn::PathArguments::AngleBracketed(ref bracks) => {
                let path = bracks.get_path().unwrap();
                let new_seg = path.segments.iter().find_map(|seg| match seg.arguments {
                    syn::PathArguments::AngleBracketed(_) => Some(seg),
                    _ => None,
                });
                if let Some(segment) = new_seg {
                    segment.find_inner()
                } else {
                    self
                }
            }
            _ => self,
        }
    }
    fn get_gen_arg_path(&self) -> &syn::Path {
        if let syn::PathArguments::AngleBracketed(AngleBracketedGenericArguments { args, .. }) =
            &self.arguments
        {
            args.iter()
                .find_map(|arg| {
                    if let syn::GenericArgument::Type(syn::Type::Path(syn::TypePath {
                        path, ..
                    })) = arg
                    {
                        Some(path)
                    } else {
                        None
                    }
                })
                .expect("Failed to unwrap path in get_gen_arg_path().")
        } else {
            panic!()
        }
    }
}

trait CompareIdents {
    fn compare_idents(&self, ident: &syn::Ident) -> bool;
}

impl CompareIdents for syn::Path {
    fn compare_idents(&self, ident: &syn::Ident) -> bool {
        self.segments.iter().any(|seg| seg.ident == *ident)
    }
}

impl CompareIdents for syn::Type {
    fn compare_idents(&self, ident: &syn::Ident) -> bool {
        if let syn::Type::Path(syn::TypePath { path, .. }) = self {
            if path.is_ident(ident) {
                true
            } else {
                path.bracketed_generic(ident)
            }
        } else {
            false
        }
    }
}

trait PathHelpers {
    fn bracketed_generic(&self, ident: &syn::Ident) -> bool;
    fn is_bracketed(&self) -> bool;
}

impl PathHelpers for syn::Path {
    fn bracketed_generic(&self, ident: &syn::Ident) -> bool {
        self.segments.iter().any(|seg| match &seg.arguments {
            syn::PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                ref args, ..
            }) => args.iter().any(|arg| {
                let new_seg = seg.find_inner();
                let path = new_seg.get_gen_arg_path();
                path.compare_idents(ident)
            }),
            _ => false,
        })
    }
    fn is_bracketed(&self) -> bool {
        self.segments
            .iter()
            .all(|seg| !matches!(seg.arguments, syn::PathArguments::None))
    }
}

trait GenericsTypeFieldHelpers {
    fn is_named_ident(&self, name: &str) -> bool;
    fn into_where_predicate(&self, generic_ident: &syn::Ident) -> syn::WherePredicate;
    fn get_inner_path(&self) -> Option<&syn::TypePath>;
}

impl GenericsTypeFieldHelpers for syn::Type {
    fn is_named_ident(&self, name: &str) -> bool {
        if let syn::Type::Path(syn::TypePath { path, .. }) = self {
            path.segments
                .iter()
                .any(|seg| seg.ident.to_string() == name)
        } else {
            false
        }
    }

    fn into_where_predicate(&self, generic_ident: &syn::Ident) -> syn::WherePredicate {
        let mut bounds = syn::punctuated::Punctuated::new();
        let r_val_path = quote::quote! { std::fmt::Debug }.into_path();
        let type_param_bound = syn::TypeParamBound::Trait(syn::TraitBound {
            paren_token: None,
            modifier: syn::TraitBoundModifier::None,
            lifetimes: None,
            path: r_val_path,
        });

        bounds.push_value(type_param_bound);

        syn::WherePredicate::Type(syn::PredicateType {
            lifetimes: None,
            bounded_ty: syn::Type::Path(self.get_inner_path().unwrap().clone()),
            colon_token: syn::token::Colon::default(),
            bounds,
        })
    }

    fn get_inner_path(&self) -> Option<&syn::TypePath> {
        if let syn::Type::Path(type_path) = self {
            if type_path.path.is_bracketed() {
                type_path.path.segments.iter().find_map(|seg| {
                    if let syn::PathArguments::AngleBracketed(bracketed) = &seg.arguments {
                        bracketed.get_type_path()
                    } else {
                        None
                    }
                })
            } else {
                Some(type_path)
            }
        } else {
            None
        }
    }
}

trait ExtendSynGeneric {
    fn make_where_with_predicates(&mut self, predicate: syn::WherePredicate);
}

impl ExtendSynGeneric for syn::Generics {
    fn make_where_with_predicates(&mut self, predicate: syn::WherePredicate) {
        let mut predicates = syn::punctuated::Punctuated::new();
        predicates.push_value(predicate);
        self.where_clause = Some(syn::WhereClause {
            where_token: syn::token::Where::default(),
            predicates,
        })
    }
}

fn syn_error(
    tokens: impl quote::ToTokens,
    msg: impl AsRef<str> + std::fmt::Display,
) -> syn::Result<syn::Error> {
    Err(syn::Error::new_spanned(tokens, msg))
}
