#![recursion_limit = "128"]
#![allow(unused_variables)]

// https://www.youtube.com/watch?v=geovSK3wMB8

// @ 2:03:49

extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(input as DeriveInput);
    let name = &ast.ident;
    let new_name = format!("{}Builder", name);
    let new_ident = syn::Ident::new(&new_name, name.span());
    let fields = if let syn::Data::Struct(syn::DataStruct {
        fields: syn::Fields::Named(syn::FieldsNamed { ref named, .. }),
        ..
    }) = ast.data
    {
        named
    } else {
        unimplemented!()
    };

    let optionized = fields.iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;
        if ty_inner_type("Option", &ty).is_some() {
            quote! { #name: #ty }
        } else {
            quote! { #name: std::option::Option<#ty> }
        }
    });

    let methods = fields.iter().map(|field| {
        let name = field.ident.as_ref().unwrap();
        let ty = &field.ty;
        if let Some(inner_ty) = ty_inner_type("Option", ty) {
            quote! {
                pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        } else {
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = Some(#name);
                    self
                }
            }
        }
    });

    let extended_methods = fields.iter().filter_map(|field| {
        let name = &field.ident;
        for attr in &field.attrs {
            if attr.path().is_ident("builder") {
                let attr_expr: syn::Expr = attr.parse_args().expect("Failed to find syn::Expr.");
                let expr_path = if let syn::Expr::Assign(ref expr_assign) = attr_expr {
                    if let syn::Expr::Path(ref path) = *expr_assign.left {
                        path
                    } else {
                        panic!("Failed to find inner left hand assignment.");
                    }
                } else {
                    panic!("Failed to find left hand assignment.");
                };

                assert!(expr_path.path.is_ident("each"));
                let lit_token = if let syn::Expr::Assign(ref expr_assign) = &attr_expr {
                    if let syn::Expr::Lit(ref expr) = *expr_assign.right {
                        if let syn::Lit::Str(token) = expr.lit.clone() {
                            let method_ident = syn::Ident::new(&token.value(), token.span());
                            let inner_ty = ty_inner_type("Vec", &field.ty).expect("failed to find type");
                            return Some(quote! {
                                pub fn #method_ident(&mut self, #method_ident: #inner_ty) -> &mut Self {
                                    if let Some(ref mut values) = self.#name {
                                        values.push(#method_ident);
                                    } else {
                                        self.#name = Some(vec![#method_ident]);
                                    }
                                    self
                                }
                            });
                        } else {
                            panic!("Failed to find syn::LitStr.");
                        }
                    } else {
                        panic!("Failed to find syn::Expr::Lit.");
                    }
                } else {
                    panic!("Failed to find syn::Expr::Assign.");
                };
            }
        }
        None
    });

    let build_fields = fields.iter().map(|field| {
        let name = &field.ident;
        if ty_inner_type("Option", &field.ty).is_some() {
            quote! {
                #name: self.#name.clone()
            }
        } else {
            quote! {
                #name: self.#name.clone().ok_or(concat!(stringify!(#name), " is not set"))?
            }
        }
    });

    let build_empty = fields.iter().map(|field| {
        let name = &field.ident;
        quote! { #name: None }
    });

    let expanded = quote! {
        pub struct #new_ident {
            #(#optionized,)*
        }
        impl #new_ident {
            #(#methods)*
            #(#extended_methods)*
            pub fn build(&self) -> Result<#name, Box<dyn std::error::Error>> {
                std::result::Result::Ok(#name {
                    #(#build_fields,)*
                })
            }
        }
        impl #name {
            fn builder() -> #new_ident {
                #new_ident {
                    #(#build_empty,)*
                }
            }
        }
    };

    expanded.into()
}

fn ty_inner_type<'a>(eval_str: &str, ty: &'a syn::Type) -> Option<&'a syn::Type> {
    if let syn::Type::Path(ref p) = ty {
        if p.path.segments.len() != 1 || p.path.segments[0].ident != eval_str {
            return None;
        }

        if let syn::PathArguments::AngleBracketed(ref inner_ty) = p.path.segments[0].arguments {
            if inner_ty.args.len() != 1 {
                return None;
            }

            let inner_ty = inner_ty.args.first().unwrap();
            if let syn::GenericArgument::Type(ref stripped_type) = inner_ty {
                return Some(stripped_type);
            }
        }
    }
    None
}
