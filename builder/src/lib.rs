#![recursion_limit = "128"]
#![allow(unused_variables)]

extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::spanned::Spanned;
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
        if ty_inner_type("Option", &ty).is_some() || builder_of(&field) {
            quote! { #name: #ty }
        } else {
            quote! { #name: std::option::Option<#ty> }
        }
    });

    let methods = fields.iter().map(|field| {
        let name = field
            .ident
            .as_ref()
            .expect("Failed to find ident in field.");
        let ty = &field.ty;
        let set_method = if let std::option::Option::Some(inner_ty) = ty_inner_type("Option", ty) {
            quote! {
                pub fn #name(&mut self, #name: #inner_ty) -> &mut Self {
                    self.#name = std::option::Option::Some(#name);
                    self
                }
            }
        } else if builder_of(&field) {
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = #name;
                    self
                }
            }
        } else {
            quote! {
                pub fn #name(&mut self, #name: #ty) -> &mut Self {
                    self.#name = std::option::Option::Some(#name);
                    self
                }
            }
        };
        match extended_method(&field) {
            std::option::Option::None => set_method,
            std::option::Option::Some((true, extend_method)) => extend_method,
            std::option::Option::Some((false, extend_method)) => quote! {
                #set_method
                #extend_method
            },
        }
    });

    let build_fields = fields.iter().map(|field| {
        let name = &field.ident;
        if ty_inner_type("Option", &field.ty).is_some() || builder_of(&field) {
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
        if builder_of(&field) {
            quote! { #name: Vec::new() }
        } else {
            quote! { #name: std::option::Option::None }
        }
    });

    let expanded = quote! {
        pub struct #new_ident {
            #(#optionized,)*
        }
        impl #new_ident {
            #(#methods)*
            pub fn build(&self) -> std::result::Result<#name, std::boxed::Box<dyn std::error::Error>> {
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

fn ty_inner_type<'a>(eval_str: &str, ty: &'a syn::Type) -> std::option::Option<&'a syn::Type> {
    if let syn::Type::Path(ref p) = ty {
        if p.path.segments.len() != 1 || p.path.segments[0].ident != eval_str {
            return std::option::Option::None;
        }

        if let syn::PathArguments::AngleBracketed(ref inner_ty) = p.path.segments[0].arguments {
            if inner_ty.args.len() != 1 {
                return std::option::Option::None;
            }

            let inner_ty = inner_ty.args.first().unwrap();
            if let syn::GenericArgument::Type(ref stripped_type) = inner_ty {
                return std::option::Option::Some(stripped_type);
            }
        }
    }
    std::option::Option::None
}

fn builder_of(field: &syn::Field) -> bool {
    for attr in &field.attrs {
        return attr.path().is_ident("builder");
    }
    false
}

fn extended_method(field: &syn::Field) -> std::option::Option<(bool, TokenStream2)> {
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
            if !&expr_path.path.is_ident("each") {
                return std::option::Option::Some((
                    false,
                    syn::Error::new(attr.meta.span(), "expected `builder(each = \"...\")`")
                        .to_compile_error(),
                ));
            }

            if let syn::Expr::Assign(ref expr_assign) = &attr_expr {
                if let syn::Expr::Lit(ref expr) = *expr_assign.right {
                    if let syn::Lit::Str(token) = expr.lit.clone() {
                        let args = syn::Ident::new(&token.value(), token.span());
                        let inner_ty =
                            ty_inner_type("Vec", &field.ty).expect("failed to find type");
                        if args == field.ident.clone().unwrap() {}
                        return std::option::Option::Some((
                            args == field.ident.clone().unwrap(),
                            quote! {
                                pub fn #args(&mut self, #args: #inner_ty) -> &mut Self {
                                    self.#name.push(#args);
                                    self
                            }},
                        ));
                    } else {
                        panic!("Failed to find syn::LitStr.");
                    }
                } else {
                    panic!("Failed to find syn::Expr::Lit.");
                }
            } else {
                panic!("Failed to find syn::Expr::Assign.");
            }
        }
    }
    return std::option::Option::None;
}
