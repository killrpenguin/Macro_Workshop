#![allow(unused_imports, unused_variables)]
#![allow(dead_code)]

extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use std::fmt;
use syn::{parse_macro_input, token::Comma, DeriveInput};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let token_stream = syn::parse_macro_input!(input as DeriveInput);
    let derived_dt_ident = token_stream.ident;
    let named_fields = match &token_stream.data {
        syn::Data::Struct(syn::DataStruct { fields, .. }) => {
            if let syn::Fields::Named(syn::FieldsNamed { named, .. }) = fields {
                named
            } else {
                return syn_error(fields, "Invalid field.").err().unwrap().to_compile_error().into()
            }
        }
        syn::Data::Enum(syn::DataEnum { variants, .. }) => unimplemented!(),
        syn::Data::Union(syn::DataUnion { fields, .. }) => unimplemented!(),
    };
    let debug = match impl_fmt_debug(derived_dt_ident, named_fields) {
        Ok(debug_stream) => debug_stream,
        Err(err) => return err.into_compile_error().into(),
    };
    let output = quote! {
       #debug
    };
    output.into()
}

fn impl_fmt_debug(data_type_ident: syn::Ident, named_fields: &syn::punctuated::Punctuated<syn::Field, Comma>) -> syn::Result<TokenStream2> {
    let data_type_name = data_type_ident.to_string();
    let field_ident = named_fields.iter().map(|val| {
        let name = val.ident.as_ref().unwrap();
        name
    });
    let field_name = named_fields.iter().map(|val| {
        let name = val.ident.as_ref().unwrap().to_string();
        name
    });
    Ok(quote! {
        use std::fmt;
        impl std::fmt::Debug for #data_type_ident {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
                f.debug_struct(#data_type_name)#(.field(#field_name, &self.#field_ident))*.finish()
            }
        }
    })
}

// fn impl_fmt_debug(stream: &syn::DeriveInput) -> syn::Result<TokenStream2> {
//     let data_type_ident = &stream.ident;
//     let data_type_name = &stream.ident.to_string();
//     let named_fields = match &stream.data {
//         syn::Data::Struct(syn::DataStruct { fields, .. }) => {
//             if let syn::Fields::Named(syn::FieldsNamed { named, .. }) = fields {
//                 named
//             } else {
//                 return Err(syn_error(fields, "Invalid field.").err().unwrap());
//             }
//         }
//         syn::Data::Enum(syn::DataEnum { variants, .. }) => unimplemented!(),
//         syn::Data::Union(syn::DataUnion { fields, .. }) => unimplemented!(),
//     };
//     let field_ident = named_fields.iter().map(|val| {
//         let name = val.ident.as_ref().unwrap();
//         name
//     });
//     let field_name = named_fields.iter().map(|val| {
//         let name = val.ident.as_ref().unwrap().to_string();
//         name
//     });
//     Ok(quote! {
//         use std::fmt;
//         impl std::fmt::Debug for #data_type_ident {
//             fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
//                 f.debug_struct(#data_type_name)#(.field(#field_name, &self.#field_ident))*.finish()
//             }
//         }
//     })
// }

fn syn_error(tokens: impl ToTokens, msg: impl std::fmt::Display) -> syn::Result<()> {
    Err(syn::Error::new_spanned(tokens, msg))
}
