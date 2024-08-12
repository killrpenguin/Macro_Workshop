//#![allow(unused_imports, unused_variables)]
//#![allow(dead_code)]

extern crate proc_macro;
use proc_macro::TokenStream;
use proc_macro2::Span;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use std::fmt;
use syn::{
    parse_macro_input,
    visit_mut::{self, VisitMut},
};

#[proc_macro_attribute]
pub fn sorted(_attrs: TokenStream, item: TokenStream) -> TokenStream {
    let mut out = item.clone();
    let mut stream: syn::Item = parse_macro_input!(item as syn::Item);
    let data = LexicographicMatch;
    match item_strategy(data, &mut stream) {
        Ok(val) => val.into_token_stream().into(),
        Err(err) => {
            out.extend(TokenStream::from(err.to_compile_error()));
            out
        }
    }
}

#[proc_macro_attribute]
pub fn check(_attrs: TokenStream, item: TokenStream) -> TokenStream {
    let mut stream: syn::Item = parse_macro_input!(item as syn::Item);
    let data = LexicographicMatch;
    match item_strategy(data, &mut stream) {
        Ok(val) => val.into_token_stream().into(),
        Err(err) => TokenStream::from(err.to_compile_error()),
    }
}

#[derive(Debug, Default)]
struct LexicographicMatch;

fn item_strategy(
    mut data: impl VisitMut + fmt::Debug,
    stream: &mut syn::Item,
) -> syn::Result<TokenStream2> {
    match stream {
        syn::Item::Enum(val) => {
            let pathes: Vec<syn::Path> = collect_pathes(&val.variants)?;
            match lexicographic_sort(&pathes, pathes.len()) {
                Ok(()) => Ok(stream.into_token_stream()),
                Err(err) => Err(err),
            }
        }
        syn::Item::Fn(val) => {
            data.visit_item_fn_mut(val);
            let expr_match = val
                .block
                .stmts
                .iter()
                .find_map(is_match_expr)
                .expect("Failed to find match expression in item strat.");
            let pathes = collect_pathes(expr_match.arms)?;
            if let Some(error) = lexicographic_sort(&pathes, pathes.len()).err() {
                let comp_error = error.to_compile_error();
                Ok(quote! { #val #comp_error })
            } else {
                Ok(quote! { #val })
            }
        }
        _ => Err(syn::Error::new(
            Span::call_site(),
            "expected enum or match expression",
        )),
    }
}

fn lexicographic_sort(pathes: &Vec<syn::Path>, len: usize) -> syn::Result<()> {
    let mut sorted_list: Vec<String> = Vec::with_capacity(len);
    for path in pathes {
        if !path.segments.is_empty() {
            let name = full_path_name(path);
            if sorted_list
                .last()
                .map(|last_item| &name < last_item)
                .unwrap_or(false)
            {
                let should_be = sorted_list
                    .binary_search(&name)
                    .expect_err("Failed to find binary search value in lexico sort.");

                return Err(syn::Error::new_spanned(
                    path,
                    format!("{} should sort before {}", name, sorted_list[should_be]),
                ));
            }
            sorted_list.push(name);
        }
    }
    Ok(())
}

impl visit_mut::VisitMut for LexicographicMatch {
    fn visit_expr_match_mut(&mut self, node: &mut syn::ExprMatch) {
        if node.attrs.iter().any(|expr| expr.path().is_ident("sorted")) {
            node.attrs.clear();
        }
        visit_mut::visit_expr_match_mut(self, node);
    }
    fn visit_item_fn_mut(&mut self, i: &mut syn::ItemFn) {
        visit_mut::visit_item_fn_mut(self, i);
    }
}

trait IntoPath {
    fn into_path(self) -> syn::Result<syn::Path>;
}

impl IntoPath for syn::Ident {
    fn into_path(self) -> syn::Result<syn::Path> {
        let mut segments = syn::punctuated::Punctuated::new();
        segments.push_value(self.into());
        Ok(syn::Path {
            leading_colon: None,
            segments,
        })
    }
}

impl IntoPath for &syn::Variant {
    fn into_path(self) -> syn::Result<syn::Path> {
        let mut segments = syn::punctuated::Punctuated::new();
        segments.push_value(self.ident.clone().into());
        Ok(syn::Path {
            leading_colon: None,
            segments,
        })
    }
}

impl IntoPath for syn::Arm {
    fn into_path(self) -> syn::Result<syn::Path> {
        match self.pat {
            syn::Pat::Path(pat_path) => Ok(pat_path.path),
            syn::Pat::TupleStruct(tpl_pat) => Ok(tpl_pat.path),
            syn::Pat::Struct(pat_struct) => Ok(pat_struct.path),
            syn::Pat::Wild(syn::PatWild { attrs, .. }) => {
                if attrs.is_empty() {
                    Ok(syn::Path {
                        leading_colon: None,
                        segments: syn::punctuated::Punctuated::new(),
                    })
                } else {
                    Ok(attrs[0].meta.path().clone())
                }
            }
            syn::Pat::Ident(syn::PatIdent {
                ident,
                subpat: None,
                ..
            }) => Ok(ident.into_path().expect("Couldn't convert into_path().")),
            other => Err(syn::Error::new_spanned(other, "unsupported by #[sorted]")),
        }
    }
}

fn collect_pathes<I>(iter: I) -> syn::Result<Vec<syn::Path>>
where
    I: IntoIterator,
    I::Item: IntoPath,
{
    iter.into_iter().map(IntoPath::into_path).collect()
}

fn is_match_expr(expr: &syn::Stmt) -> Option<syn::ExprMatch> {
    if let syn::Stmt::Expr(syn::Expr::Match(expr_match), _) = expr {
        Some(expr_match.clone())
    } else {
        None
    }
}

fn full_path_name(path: &syn::Path) -> String {
    if path.segments.len() == 1 {
        return path.segments[0].ident.to_string();
    }
    path.segments
        .iter()
        .map(|seg| format!("{}", quote! { #seg }))
        .collect::<Vec<_>>()
        .join("::")
}
