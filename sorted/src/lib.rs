#![allow(unused_imports, unused_variables)]
#![allow(dead_code)]

extern crate proc_macro;
use proc_macro::TokenStream;
use proc_macro2::Span;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use std::fmt;
use syn::fold::fold_expr_match;
use syn::{
    parse_macro_input, parse_quote,
    visit_mut::{self, VisitMut},
};

#[proc_macro_attribute]
pub fn sorted(_attrs: TokenStream, item: TokenStream) -> TokenStream {
    let mut out = item.clone();
    let mut stream: syn::Item = parse_macro_input!(item as syn::Item);
    let data = LexicographicMatch::default();
    match item_strategy::<TokenStream2>(data, &mut stream) {
        Ok(val) => val.into_token_stream().into(),
        Err(err) => {
            out.extend(TokenStream::from(err.to_compile_error()));
            out
        }
    }
}

#[proc_macro_attribute]
pub fn check(_attrs: TokenStream, item: TokenStream) -> TokenStream {
    let mut out = item.clone();
    let mut stream: syn::Item = parse_macro_input!(item as syn::Item);
    let data = LexicographicMatch::default();
    match item_strategy::<TokenStream2>(data, &mut stream) {
        Ok(val) => return val.into_token_stream().into(),
        Err(err) => {
            out.extend(TokenStream::from(err.to_compile_error()));
            return out;
        }
    };
}

#[derive(Debug, Default)]
struct LexicographicMatch;

fn item_strategy<R>(
    mut data: impl VisitMut + fmt::Debug,
    stream: &mut syn::Item,
) -> syn::Result<TokenStream2> {
    match stream {
        syn::Item::Enum(val) => {
            let pathes: Vec<syn::Path> = collect_pathes(val.variants.clone())?;
            match lexicographic_sort(&pathes, pathes.len()) {
                Ok(()) => return Ok(stream.into_token_stream()),
                Err(err) => return Err(err),
            };
        }
        syn::Item::Fn(val) => {
            let expr_match = &val
                .block
                .stmts
                .iter()
                .find_map(|stmt| is_match_expr(&stmt))
                .expect("Failed to find match expression in item strat.");
            let pathes = collect_pathes(expr_match.arms.clone())?;
            data.visit_item_fn_mut(val);
            let sort_error = lexicographic_sort(&pathes, pathes.len()).err();
            if let Some(error) = sort_error {
                let comp_error = error.to_compile_error();
                return Ok(quote! { #val #comp_error});
            } else {
                return Ok(quote! { #val});
            }
        }
        _ => {
            return Err(syn::Error::new(
                Span::call_site(),
                "expected enum or match expression",
            ))
        }
    }
}

fn is_match_expr(expr: &syn::Stmt) -> Option<syn::ExprMatch> {
    if let syn::Stmt::Expr(stmt_expr, _) = expr {
        if let syn::Expr::Match(expr_match) = stmt_expr {
            return Some(expr_match.clone());
        } else {
            return None;
        }
    } else {
        return None;
    }
}

fn is_sorted_flag(attr: &syn::Attribute) -> Option<()> {
    if attr.meta.path().is_ident("sorted") {
        return Some(());
    } else {
        return None;
    }
}

fn item_fn_strategy(
    mut data: impl VisitMut + fmt::Debug,
    i: &mut syn::ItemFn,
) -> syn::Result<TokenStream2> {
    data.visit_item_fn_mut(i);
    let expr_match = &i
        .block
        .stmts
        .iter()
        .find_map(|stmt| is_match_expr(&stmt))
        .expect("Failed to find stmt in fn strat.");
    let pathes = collect_pathes(expr_match.arms.clone())?;
    match lexicographic_sort(&pathes, pathes.len()) {
        Ok(()) => {
            if let Some(_) = expr_match
                .attrs
                .iter()
                .find_map(|attr| is_sorted_flag(&attr))
            {
                return Ok(quote! { #i });
            } else {
                panic!()
            }
        }
        Err(err) => return Err(err),
    }
}

fn lexicographic_sort(pathes: &Vec<syn::Path>, len: usize) -> syn::Result<()> {
    let mut sorted_list: Vec<String> = Vec::with_capacity(len);
    for path in pathes {
        if !path.segments.is_empty() {
            let name = path
                .get_ident()
                .expect("path.get_ident() failed.")
                .to_string();
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

trait FullPath {
    fn full_path(&self) -> String;
}

impl FullPath for syn::Path {
    fn full_path(&self) -> String {
        return self.segments.iter().fold(String::new(), |name, iter| {
            format!("{}::{}", name, iter.ident)
        });
    }
}

trait IntoPath {
    fn into_path(self) -> syn::Result<syn::Path>;
}

impl IntoPath for syn::Variant {
    fn into_path(self) -> syn::Result<syn::Path> {
        let mut segments = syn::punctuated::Punctuated::new();
        segments.push_value(self.ident.into());
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
