#![allow(unused_imports, unused_variables)]
#![allow(dead_code)]

extern crate proc_macro;
use proc_macro::TokenStream;
use proc_macro2::Span;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use std::fmt;
use syn::{
    parse_macro_input, parse_quote,
    visit_mut::{self, VisitMut},
};

#[proc_macro_attribute]
pub fn sorted(_attrs: TokenStream, item: TokenStream) -> TokenStream {
    let mut out = item.clone();
    let mut stream: syn::Item = parse_macro_input!(item as syn::Item);
    match item_strategy::<LexicographicMatch, TokenStream2>(None, &mut stream) {
        Ok(val) => return stream.into_token_stream().into(),
        Err(err) => {
            out.extend(TokenStream::from(err.to_compile_error()));
            return out;
        }
    };
}

#[proc_macro_attribute]
pub fn check(_attrs: TokenStream, item: TokenStream) -> TokenStream {
    let mut out = item.clone();
    let mut stream: syn::Item = parse_macro_input!(item as syn::Item);
    let data = LexicographicMatch::default();
    match item_strategy::<LexicographicMatch, TokenStream2>(Some(data), &mut stream) {
        Ok(val) => {
            eprintln!("{:#?}", stream);
            return val.into_token_stream().into()
        },
        Err(err) => {
            out.extend(TokenStream::from(err.to_compile_error()));
            return out;
        }
    };
}

#[derive(Debug, Default)]
struct LexicographicMatch;

fn item_strategy<T: VisitMut + fmt::Debug, R>(
    data: Option<T>,
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
        syn::Item::Fn(val) => match stmt_strategy(data.unwrap(), val.block.stmts.clone()) {
            Ok(ts) => {
                return Ok(quote! { #stream #ts })
            },
            Err(err) => return Err(err),
        },
        _ => {
            return Err(syn::Error::new(
                Span::call_site(),
                "expected enum or match expression",
            ))
        }
    }
}

fn stmt_strategy<T: VisitMut + fmt::Debug>(
    mut data: T,
    mut stmts: Vec<syn::Stmt>,
) -> syn::Result<TokenStream2> {
    let mut out = TokenStream2::new();
    for stmt in stmts.iter_mut() {
        data.visit_stmt_mut(stmt);
        match stmt {
            syn::Stmt::Expr(ref mut expr, _) => match expr {
                syn::Expr::Match(ref mut match_expr) => {
                    eprintln!("{:#?}", match_expr);
                    let pathes = collect_pathes(match_expr.arms.clone())?;
                    match lexicographic_sort(&pathes, pathes.len()) {
                        Ok(()) => continue,
                        Err(err) => return Err(err),
                    }
                },
                other => out.extend(quote! { #other }),
            },
            other => out.extend(quote! { #other }),
        };
    }
    Ok(out)
}

fn lexicographic_sort(pathes: &Vec<syn::Path>, len: usize) -> syn::Result<()> {
    let mut sorted_list: Vec<String> = Vec::with_capacity(len);
    for path in pathes {
        if !path.segments.is_empty() {
            let name = path.get_ident().unwrap().to_string();
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
                    format!(
                        "{} should sort before {}",
                        name.to_string(),
                        sorted_list[should_be]
                    ),
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
            node.attrs.clear()
        }
        visit_mut::visit_expr_match_mut(self, node);
    }
    fn visit_stmt_mut(&mut self, node: &mut syn::Stmt) {
        visit_mut::visit_stmt_mut(self, node);
        match node {
            syn::Stmt::Expr(ref mut expr, _) => match expr {
                syn::Expr::Match(ref mut match_expr) => {
                    self.visit_expr_match_mut(match_expr);
                    let pathes = collect_pathes(match_expr.arms.clone()).unwrap();
                    match lexicographic_sort(&pathes, pathes.len()) {
                        Ok(()) => return,
                        Err(err) => err.to_compile_error(),
                    }
                }
                _ => return,
            },
            _ => return,
        };
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
