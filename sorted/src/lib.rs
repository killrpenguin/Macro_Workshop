extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::quote;
use syn::visit_mut::VisitMut;
use syn::{parse_macro_input, visit_mut};

#[proc_macro_attribute]
pub fn sorted(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut out = item.clone();
    let input_stream: syn::Item = parse_macro_input!(item as syn::Item);
    if let Err(e) = sorted_impl::<(), syn::Error>(&input_stream) {
        out.extend(TokenStream::from(e.to_compile_error()));
    }
    out
}

fn sorted_impl<T, E>(input: &syn::Item) -> syn::Result<()> {
    match input {
        syn::Item::Enum(enum_variants) => {
            if !enum_variants.variants.is_empty() {
                let variant_paths: Vec<syn::Path> =
                    collect_paths(enum_variants.variants.clone()).expect("");
                if let Err(e) = lexicographic_sort(variant_paths, enum_variants.variants.len()) {
                    return Err(e);
                } else {
                    return Ok(());
                }
            }
            return Ok(());
        }
        _ => Err(syn::Error::new(
            Span::call_site(),
            "expected enum or match expression",
        )),
    }
}

#[proc_macro_attribute]
pub fn check(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let mut stream: syn::ItemFn = parse_macro_input!(item as syn::ItemFn);
    let mut lm = LexicographicMatch::default();
    lm.visit_item_fn_mut(&mut stream);
    let mut out = quote! { #stream };
    out.extend(lm.errors.into_iter().map(|err| err.to_compile_error()));
    out.into()
}

#[derive(Default, Debug)]
struct LexicographicMatch {
    errors: Vec<syn::Error>,
}

impl visit_mut::VisitMut for LexicographicMatch {
    fn visit_expr_match_mut(&mut self, node: &mut syn::ExprMatch) {
        if node.attrs.iter().any(|expr| expr.path().is_ident("sorted")) {
            node.attrs.retain(|expr| !expr.path().is_ident("sorted"));
            let mut paths = Vec::with_capacity(node.arms.len());
            for arm in node.arms.iter() {
                let path_segs =
                    &arm.clone().into_path().expect("Expected path in visit_expr_match_mut.");
                paths.push(path_segs.clone());
            }
            let _ = lexicographic_sort(paths, node.arms.len())
                .unwrap_or_else(|err| self.errors.push(err));
        }
        visit_mut::visit_expr_match_mut(self, node);
    }
}

fn lexicographic_sort(paths: Vec<syn::Path>, len: usize) -> syn::Result<()> {
    let mut sorted_list: Vec<String> = Vec::with_capacity(len);
    for path in paths {
        let segment: &syn::PathSegment = path.segments.iter().next().unwrap();
        let name = segment.ident.to_string();
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
    Ok(())
}

fn collect_paths<I>(iter: I) -> syn::Result<Vec<syn::Path>>
where
    I: IntoIterator,
    I::Item: IntoPath,
{
    iter.into_iter().map(IntoPath::into_path).collect()
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
            _ => Err(syn::Error::new(
                Span::call_site(),
                "Failed to find path for pattern in match expression.",
            )),
        }
    }
}
