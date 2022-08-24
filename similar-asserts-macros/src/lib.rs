extern crate proc_macro;

use proc_macro::{Delimiter, Group, TokenTree};
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::punctuated::Punctuated;

type FormatArgs = Punctuated<syn::Expr, syn::token::Comma>;

#[proc_macro]
pub fn assert_impl(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    fix_hygiene_bug(actual_assert_impl(syn::parse_macro_input!(tokens)).into())
}

fn actual_assert_impl(args: Args) -> TokenStream {
    match args.expr {
        syn::Expr::Binary(expr) => assert_binary_op(args.crate_name, expr, args.format_args),
        expr => assert_bool_expr(args.crate_name, expr, args.format_args),
    }
}

fn assert_binary_op(
    crate_name: syn::Path,
    expr: syn::ExprBinary,
    format_args: Option<FormatArgs>,
) -> TokenStream {
    match expr.op {
        // For a == b we directly dispatch to `assert_eq!` which performs diffing.
        syn::BinOp::Eq(_) => {
            let syn::ExprBinary { left, right, .. } = &expr;
            return match format_args {
                Some(format_args) => {
                    quote! { #crate_name::assert_eq!(#left, #right, #format_args) }
                }
                None => quote! { #crate_name::assert_eq!(#left, #right) },
            };
        }
        syn::BinOp::Lt(_)
        | syn::BinOp::Le(_)
        | syn::BinOp::Ne(_)
        | syn::BinOp::Ge(_)
        | syn::BinOp::Gt(_) => {}
        _ => return assert_bool_expr(crate_name, syn::Expr::Binary(expr), format_args),
    }

    let syn::ExprBinary {
        left, right, op, ..
    } = &expr;
    let left_expr = expression_to_string(&crate_name, left.to_token_stream());
    let right_expr = expression_to_string(&crate_name, right.to_token_stream());
    let op_str = op.to_token_stream().to_string();
    let hint = match format_args {
        Some(x) => quote!(format_args!(": {}", format_args!(#x))),
        None => quote!(""),
    };

    quote! {
        match (&(#left), &(#right)) {
            (left_val, right_val) => {
                if !(*left_val #op *right_val) {
                    use #crate_name::__style;
                    use #crate_name::print::{PrintMode, PrintObject};
                    let left_expr = #left_expr;
                    let right_expr = #right_expr;
                    let mut left_val_tup1 = (&left_val,);
                    let mut right_val_tup1 = (&right_val,);
                    let left_short = left_val_tup1.print_object(PrintMode::Default);
                    let left_short = left_short.as_deref().unwrap_or("<unprintable object>");
                    let right_short = right_val_tup1.print_object(PrintMode::Default);
                    let right_short = right_short.as_deref().unwrap_or("<unprintable object>");
                    panic!(
                        "assertion failed: `(left {} right)`{}'\
                        \n  left: `{}` = {}\
                        \n right: `{}` = {}\n",
                        #op_str,
                        #hint,
                        __style(&left_expr).yellow(),
                        __style(&left_short).cyan(),
                        __style(&right_expr).yellow(),
                        __style(&right_short).cyan(),
                    );
                }
            }
        }
    }
}

fn assert_bool_expr(
    crate_name: syn::Path,
    expr: syn::Expr,
    format_args: Option<FormatArgs>,
) -> TokenStream {
    let expr_str = expression_to_string(&crate_name, expr.to_token_stream());

    let custom_msg = match format_args {
        Some(x) => quote!(Some(format_args!(#x))),
        None => quote!(None),
    };

    quote! {
        match #expr {
            false => {
                #crate_name::__magic_assert::print::FailedCheck {
                    file: file!(),
                    line: line!(),
                    column: column!(),
                    custom_msg: #custom_msg,
                    expression: #crate_name::__magic_assert::print::BooleanExpr {
                        expression: #expr_str,
                    },
                }.print();
                Err(())
            }
            true => Ok(()),
        }
    }
}

fn expression_to_string(crate_name: &syn::Path, ts: TokenStream) -> TokenStream {
    quote!(#crate_name::__stringify!(#ts))
}

struct Args {
    crate_name: syn::Path,
    expr: syn::Expr,
    format_args: Option<FormatArgs>,
}

impl syn::parse::Parse for Args {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let crate_name = input.parse()?;
        let _comma: syn::token::Comma = input.parse()?;
        let expr = input.parse()?;
        let format_args = if input.is_empty() {
            FormatArgs::new()
        } else {
            input.parse::<syn::token::Comma>()?;
            FormatArgs::parse_terminated(input)?
        };

        let format_args = Some(format_args).filter(|x| !x.is_empty());
        Ok(Self {
            crate_name,
            expr,
            format_args,
        })
    }
}

/// Fix-up a token stream to work around a hygiene bug in the Rust compiler.
///
/// This turns all none-delimited groups into parenthesis,
/// so that their precedence remains correct.
///
/// See https://github.com/rust-lang/rust/issues/74036
/// and https://github.com/rust-lang/rust/issues/67062
fn fix_hygiene_bug(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    tokens
        .into_iter()
        .map(|token| match token {
            TokenTree::Group(group) => {
                let mut fixed = Group::new(
                    match group.delimiter() {
                        Delimiter::None => Delimiter::Parenthesis,
                        delimiter => delimiter,
                    },
                    fix_hygiene_bug(group.stream()),
                );
                fixed.set_span(group.span());
                TokenTree::Group(fixed)
            }
            token => token,
        })
        .collect()
}
