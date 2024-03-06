use proc_macro2::TokenStream;
use quote::quote;
use syn::Result;

use crate::{irfmt::IRFmtInput, macro_attr::IRKind};

pub(crate) fn derive_not_parsable(input: impl Into<TokenStream>) -> Result<TokenStream> {
    let input = syn::parse2::<IRFmtInput>(input.into())?;
    match input.kind {
        IRKind::Type => Ok(emit_not_parsable_type(input.ident)),
        IRKind::Attribute => Ok(emit_not_parsable_attribute(input.ident)),
        IRKind::Op => Ok(emit_not_parsable_op(input.ident)),
    }
}

fn emit_not_parsable_type(name: syn::Ident) -> TokenStream {
    quote! {
        impl ::pliron::parsable::Parsable for #name {
            type Arg = ();
            type Parsed = ::pliron::r#type::TypePtr<Self>;

            fn parse<'a>(
                _state_stream: &mut ::pliron::parsable::StateStream<'a>,
                _arg: Self::Arg,
            ) -> ::pliron::parsable::ParseResult<'a, Self::Parsed>
            where
                Self: Sized,
            {
                todo!()
            }

        }
    }
}

fn emit_not_parsable_attribute(name: syn::Ident) -> TokenStream {
    quote! {
        impl ::pliron::parsable::Parsable for #name {
            type Arg = ();
            type Parsed = ::pliron::attribute::AttrObj;

            fn parse<'a>(
                _state_stream: &mut ::pliron::parsable::StateStream<'a>,
                _arg: Self::Arg,
            ) -> ::pliron::parsable::ParseResult<'a, Self::Parsed> {
                todo!()
            }

        }
    }
}

fn emit_not_parsable_op(name: syn::Ident) -> TokenStream {
    quote! {
        impl ::pliron::parsable::Parsable for #name {
            type Arg = Vec<(::pliron::identifier::Identifier, ::pliron::location::Location)>;
            type Parsed = ::pliron::op::OpObj;

            fn parse<'a>(
                _state_stream: &mut ::pliron::parsable::StateStream<'a>,
                _arg: Self::Arg,
            ) -> ::pliron::parsable::ParseResult<'a, Self::Parsed> {
                todo!()
            }

        }
    }
}
