use proc_macro2::{Span, TokenStream};
use quote::format_ident;
use syn::parse::{Parse, ParseStream};
use syn::Data;
use syn::{self, DataStruct, DeriveInput};
use winnow::{
    ascii::{self, escaped, multispace0},
    combinator::{alt, cut_err, delimited, opt, preceded, separated},
    error::{ErrorKind, StrContext},
    stream::Location,
    token::{none_of, one_of, take_while},
    Located, Parser,
};

use crate::attr::{require_once, AsmFormat, Attribute, IRKind};

pub(crate) struct AsmFmtInput {
    pub ident: syn::Ident,
    pub kind: IRKind,
    pub format: AsmFormat,
    pub data: FmtData,
}

pub(crate) enum FmtData {
    Struct(Struct),
}

impl Parse for AsmFmtInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let input = DeriveInput::parse(input)?;
        Self::try_from(input)
    }
}

impl TryFrom<DeriveInput> for AsmFmtInput {
    type Error = syn::Error;

    fn try_from(input: DeriveInput) -> syn::Result<Self> {
        let mut kind = None;
        let mut format = None;

        for attr in &input.attrs {
            if attr.path().is_ident(AsmFormat::ATTR_NAME) {
                require_once(AsmFormat::ATTR_NAME, &format, attr)?;
                format = Some(AsmFormat::from_syn(attr)?);
            }
            if attr.path().is_ident(IRKind::ATTR_NAME) {
                require_once(IRKind::ATTR_NAME, &kind, attr)?;
                kind = Some(IRKind::from_syn(attr)?);
            }
        }

        let Some(kind) = kind else {
            return Err(syn::Error::new_spanned(
                input,
                "unknown IR object type. Use #[ir_kind=...] or one of the supported derive clauses Type, Attrib, ...",
            ));
        };

        let data = match input.data {
            Data::Struct(ref data) => Struct::from_syn(data).map(FmtData::Struct),
            Data::Enum(_) => Err(syn::Error::new_spanned(
                &input,
                "Type can only be derived for structs",
            )),
            Data::Union(_) => Err(syn::Error::new_spanned(
                &input,
                "Type can only be derived for structs",
            )),
        }?;

        let mut format = match format {
            Some(f) => f,
            None => {
                let mut format = match kind {
                    IRKind::Op => generic_op_format(),
                    IRKind::Type | IRKind::Attribute => try_format_from_input(&input)?,
                };
                if !format.is_empty() && kind != IRKind::Op {
                    format.enclose(Elem::Lit("<".into()), Elem::Lit(">".into()));
                }
                format.into()
            }
        };

        if kind == IRKind::Op {
            format.format_mut().prepend(Optional::new(
                Elem::new_directive("results"),
                Format::from(vec![Elem::new_directive("results"), Elem::new_lit(" = ")]),
            ));
        }

        Ok(Self {
            ident: input.ident,
            kind,
            format,
            data,
        })
    }
}

pub(crate) struct Struct {
    pub fields: Vec<FieldIdent>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum FieldIdent {
    Named(String),
    Unnamed(usize),
}

impl From<FieldIdent> for Elem {
    fn from(value: FieldIdent) -> Self {
        match value {
            FieldIdent::Named(name) => Elem::new_var(name),
            FieldIdent::Unnamed(index) => Elem::new_unnamed_var(index),
        }
    }
}

impl From<&FieldIdent> for Elem {
    fn from(value: &FieldIdent) -> Self {
        match value {
            FieldIdent::Named(name) => Elem::new_var(name),
            FieldIdent::Unnamed(index) => Elem::new_unnamed_var(*index),
        }
    }
}

impl From<&str> for FieldIdent {
    fn from(s: &str) -> Self {
        Self::Named(s.to_string())
    }
}

impl From<String> for FieldIdent {
    fn from(s: String) -> Self {
        Self::Named(s)
    }
}

impl From<usize> for FieldIdent {
    fn from(i: usize) -> Self {
        Self::Unnamed(i)
    }
}

impl quote::ToTokens for FieldIdent {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        match self {
            Self::Named(name) => {
                let ident = format_ident!("{}", name);
                ident.to_tokens(tokens);
            }
            Self::Unnamed(index) => {
                let ident = syn::Index::from(*index);
                ident.to_tokens(tokens);
            }
        }
    }
}

impl Struct {
    fn from_syn(data: &DataStruct) -> syn::Result<Self> {
        let fields = data
            .fields
            .iter()
            .enumerate()
            .map(|(i, f)| match f.ident {
                Some(ref ident) => FieldIdent::Named(ident.to_string()),
                None => FieldIdent::Unnamed(i),
            })
            .collect();

        Ok(Self { fields })
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub(crate) struct Format {
    pub elems: Vec<Elem>,
}

impl From<Vec<Elem>> for Format {
    fn from(elems: Vec<Elem>) -> Self {
        Self { elems }
    }
}

impl Format {
    pub fn is_empty(&self) -> bool {
        self.elems.is_empty()
    }

    pub fn prepend(&mut self, elem: impl Into<Elem>) {
        self.elems.insert(0, elem.into());
    }

    pub fn append(&mut self, elem: impl Into<Elem>) {
        self.elems.push(elem.into());
    }

    pub fn enclose(&mut self, open: impl Into<Elem>, close: impl Into<Elem>) {
        self.prepend(open);
        self.append(close);
    }
}

impl Format {
    pub fn parse(input: &str) -> Result<Self> {
        let mut input = Located::new(input);
        let elems = match parse_asm_fmt(&mut input) {
            Ok(elems) => elems,
            Err(err) => {
                let msg = format!("{}", err);
                return Err(msg.into());
            }
        };
        Ok(Self { elems })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum Elem {
    // Literal is a custom string enclosed in backticks. For example `lit` or `(`.
    Lit(Lit),

    // varialbes are custom identifiers starting with a dollar sign. For example $var or $0.
    Var(Var),

    // Unnamed variables are custom identifiers starting with a dollar sign and a number.
    UnnamedVar(UnnamedVar),

    // Directives are builtin identifiers. Some directives may have optional arguments enclosed
    // in parens. For example `attr-dict` or `directive($arg1, other-directive)`.
    Directive(Directive),

    Optional(Optional),
}

impl Default for Elem {
    fn default() -> Self {
        Self::Lit(Lit::new(""))
    }
}

impl Elem {
    pub fn new_lit(s: impl Into<String>) -> Self {
        Self::Lit(Lit::new(s))
    }

    pub fn new_lit_at(pos: usize, s: impl Into<String>) -> Self {
        Self::Lit(Lit::new_at(pos, s))
    }

    pub fn new_var(s: impl Into<String>) -> Self {
        Self::Var(Var::new(s))
    }

    pub fn new_var_at(pos: usize, s: impl Into<String>) -> Self {
        Self::Var(Var::new_at(pos, s.into()))
    }

    pub fn new_unnamed_var(index: usize) -> Self {
        Self::UnnamedVar(UnnamedVar::new(index))
    }

    pub fn new_unnamed_var_at(pos: usize, index: usize) -> Self {
        Self::UnnamedVar(UnnamedVar::new_at(pos, index))
    }

    pub fn new_directive(name: impl Into<String>) -> Self {
        Self::Directive(Directive::new(name))
    }

    pub fn new_directive_at(pos: usize, name: impl Into<String>) -> Self {
        Self::Directive(Directive::new_at(pos, name))
    }

    #[allow(dead_code)]
    pub fn new_directive_with_args(name: impl Into<String>, args: Vec<Elem>) -> Self {
        Self::Directive(Directive::new_with_args(name, args))
    }

    pub fn new_directive_with_args_at(
        pos: usize,
        name: impl Into<String>,
        args: Vec<Elem>,
    ) -> Self {
        Self::Directive(Directive::new_with_args_at(pos, name, args))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Lit {
    pub pos: Option<usize>,
    pub lit: String,
}

impl From<Lit> for Elem {
    fn from(lit: Lit) -> Self {
        Self::Lit(lit)
    }
}

impl From<&str> for Lit {
    fn from(s: &str) -> Self {
        Self::new(s)
    }
}

impl Lit {
    pub fn new(s: impl Into<String>) -> Self {
        Self {
            pos: None,
            lit: s.into(),
        }
    }

    pub fn new_at(pos: usize, s: impl Into<String>) -> Self {
        Self {
            pos: Some(pos),
            lit: s.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Var {
    pub pos: Option<usize>,
    pub name: String,
}

impl Var {
    pub fn new(s: impl Into<String>) -> Self {
        Self {
            pos: None,
            name: s.into(),
        }
    }

    pub fn new_at(pos: usize, s: impl Into<String>) -> Self {
        Self {
            pos: Some(pos),
            name: s.into(),
        }
    }
}

impl From<Var> for Elem {
    fn from(lit: Var) -> Self {
        Self::Var(lit)
    }
}

impl From<&str> for Var {
    fn from(s: &str) -> Self {
        Self::new(s)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UnnamedVar {
    pub pos: Option<usize>,
    pub index: usize,
}

impl From<UnnamedVar> for Elem {
    fn from(var: UnnamedVar) -> Self {
        Self::UnnamedVar(var)
    }
}

impl From<usize> for UnnamedVar {
    fn from(index: usize) -> Self {
        Self::new(index)
    }
}

impl UnnamedVar {
    pub fn new(index: usize) -> Self {
        Self { pos: None, index }
    }

    pub fn new_at(pos: usize, index: usize) -> Self {
        Self {
            pos: Some(pos),
            index,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct Directive {
    pub pos: Option<usize>,
    pub name: String,
    pub args: Vec<Elem>,
}

impl Directive {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            pos: None,
            name: name.into(),
            args: vec![],
        }
    }

    pub fn new_at(pos: usize, name: impl Into<String>) -> Self {
        Self {
            pos: Some(pos),
            name: name.into(),
            args: vec![],
        }
    }

    pub fn new_with_args(name: impl Into<String>, args: Vec<Elem>) -> Self {
        Self {
            pos: None,
            name: name.into(),
            args,
        }
    }

    pub fn new_with_args_at(pos: usize, name: impl Into<String>, args: Vec<Elem>) -> Self {
        Self {
            pos: Some(pos),
            name: name.into(),
            args,
        }
    }
}

impl From<Directive> for Elem {
    fn from(directive: Directive) -> Self {
        Self::Directive(directive)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Optional {
    pub check: Box<Elem>,
    pub then_format: Format,
    pub else_format: Option<Format>,
}

impl From<Optional> for Elem {
    fn from(optional: Optional) -> Self {
        Self::Optional(optional)
    }
}

impl Optional {
    pub fn new(check: Elem, then_format: Format) -> Self {
        Self {
            check: Box::new(check),
            then_format,
            else_format: None,
        }
    }

    #[allow(dead_code)]
    pub fn new_with_else(check: Elem, then_format: Format, else_format: Format) -> Self {
        Self {
            check: Box::new(check),
            then_format,
            else_format: Some(else_format),
        }
    }
}

type Result<T, E = Error> = std::result::Result<T, E>;

type Error = Box<dyn std::error::Error>;

struct FmtValue(Vec<Elem>);

impl From<Elem> for FmtValue {
    fn from(elem: Elem) -> Self {
        Self(vec![elem])
    }
}

impl From<Vec<Elem>> for FmtValue {
    fn from(elems: Vec<Elem>) -> Self {
        Self(elems)
    }
}

impl From<Directive> for FmtValue {
    fn from(d: Directive) -> Self {
        Self(vec![Elem::Directive(d)])
    }
}

impl From<Optional> for FmtValue {
    fn from(opt: Optional) -> Self {
        Self(vec![Elem::Optional(opt)])
    }
}

impl From<FmtValue> for Vec<Elem> {
    fn from(value: FmtValue) -> Self {
        value.0
    }
}

impl From<FmtValue> for Format {
    fn from(value: FmtValue) -> Self {
        Self { elems: value.0 }
    }
}

impl FmtValue {
    // flattens a FmtValue such that it contains no nested Values.
    fn flatten(self) -> Vec<Elem> {
        self.0
    }

    fn flatten_into(self, values: &mut Vec<Elem>) {
        values.extend(self.0);
    }
}

pub struct AttribTypeFmtEvaler<'a> {
    span: Span,
    fields: &'a [FieldIdent],
}

impl<'a> AttribTypeFmtEvaler<'a> {
    pub fn new(span: Span, fields: &'a [FieldIdent]) -> Self {
        Self { span, fields }
    }

    fn span(&self) -> Span {
        self.span
    }

    pub fn eval(&self, f: Format) -> syn::Result<Format> {
        Ok(self.eval_format(f, true)?.into())
    }

    fn eval_format(&self, f: Format, toplevel: bool) -> syn::Result<Format> {
        let elems = self.eval_elems(f.elems, toplevel)?;
        Ok(elems.into())
    }

    fn eval_elems(&self, elem: Vec<Elem>, toplevel: bool) -> syn::Result<FmtValue> {
        let results = elem.into_iter().map(|e| self.eval_elem(e, toplevel));
        let mut elems = vec![];
        for r in results {
            r?.flatten_into(&mut elems);
        }
        Ok(FmtValue(elems))
    }

    fn eval_elem(&self, elem: Elem, toplevel: bool) -> syn::Result<FmtValue> {
        match elem {
            Elem::Lit(_) | Elem::Var(_) | Elem::UnnamedVar(_) => Ok(elem.into()),
            Elem::Directive(d) => self.eval_directive(d, toplevel),
            Elem::Optional(opt) => self.eval_optional(opt, toplevel),
        }
    }

    fn eval_directive(&self, d: Directive, toplevel: bool) -> syn::Result<FmtValue> {
        match d.name.as_str() {
            "params" => {
                require_no_args(self.span, "params", &d.args)?;
                if toplevel {
                    Ok(FmtValue::from(d))
                } else {
                    Ok(FmtValue::from(
                        self.fields.iter().map(|f| f.into()).collect::<Vec<_>>(),
                    ))
                }
            }
            "struct" => {
                require_toplevel(self.span, &d.name, toplevel)?;
                require_args(self.span, "struct", &d.args)?;
                let args = self.eval_args(d.args)?;
                Ok(FmtValue::from(Directive { args, ..d }))
            }
            _ => {
                require_toplevel(self.span, &d.name, toplevel)?;
                let args = self.eval_args(d.args)?;
                Ok(FmtValue::from(Directive { args, ..d }))
            }
        }
    }

    fn eval_args(&self, args: Vec<Elem>) -> syn::Result<Vec<Elem>> {
        let values = self.eval_elems(args, false)?;
        Ok(values.into())
    }

    fn eval_optional(&self, opt: Optional, toplevel: bool) -> syn::Result<FmtValue> {
        require_toplevel(self.span(), "optional", toplevel).unwrap();

        let mut check_tmp = self.eval_elem(*opt.check, false)?.flatten();
        let Some(check) = check_tmp.pop() else {
            return Err(syn::Error::new(
                self.span(),
                "`check` argument of `optional` has no value",
            ));
        };
        if !check_tmp.is_empty() {
            return Err(syn::Error::new(
                self.span(),
                "`check` argument of `optional` directive must be a single value",
            ));
        }

        let then_format = self.eval_format(opt.then_format, toplevel)?;
        let else_format = opt
            .else_format
            .map(|f| self.eval_format(f, toplevel))
            .transpose()?;

        Ok(FmtValue::from(Optional {
            check: Box::new(check),
            then_format,
            else_format,
        }))
    }
}

fn require_toplevel(span: Span, directive: &str, toplevel: bool) -> syn::Result<()> {
    if !toplevel {
        return Err(syn::Error::new(
            span,
            format!("`{}` directive is only allowed at the top-level", directive),
        ));
    }
    Ok(())
}

fn require_no_args(span: Span, directive: &str, args: &[Elem]) -> syn::Result<()> {
    if !args.is_empty() {
        return Err(syn::Error::new(
            span,
            format!("`{}` directive does not take any arguments", directive),
        ));
    }
    Ok(())
}

fn require_args(span: Span, directive: &str, args: &[Elem]) -> syn::Result<()> {
    if args.is_empty() {
        return Err(syn::Error::new(
            span,
            format!("`{}` directive requires arguments", directive),
        ));
    }
    Ok(())
}

type Str<'a> = Located<&'a str>;

type PResult<O, E = ContextError> = winnow::PResult<O, E>;

#[derive(Debug, PartialEq)]
pub(super) struct ContextError {
    max_pos: usize,
    context: Vec<StrContext>,
}

impl ContextError {
    fn new_at(pos: usize) -> Self {
        Self {
            max_pos: pos,
            context: Vec::new(),
        }
    }
}

impl<'a> winnow::error::ParserError<Str<'a>> for ContextError {
    fn from_error_kind(cx: &Str<'a>, _kind: ErrorKind) -> Self {
        Self::new_at(cx.location())
    }

    fn append(self, _input: &Str<'a>, _kind: ErrorKind) -> Self {
        self
    }

    fn or(self, other: Self) -> Self {
        // rightmost parse wins
        match self.max_pos.cmp(&other.max_pos) {
            std::cmp::Ordering::Less => other,
            std::cmp::Ordering::Greater => self,
            std::cmp::Ordering::Equal => {
                let max_pos = self.max_pos;
                let (mut context, other) = if self.context.capacity() > other.context.len() {
                    (self.context, other.context)
                } else {
                    (other.context, self.context)
                };
                context.extend(other);
                Self { max_pos, context }
            }
        }
    }
}

impl<'a> winnow::error::AddContext<Str<'a>, StrContext> for ContextError {
    fn add_context(mut self, cx: &Str<'a>, info: StrContext) -> Self {
        let pos = cx.location();
        match pos.cmp(&self.max_pos) {
            std::cmp::Ordering::Less => self,
            std::cmp::Ordering::Greater => {
                self.context.clear();
                self.context.push(info);
                self
            }
            std::cmp::Ordering::Equal => {
                self.context.push(info);
                self
            }
        }
    }
}

fn parse_asm_fmt(input: &mut Str) -> PResult<Vec<Elem>> {
    let mut elems = vec![];
    multispace0.parse_next(input)?;
    while !input.is_empty() {
        elems.push(parse_fmt_elem(input)?);
    }
    Ok(elems)
}

fn parse_fmt_elem(input: &mut Str) -> PResult<Elem> {
    let res = alt((parse_lit, parse_unnamed_var, parse_var, parse_directive)).parse_next(input);
    multispace0(input)?;
    res
}

fn parse_lit(input: &mut Str) -> PResult<Elem> {
    let loc = input.location();
    let string_contents = escaped(
        none_of(&['`', '\\']),
        '\\',
        one_of(&['\\', '`', 'n', 'r', 't']),
    );
    let s = delimited(backtick, string_contents, backtick)
        .context(StrContext::Label("<literal>"))
        .parse_next(input)?;
    Ok(Elem::new_lit_at(loc, s))
}

fn parse_var(input: &mut Str) -> PResult<Elem> {
    let loc = input.location();
    let s = preceded(
        dollar,
        take_while(1.., |c: char| c.is_alphanumeric() || c == '_' || c == '.'),
    )
    .context(StrContext::Label("<variable>"))
    .parse_next(input)?;
    Ok(Elem::new_var_at(loc, s))
}

fn parse_unnamed_var(input: &mut Str) -> PResult<Elem> {
    let loc = input.location();
    let s = preceded(dollar, ascii::digit1)
        .context(StrContext::Label("<variable>"))
        .parse_next(input)?;
    let idx = s.parse::<usize>().unwrap();
    Ok(Elem::new_unnamed_var_at(loc, idx))
}

fn parse_directive(input: &mut Str) -> PResult<Elem> {
    let loc = input.location();
    let name = take_while(1.., |c: char| c.is_alphanumeric() || c == '-' || c == '_')
        .context(StrContext::Label("<directive>"))
        .parse_next(input)?;

    if opt(paren_open).parse_next(input)?.is_none() {
        return Ok(Elem::new_directive_at(loc, name));
    }
    let args = cut_err(separated(0.., parse_fmt_elem, (comma, multispace0)))
        .context(StrContext::Label("<directive-argument-list>"))
        .parse_next(input)?;

    paren_close.parse_next(input)?;

    Ok(Elem::new_directive_with_args_at(loc, name, args))
}

fn backtick(input: &mut Str) -> PResult<char> {
    sym('`').parse_next(input)
}

fn dollar(input: &mut Str) -> PResult<char> {
    sym('$').parse_next(input)
}

fn paren_open(input: &mut Str) -> PResult<char> {
    sym('(').parse_next(input)
}

fn paren_close(input: &mut Str) -> PResult<char> {
    sym(')').parse_next(input)
}

fn comma(input: &mut Str) -> PResult<char> {
    sym(',').parse_next(input)
}

fn sym<'a>(c: char) -> impl Parser<Str<'a>, char, ContextError> {
    c.context(StrContext::Expected(c.into()))
}

pub(crate) fn try_format_from_input(input: &syn::DeriveInput) -> syn::Result<Format> {
    // TODO: add support for per field attributes?

    let data = match input.data {
        Data::Struct(ref data) => data,
        _ => {
            return Err(syn::Error::new_spanned(
                input,
                "Type can only be derived for structs",
            ))
        }
    };

    let elems = match data.fields {
        syn::Fields::Named(ref fields) => {
            let mut elems = vec![];
            for (i, field) in fields.named.iter().enumerate() {
                let ident = field.ident.as_ref().unwrap();
                if i > 0 {
                    elems.push(Elem::new_lit(", "));
                }
                elems.push(Elem::new_lit(format!("{}=", ident)));
                elems.push(Elem::new_var(ident.to_string()));
            }
            elems
        }
        syn::Fields::Unnamed(ref fields) => (0..(fields.unnamed.len()))
            .map(|i| Elem::new_unnamed_var(i as usize))
            .collect::<Vec<_>>(),
        syn::Fields::Unit => vec![],
    };
    Ok(Format { elems })
}

pub(crate) fn generic_op_format() -> Format {
    Format {
        elems: vec![Directive::new("operation_generic_format").into()],
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_literal() {
        let input = "`lit`";
        let expected = vec![Elem::new_lit_at(0, "lit")];
        assert_eq!(parse_asm_fmt(&mut Located::new(input)), Ok(expected));
    }

    #[test]
    fn literal_with_escaped_chars() {
        let input = r#"`hello\n \`world\``"#;
        let expected = vec![Elem::new_lit_at(0, r#"hello\n \`world\`"#)];
        assert_eq!(parse_asm_fmt(&mut Located::new(input)), Ok(expected));
    }

    #[test]
    fn simple_variable() {
        let input = "$var";
        let expected = vec![Elem::new_var_at(0, "var")];
        assert_eq!(parse_asm_fmt(&mut Located::new(input)), Ok(expected));
    }

    #[test]
    fn simple_directive() {
        let input = "directive";
        let expected = vec![Elem::new_directive_at(0, "directive")];
        assert_eq!(parse_asm_fmt(&mut Located::new(input)), Ok(expected));
    }

    #[test]
    fn directive_with_empty_args() {
        let input = "directive()";
        let expected = vec![Elem::new_directive_at(0, "directive")];
        assert_eq!(parse_asm_fmt(&mut Located::new(input)), Ok(expected));
    }

    #[test]
    fn directive_with_args() {
        let input = "directive($arg1, other-directive)";
        let expected = vec![Elem::new_directive_with_args_at(
            0,
            "directive",
            vec![
                Elem::new_var_at(10, "arg1"),
                Elem::new_directive_at(17, "other-directive"),
            ],
        )];
        assert_eq!(parse_asm_fmt(&mut Located::new(input)), Ok(expected));
    }
}
