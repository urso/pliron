use crate::{
    common_traits::Verify,
    context::{Context, Ptr},
    dialect::Dialect,
    error::CompilerError,
    impl_type,
    parsable::{identifier, spaced, to_parse_result, Parsable, StateStream},
    printable::{self, Printable, PrintableIter},
    r#type::{type_parser, Type, TypeObj},
    storage_uniquer::TypeValueHash,
};
use combine::{between, easy, optional, sep_by, token, ParseResult, Parser};

use std::hash::Hash;

/// A field in a [StructType].
#[derive(Clone, PartialEq, Eq)]
pub struct StructField {
    pub field_name: String,
    pub field_type: Ptr<TypeObj>,
}

impl Printable for StructField {
    fn fmt(
        &self,
        ctx: &Context,
        state: &printable::State,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        write!(
            f,
            "{}: {}",
            self.field_name,
            self.field_type.print(ctx, state)
        )
    }
}

impl Parsable for StructField {
    type Parsed = StructField;

    fn parse<'a>(
        state_stream: &mut StateStream<'a>,
    ) -> ParseResult<Self::Parsed, easy::ParseError<StateStream<'a>>> {
        // Parse a single type annotated field.
        (spaced(identifier()), token(':'), spaced(type_parser()))
            .parse_stream(state_stream)
            .map(|(field_name, _, field_type)| StructField {
                field_name,
                field_type,
            })
    }
}

/// Represents a c-like struct type.
/// Limitations and warnings on its usage are similar to that in MLIR.
/// `<https://mlir.llvm.org/docs/Dialects/LLVM/#structure-types>`
///   1. Anonymous (aka unnamed) structs cannot be recursive.
///   2. Named structs are uniqued *only* by name, and may be recursive.
///      Call "set_fields" after creation to set recursive types.
///   3. LLVM calls anonymous structs as literal structs and
///      named structs as identified structs.
pub struct StructType {
    name: Option<String>,
    fields: Vec<StructField>,
    finalized: bool,
}
impl_type!(StructType, "struct", "llvm");

impl StructType {
    /// Get or create a new named StructType.
    /// If fields is None, it indicates an opaque (i.e., not finalized) struct.
    /// Opaque structs must be finalized (by passing non-none `fields`) for verify() to succeed.
    /// Opaque structs are an intermediary in creating recursive types.
    /// Returns an error when the name is already registered but the fields don't match.
    pub fn get_named(
        ctx: &mut Context,
        name: &str,
        fields: Option<Vec<StructField>>,
    ) -> Result<Ptr<TypeObj>, CompilerError> {
        let self_ptr = Type::register_instance(
            StructType {
                name: Some(name.to_string()),
                fields: fields.clone().unwrap_or_default(),
                finalized: fields.is_some(),
            },
            ctx,
        );
        // Verify that we created a new or equivalent existing type.
        let mut self_ref = self_ptr.deref_mut(ctx);
        let self_ref = self_ref.downcast_mut::<StructType>().unwrap();
        assert!(self_ref.name.as_ref().unwrap() == name);
        if let Some(fields) = fields {
            if !self_ref.finalized {
                self_ref.fields = fields;
                self_ref.finalized = true;
            } else if self_ref.fields != fields {
                return Err(CompilerError::BadInput {
                    msg: format!("Struct {name} already exists and is different"),
                });
            }
        }
        Ok(self_ptr)
    }

    /// Get or create a new unnamed (anonymous) struct.
    /// These are finalized upon creation, and uniqued based on the fields.
    pub fn get_unnamed(ctx: &mut Context, fields: Vec<StructField>) -> Ptr<TypeObj> {
        Type::register_instance(
            StructType {
                name: None,
                fields,
                finalized: true,
            },
            ctx,
        )
    }

    /// Is this struct finalized? Returns false for non [StructType]s.
    pub fn is_finalized(ctx: &Context, ty: Ptr<TypeObj>) -> bool {
        ty.deref(ctx)
            .downcast_ref::<StructType>()
            .filter(|s| s.finalized)
            .is_some()
    }

    /// If a named struct already exists, get a pointer to it.
    pub fn get_existing_named(ctx: &Context, name: &str) -> Option<Ptr<TypeObj>> {
        Type::get_instance(
            StructType {
                name: Some(name.to_string()),
                /// Named structs are uniqued only on the name.
                fields: vec![],
                finalized: false,
            },
            ctx,
        )
    }

    /// If an unnamed struct already exists, get a pointer to it.
    pub fn get_existing_unnamed(ctx: &Context, fields: Vec<StructField>) -> Option<Ptr<TypeObj>> {
        Type::get_instance(
            StructType {
                name: None,
                fields,
                finalized: true,
            },
            ctx,
        )
    }
}

impl Verify for StructType {
    fn verify(&self, _ctx: &Context) -> Result<(), CompilerError> {
        if !self.finalized {
            return Err(CompilerError::VerificationError {
                msg: "Struct not finalized".to_string(),
            });
        }
        Ok(())
    }
}

impl Printable for StructType {
    fn fmt(
        &self,
        ctx: &Context,
        state: &printable::State,
        f: &mut core::fmt::Formatter<'_>,
    ) -> core::fmt::Result {
        write!(f, "{} <", Self::get_type_id_static().disp(ctx))?;
        use std::cell::RefCell;
        // Ugly, but also the simplest way to avoid infinite recursion.
        // MLIR does the same: see LLVMTypeSyntax::printStructType.
        thread_local! {
            // We use a vec instead of a HashMap hoping that this isn't
            // going to be large, in which case vec would be faster.
            static IN_PRINTING: RefCell<Vec<String>>  = RefCell::new(vec![]);
        }
        if let Some(name) = &self.name {
            let in_printing = IN_PRINTING.with(|f| f.borrow().contains(name));
            if in_printing {
                return write!(f, "{}>", name.clone());
            }
            IN_PRINTING.with(|f| f.borrow_mut().push(name.clone()));
            write!(f, "{name} ")?;
        }

        write!(
            f,
            "{{ {} }}",
            self.fields
                .iter()
                .iprint(ctx, state, printable::ListSeparator::SpacedChar(','))
        )?;

        // Done processing this struct. Remove it from the stack.
        if let Some(name) = &self.name {
            debug_assert!(IN_PRINTING.with(|f| f.borrow().last().unwrap() == name));
            IN_PRINTING.with(|f| f.borrow_mut().pop());
        }
        write!(f, ">")
    }
}

impl Hash for StructType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match &self.name {
            Some(name) => name.hash(state),
            None => self.fields.iter().for_each(
                |StructField {
                     field_name,
                     field_type,
                 }| {
                    field_name.hash(state);
                    field_type.hash(state);
                },
            ),
        }
    }
}

impl PartialEq for StructType {
    fn eq(&self, other: &Self) -> bool {
        match (&self.name, &other.name) {
            (Some(name), Some(other_name)) => name == other_name,
            (None, None) => {
                self.fields.len() == other.fields.len()
                    && self.fields.iter().zip(other.fields.iter()).all(|(f1, f2)| {
                        f1.field_name == f2.field_name && f1.field_type == f2.field_type
                    })
            }
            _ => false,
        }
    }
}

impl Parsable for StructType {
    type Parsed = Ptr<TypeObj>;

    fn parse<'a>(
        state_stream: &mut StateStream<'a>,
    ) -> ParseResult<Self::Parsed, easy::ParseError<StateStream<'a>>>
    where
        Self: Sized,
    {
        let body_parser = || {
            combine::parser(|parsable_state: &mut StateStream<'a>| {
                // Parse multiple type annotated fields separated by ','.
                let fields_parser = sep_by::<Vec<_>, _, _, _>(StructField::parser(), token(','));

                // The body is multiple type annotated fields surrounded by '{' and '}'.
                let mut body = between(spaced(token('{')), spaced(token('}')), fields_parser);

                // Finally parse the whole thing.
                body.parse_stream(parsable_state).into_result()
            })
        };

        let named = spaced(identifier())
            .and(optional(body_parser()))
            .map(|(name, body_opt)| (Some(name), body_opt));
        let anonymous = body_parser().map(|body| (None::<String>, Some(body)));

        // A struct type is named or anonymous.
        let mut struct_parser = between(
            spaced(token('<')),
            spaced(token('>')),
            (combine::position(), named.or(anonymous)),
        );

        struct_parser
            .parse_stream(state_stream)
            .and_then(|(position, (name_opt, body_opt))| {
                let ctx = &mut state_stream.state.ctx;
                if let Some(name) = name_opt {
                    to_parse_result(StructType::get_named(ctx, &name, body_opt), position)
                } else {
                    to_parse_result(
                        Ok(StructType::get_unnamed(
                            ctx,
                            body_opt.expect("Without a name, a struct type must have a body."),
                        )),
                        position,
                    )
                }
            })
    }
}

impl Eq for StructType {}

#[derive(Hash, PartialEq, Eq)]
pub struct PointerType {
    to: Ptr<TypeObj>,
}
impl_type!(PointerType, "ptr", "llvm");

impl PointerType {
    /// Get or create a new pointer type.
    pub fn get(ctx: &mut Context, to: Ptr<TypeObj>) -> Ptr<TypeObj> {
        Type::register_instance(PointerType { to }, ctx)
    }
    /// Get, if it already exists, a pointer type.
    pub fn get_existing(ctx: &Context, to: Ptr<TypeObj>) -> Option<Ptr<TypeObj>> {
        Type::get_instance(PointerType { to }, ctx)
    }

    /// Get the pointee type.
    pub fn get_pointee_type(&self) -> Ptr<TypeObj> {
        self.to
    }
}

impl Printable for PointerType {
    fn fmt(
        &self,
        ctx: &Context,
        _state: &printable::State,
        f: &mut core::fmt::Formatter<'_>,
    ) -> core::fmt::Result {
        write!(
            f,
            "{} <{}>",
            Self::get_type_id_static().disp(ctx),
            self.to.disp(ctx)
        )
    }
}

impl Parsable for PointerType {
    type Parsed = Ptr<TypeObj>;

    fn parse<'a>(
        state_stream: &mut StateStream<'a>,
    ) -> ParseResult<Self::Parsed, easy::ParseError<StateStream<'a>>>
    where
        Self: Sized,
    {
        spaced(combine::between(token('<'), token('>'), type_parser()))
            .parse_stream(state_stream)
            .map(|pointee_ty| PointerType::get(state_stream.state.ctx, pointee_ty))
    }
}

impl Verify for PointerType {
    fn verify(&self, _ctx: &Context) -> Result<(), CompilerError> {
        todo!()
    }
}

pub fn register(dialect: &mut Dialect) {
    StructType::register_type_in_dialect(dialect, StructType::parser_fn);
    PointerType::register_type_in_dialect(dialect, PointerType::parser_fn);
}

#[cfg(test)]
mod tests {

    use expect_test::expect;

    use crate::{
        common_traits::Verify,
        context::Context,
        dialects::{
            self,
            builtin::types::{IntegerType, Signedness},
            llvm::types::{PointerType, StructField, StructType},
        },
        error::CompilerError,
        parsable::{self, state_stream_from_iterator},
        printable::Printable,
        r#type::{type_parser, Type},
    };

    #[test]
    fn test_struct() -> Result<(), CompilerError> {
        let mut ctx = Context::new();
        let int64_ptr = IntegerType::get(&mut ctx, 64, Signedness::Signless);

        // Create an opaque struct since we want a recursive type.
        let list_struct = StructType::get_named(&mut ctx, "LinkedList", None)?;
        assert!(!StructType::is_finalized(&ctx, list_struct));
        let list_struct_ptr = PointerType::get(&mut ctx, list_struct);
        let fields = vec![
            StructField {
                field_name: "data".to_string(),
                field_type: int64_ptr,
            },
            StructField {
                field_name: "next".to_string(),
                field_type: list_struct_ptr,
            },
        ];
        // Finalize the type now.
        StructType::get_named(&mut ctx, "LinkedList", Some(fields))?;
        assert!(StructType::is_finalized(&ctx, list_struct));

        let list_struct_2 = StructType::get_existing_named(&ctx, "LinkedList").unwrap();
        assert!(list_struct == list_struct_2);
        assert!(StructType::get_existing_named(&ctx, "LinkedList2").is_none());

        assert_eq!(
            list_struct
                .deref(&ctx)
                .downcast_ref::<StructType>()
                .unwrap()
                .disp(&ctx)
                .to_string(),
            "llvm.struct <LinkedList { data: builtin.integer <i64>, next: llvm.ptr <llvm.struct <LinkedList>> }>"
        );

        let head_fields = vec![
            StructField {
                field_name: "len".to_string(),
                field_type: int64_ptr,
            },
            StructField {
                field_name: "first".to_string(),
                field_type: list_struct_ptr,
            },
        ];
        let head_struct = StructType::get_unnamed(&mut ctx, head_fields.clone());
        let head_struct2 = StructType::get_existing_unnamed(&ctx, head_fields).unwrap();
        assert!(head_struct == head_struct2);
        assert!(StructType::get_existing_unnamed(
            &ctx,
            vec![
                StructField {
                    field_name: "len".to_string(),
                    field_type: int64_ptr
                },
                // The actual field is a LinkedList here, rather than a pointer type to it.
                StructField {
                    field_name: "first".to_string(),
                    field_type: list_struct
                },
            ]
        )
        .is_none());

        Ok(())
    }

    #[test]
    fn test_pointer_types() {
        let mut ctx = Context::new();
        let int32_1_ptr = IntegerType::get(&mut ctx, 32, Signedness::Signed);
        let int64_ptr = IntegerType::get(&mut ctx, 64, Signedness::Signed);

        let int64pointer_ptr = PointerType { to: int64_ptr };
        let int64pointer_ptr = Type::register_instance(int64pointer_ptr, &mut ctx);
        assert_eq!(
            int64pointer_ptr.disp(&ctx).to_string(),
            "llvm.ptr <builtin.integer <si64>>"
        );
        assert!(int64pointer_ptr == PointerType::get(&mut ctx, int64_ptr));

        assert!(
            int64_ptr
                .deref(&ctx)
                .downcast_ref::<IntegerType>()
                .unwrap()
                .get_width()
                == 64
        );

        assert!(IntegerType::get_existing(&ctx, 32, Signedness::Signed).unwrap() == int32_1_ptr);
        assert!(PointerType::get_existing(&ctx, int64_ptr).unwrap() == int64pointer_ptr);
        assert!(
            int64pointer_ptr
                .deref(&ctx)
                .downcast_ref::<PointerType>()
                .unwrap()
                .get_pointee_type()
                == int64_ptr
        );
    }

    #[test]
    fn test_pointer_type_parsing() {
        let mut ctx = Context::new();
        dialects::builtin::register(&mut ctx);
        dialects::llvm::register(&mut ctx);

        let state_stream = state_stream_from_iterator(
            "llvm.ptr <builtin.integer <si64>>".chars(),
            parsable::State { ctx: &mut ctx },
        );

        let res = type_parser().parse(state_stream).unwrap().0;
        assert_eq!(
            &res.disp(&ctx).to_string(),
            "llvm.ptr <builtin.integer <si64>>"
        );
    }

    #[test]
    fn test_struct_type_parsing() {
        let mut ctx = Context::new();
        dialects::builtin::register(&mut ctx);
        dialects::llvm::register(&mut ctx);

        let state_stream = state_stream_from_iterator(
            "llvm.struct <LinkedList { data: builtin.integer <i64>, next: llvm.ptr <llvm.struct <LinkedList>> }>".chars(),
            parsable::State { ctx: &mut ctx },
        );

        let res = type_parser().parse(state_stream).unwrap().0;
        assert_eq!(&res.disp(&ctx).to_string(), "llvm.struct <LinkedList { data: builtin.integer <i64>, next: llvm.ptr <llvm.struct <LinkedList>> }>");
    }

    #[test]
    fn test_struct_type_errs() {
        let mut ctx = Context::new();
        dialects::builtin::register(&mut ctx);
        dialects::llvm::register(&mut ctx);

        let state_stream = state_stream_from_iterator(
            "llvm.struct < My1 { f1: builtin.integer<i8> } >".chars(),
            parsable::State { ctx: &mut ctx },
        );
        let _ = type_parser().parse(state_stream).unwrap().0;

        let state_stream = state_stream_from_iterator(
            "llvm.struct < My1 { f1: builtin.integer<i16> } >".chars(),
            parsable::State { ctx: &mut ctx },
        );

        let res = type_parser().parse(state_stream);
        let err_msg = format!("{}", res.err().unwrap());

        let expected_err_msg = expect![[r#"
            Parse error at line: 1, column: 15
            Compilation failed.
            Struct My1 already exists and is different
        "#]];
        expected_err_msg.assert_eq(&err_msg);

        let state_stream = state_stream_from_iterator(
            "llvm.struct < My2 >".chars(),
            parsable::State { ctx: &mut ctx },
        );
        let res = type_parser().parse(state_stream).unwrap().0;
        let expected_err_msg = expect![[r#"
            Internal compiler error. Verification failed.
            Struct not finalized"#]];
        expected_err_msg.assert_eq(&res.verify(&ctx).unwrap_err().to_string())
    }
}
