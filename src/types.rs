use crate::{
    codemap::{Span, Spanned},
    diag::SemaDiagnostics,
    sema::{scope::Scope, IntermediateExpr, Unit},
    syntax::ast,
    value::Value,
    Context,
};

/// The mutability of a pointer.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
#[repr(u8)]
pub enum Mutable {
    Yes = 1,
    No = 0,
}

/// A thin pointer [Type] (a pointer with no runtime size information).
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ThinPtr(pub Mutable, pub Type);

impl ThinPtr {
    /// Returns `true` if this [ThinPtr] type is equivalent to the provided [ThinPtr] type.
    ///
    /// Argument order matters, for example, `&mut T` is equivalent to `&T`, but `&T` is not
    /// equivalent to `&mut T`.
    #[inline]
    pub fn is_equivalent(&self, other: &ThinPtr) -> bool {
        self.0 >= other.0 && self.1.is_equivalent(&other.1)
    }

    /// Returns the name of the [ThinPtr] type.
    pub fn name(&self) -> String {
        format!(
            "&{}{}",
            if self.0 == Mutable::Yes { "mut " } else { "" },
            self.1.name()
        )
    }
}

/// The signature/type of a function.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct FuncSig {
    pub params: Vec<Type>,
    pub returns: Type,
}

impl FuncSig {
    /// Returns `true` if this [FuncSig] type is equivalent to the provided [FuncSig] type.
    ///
    /// Argument order matters, for example,  `func(T, ...)` is equivalent to `func(T)`, but not
    /// the other way around.
    pub fn is_equivalent(&self, other: &FuncSig) -> bool {
        if other.params.len() != self.params.len() {
            false
        } else {
            self.params
                .iter()
                .zip(other.params.iter())
                .all(|(left, right)| left == right)
                && self.returns.is_equivalent(&other.returns)
        }
    }

    /// Returns the name of this type.
    pub fn name(&self) -> String {
        format!(
            "func({}): {}",
            self.params
                .iter()
                .map(|item| item.name())
                .collect::<Vec<_>>()
                .join(", "),
            self.returns.name()
        )
    }

    /// Reports a diagnostic if the provided type cannot be used as a function argument.
    fn check_arg_validity(cx: &mut Context, span: Span, arg: Type) -> Result<Type, ()> {
        match arg {
            Type::Type => {
                cx.cannot_use_type_as_argument(span);
                Err(())
            }
            arg => Ok(arg),
        }
    }

    /// Reports a diagnostic if the provided type cannot be used as a return type for a function.
    fn check_return_validity(cx: &mut Context, span: Span, arg: Type) -> Result<Type, ()> {
        match arg {
            Type::Type => {
                cx.cannot_use_type_as_return(span);
                Err(())
            }
            arg => Ok(arg),
        }
    }

    /// Attempts to get the signature of a function from an AST expression.
    pub fn from_ast(
        cx: &mut Context,
        unit: &mut Unit,
        scope: &Scope,
        expr: &ast::Func,
    ) -> Result<Self, ()> {
        let mut params = Vec::new();

        for arg in &expr.args.items {
            params.push(match arg {
                ast::FuncParam::Anon(arg) => {
                    let ty = Type::from_ast(cx, unit, scope, &arg)?;
                    Self::check_arg_validity(cx, arg.span(), ty)?
                }
                ast::FuncParam::Named(ast::NamedParam { ty, .. }) => {
                    let ty = Type::from_ast(cx, unit, scope, &ty.ty)?;
                    Self::check_arg_validity(cx, arg.span(), ty)?
                }
            });
        }

        Ok(Self {
            params,
            returns: {
                let ty = Type::from_ast(cx, unit, scope, &expr.returns.ty)?;
                Self::check_return_validity(cx, expr.returns.ty.span(), ty)?
            },
        })
    }
}

/// The type of a value in Amp.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Type,
    U8,
    I32,
    ThinPtr(Box<ThinPtr>),
    Func(Box<FuncSig>),
}

impl Type {
    /// Creates a new [Type::ThinPtr] with the provided configuration.  Assumes that the provided
    /// type is sized.
    #[inline]
    pub fn thin_ptr(mutable: Mutable, ty: Type) -> Self {
        Self::ThinPtr(Box::new(ThinPtr(mutable, ty)))
    }

    /// Returns the name of this [Type].
    pub fn name(&self) -> String {
        match self {
            Self::Type => "type".to_string(),
            Self::U8 => "u8".to_string(),
            Self::I32 => "i32".to_string(),
            Self::ThinPtr(ty) => ty.name(),
            Self::Func(ty) => ty.name(),
        }
    }

    /// Returns `true` if this type is equivalent to another type.
    ///
    /// Argument order matters, for example, `&mut T` is equivalent to `&T`, but `&T` is not
    /// equivalent to `&mut T`.
    pub fn is_equivalent(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Type, Type::Type) => true,
            (Type::U8, Type::U8) => true,
            (Type::I32, Type::I32) => true,
            (Type::ThinPtr(left), Type::ThinPtr(right)) => left.is_equivalent(right),
            (Type::Func(left), Type::Func(right)) => left.is_equivalent(right),
            _ => false,
        }
    }

    /// Returns `true` if this type is an integer type.
    pub fn is_int(&self) -> bool {
        match self {
            Type::U8 | Type::I32 => true,
            _ => false,
        }
    }

    /// Attempts to resolve a constant type value from the provided expression.
    pub fn from_ast(
        cx: &mut Context,
        unit: &mut Unit,
        scope: &Scope,
        expr: &ast::Expr,
    ) -> Result<Self, ()> {
        let Value::Type(final_ty) = Value::eval({
            let value = IntermediateExpr::verify(cx, unit, scope, expr)?;

            value.clone()
            // verify that the value is a type
            .coerce(&Type::Type)
            .ok_or_else(|| cx.invalid_type_in_type_position(
                &value.default_type().expect("must have a type").name(),
                expr.span()
            ))?
        })
        .expect("constant evaluation cannot fail as types are always constant")
        else {
            unreachable!("value should be of type `type` as verified above")
        };

        Ok(final_ty)
    }
}
