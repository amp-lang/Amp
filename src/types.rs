use crate::{
    sema::{scope::Scope, Unit, IntermediateExpr},
    syntax::ast,
    Context, value::Value,
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
        self.0 >= other.0 && self.is_equivalent(other)
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
    /// Argument order matters, for example, `func(&mut T)` is equivalent to `func(&T)`, but
    /// `func(&T)` is not equivalent to `func(&mut T)`.
    pub fn is_equivalent(&self, other: &FuncSig) -> bool {
        if other.params.len() != self.params.len() {
            false
        } else {
            self.params
                .iter()
                .zip(other.params.iter())
                .all(|(left, right)| left.is_equivalent(right))
                && self.returns.is_equivalent(&other.returns)
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
                ast::FuncParam::Anon(arg) => Type::from_ast(cx, unit, scope, &arg)?,
                ast::FuncParam::Named(ast::NamedParam { ty, .. }) => Type::from_ast(cx, unit, scope, &ty.ty)?,
            });
        }

        Ok(Self {
            params,
            returns: Type::from_ast(cx, unit, scope, &expr.returns.ty)?,
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
        let Value::Type(final_ty) = Value::eval(
            IntermediateExpr::verify(cx, unit, scope, expr)?
                // verify that the value is a type
                .coerce(&Type::Type)
                .expect("TODO: report non-type in type position"),
        )
        .expect("TODO: report non-constant type")
        else { 
            unreachable!("value should be of type `type` as verified above")
        };

        Ok(final_ty)
    }
}
