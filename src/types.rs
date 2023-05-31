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

/// The type of a value in Amp.
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Type,
    U8,
    I32,
    ThinPtr(Box<ThinPtr>),
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
}
