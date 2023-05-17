//! Scoping to resolve names.

use std::collections::HashMap;

use slot_arena::Ref;

use super::Const;

/// A value in a [Scope].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum ScopedRef {
    /// A reference to a `const` declaration.
    Const(Ref<Const>),
}

/// A scope to resolve names.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Scope<'p> {
    parent: Option<&'p Scope<'p>>,
    items: HashMap<String, ScopedRef>,
}

impl<'p> Scope<'p> {
    /// Creates an empty [Scope].
    #[inline]
    pub fn new() -> Self {
        Self {
            parent: None,
            items: HashMap::new(),
        }
    }

    /// Sets the parent of this [Scope].
    #[inline]
    pub fn parent(&mut self, parent: Option<&'p Scope<'p>>) {
        self.parent = parent;
    }

    /// Creates a descendant of this [Scope].
    #[inline]
    pub fn subscope(&'p self) -> Self {
        Self {
            parent: Some(self),
            items: HashMap::new(),
        }
    }

    /// Inserts a value into this [Scope].
    #[inline]
    pub fn insert(&mut self, name: impl Into<String>, value: ScopedRef) {
        self.items.insert(name.into(), value);
    }

    /// Searches for a name in this scope.  If the name cannot be found in this subscope it
    /// recursively searches its parents for a match.
    pub fn find(&self, name: &str) -> Option<ScopedRef> {
        if let Some(item) = self.items.get(name) {
            Some(*item)
        } else if let Some(parent) = self.parent {
            parent.find(name)
        } else {
            None
        }
    }

    /// Gets the value with the provided name if it is in this scope.  Does not search parent
    /// scopes.
    #[inline]
    pub fn get(&self, name: &str) -> Option<ScopedRef> {
        self.items.get(name).copied()
    }
}
