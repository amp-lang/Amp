//! The semantic analysis phase.
//!
//! This phase lowers Amp modules into AIR (Amp intermediate representation) which stores type
//! information.
//!
//! # Passes
//! Currently, `import`/`using` statements and `static` variables aren't implemented, so there
//! aren't as many passes now as there will be in the future.
//!
//! 1. Declare the names of root `const` bindings, ensuring there are no duplicate names.
//!    Disregard type/value information at this phase, as it's not important.
//! 2. Evaluate the types and values as of global `const` bindings.

pub mod scope;

use slot_arena::SlotArena;

use crate::{
    codemap::{Span, Spanned},
    diag::SemaDiagnostics,
    syntax::ast,
    Context,
};

use self::scope::{Scope, ScopedRef};

/// The state of the semantic analysis unit.
///
/// A unit dissociates declarations from their modules, merging them all together.
#[derive(Debug)]
pub struct Unit {
    /// The constants declared in this [Unit].
    pub consts: SlotArena<Const>,
}

impl Unit {
    /// Creates an empty [Unit].
    #[inline]
    pub fn new() -> Self {
        Self {
            consts: SlotArena::new(),
        }
    }

    /// Returns the span of the provided item, if any.
    pub fn span_of_scoped_ref(&self, item: ScopedRef) -> Option<Span> {
        match item {
            ScopedRef::Const(item) => self.consts.get(item).span,
        }
    }

    /// Analyzes the provided module, lowering it to AIR.
    pub fn analyze(&mut self, cx: &mut Context, source: ast::Stmnts) -> Result<(), ()> {
        let global_scope = Scope::new();

        let mut module = Module::new(source);
        module.scope_mut().parent(Some(&global_scope));

        {
            module.declare_root_const_names(cx, self)?;
            module
        };

        Ok(())
    }
}

/// A module of Amp source, to be lowered into AIR.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Module<'root> {
    /// The source AST of the module.
    source: ast::Stmnts,

    /// The scope of items in this module.
    scope: Scope<'root>,
}

impl<'root> Module<'root> {
    /// Creates a new [Module] from the provided AST statements.
    #[inline]
    pub fn new(source: ast::Stmnts) -> Self {
        Self {
            source,
            scope: Scope::new(),
        }
    }

    /// Returns the scope of this [Module].
    #[inline]
    pub fn scope(&self) -> &Scope {
        &self.scope
    }

    /// Returns the scope of this [Module].
    #[inline]
    pub fn scope_mut(&mut self) -> &mut Scope<'root> {
        &mut self.scope
    }

    /// Declares the root `const` binding names in this module.  Also checks if there are any
    /// invalid items declared at the root of this module.
    pub fn declare_root_const_names(
        &mut self,
        cx: &mut Context,
        unit: &mut Unit,
    ) -> Result<(), ()> {
        let mut res = Ok(());

        for item in &self.source.stmnts {
            match item {
                ast::Expr::Const(decl) => {
                    if let Some(item) = self.scope.get(&decl.name.value) {
                        cx.duplicate_declaration(
                            &decl.name.value,
                            decl.span(),
                            unit.span_of_scoped_ref(item),
                        );
                        res = Err(());
                    } else {
                        let id = unit.consts.insert(Const {
                            span: Some(decl.span()),
                            name: decl.name.value.clone(),
                        });
                        self.scope.insert(&decl.name.value, ScopedRef::Const(id));
                    }
                }
                _ => todo!("throw error when invalid items are found"),
            }
        }

        res
    }
}

/// An intermediate `const` binding before it's translated to AIR.
#[derive(Debug)]
pub struct Const {
    /// The span of the entire `const` declaration, if available.  Made optional because intrinsic
    /// functions may not have span information.
    pub span: Option<Span>,

    /// The name of the declaration.
    pub name: String,
}
