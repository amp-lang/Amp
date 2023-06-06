# Changelog

## [Unreleased Changes]
- [Parse command line arguments](https://github.com/amp-lang/ampc/issues/11)
- [Disable returning types from functions entirely](https://github.com/amp-lang/ampc/issues/10)
- [Ensure `return` statement has value](https://github.com/amp-lang/ampc/issues/12)

### [`snapshot-05-06-2023-a`](https://github.com/amp-lang/ampc/releases/tag/snapshot-05-06-2023-a)
- [Report diagnostics in `Type::from_ast`](https://github.com/amp-lang/ampc/issues/5) (report invalid types)
- Report type mismatches in `const` declarations
- [Implement using function calls as values](https://github.com/amp-lang/ampc/issues/8)
- [Implement implicit returns](https://github.com/amp-lang/ampc/issues/6)
- Report diagnostic instead of panicking for mismatched return types
- Report diagnostic for function argument number length mismatches
- Report diagnostic for attempting to call non-function values
- [Report `const` declarations with values not known at compile time](https://github.com/amp-lang/ampc/issues/9)
- Report diagnostic for mismatched function arguments
- Report diagnostics for invalid statements and expressions

### [`snapshot-02-06-2023-a`](https://github.com/amp-lang/ampc/releases/tag/snapshot-02-06-2023-a)
(initial snapshot)