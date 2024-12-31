# Architecture

This is a high-level [architecture][] of the project.

The project is mostly implemented in Rust, so it is split into "crates", Rust's name for software package.

## Types crates

These define foundational types used by various other crates.

### `crates/jsonnet-syntax`

Concrete syntax trees (CST) for Jsonnet, able to represent any Jsonnet file (even one with syntax errors) losslessly. This means you can reconstruct the exact file, including whitespace, comments, and syntax errors, from a CST.

The CST types also have a high-level AST API.

### `crates/jsonnet-expr`

Expressions (and strings), allocated in arenas instead of with individual pointer types like `Box`.

For strings, the arena IDs are identical if the strings are identical. This is used to reduce memory usage, so-called string interning.

For expressions, not so. This means syntactically identical expressions at different places in a given file each have their own "identity". This is prudent, since each expression may have different semantics depending on the surrounding environment.

### `crates/jsonnet-ty`

A type system for Jsonnet.

### `crates/jsonnet-val`

Jsonnet (and JSON) values.

## Stages crates

These are the separate stages of analysis.

Each stage produces an output that is as "correct" as we could manage **as well as** errors, instead of **either** an error-less output **or** errors. This lets us continue to analyze malformed inputs as best we can in further stages.

### `crates/jsonnet-lex`

Lex a string into a sequence tokens.

### `crates/jsonnet-parse`

Parse a sequence of tokens into a concrete syntax tree.

### `crates/jsonnet-desugar`

Transform a concrete syntax tree into a more abstract form, removing things like whitespace and parentheses for operator precedence. Also simplifies complex constructs into more fundamental ones.

### `crates/jsonnet-statics`

Statically analyze an expression, checking for things like if every variable usage site has a corresponding definition site in scope.

### `crates/jsonnet-eval`

Evaluate jsonnet.

## Helper crates

These implement Jsonnet-specific things, but aren't really "big" enough to be a full stage.

### `crates/jsonnet-ident`

Some helpers concerning identifiers.

### `crates/jsonnet-escape`

Handles string escape sequences, like `\n` and `\r`.

### `crates/jsonnet-ast-escape`

A thin wrapper over jsonnet-escape, integrating it with the types from jsonnet-syntax.

### `crates/jsonnet-resolve-import`

A resolver for import paths.

### `crates/jsonnet-std-sig`

Rust code describing the standard library functions.

### `crates/jsonnet-token`

Rust code describing the tokens.

## Overall crates

These tie everything else together.

### `crates/jsonnet-analyze`

Depends on all the "stages" to create an overall high-level API for fully analyzing an entire Jsonnet project.

### `crates/jsonnet-ls`

The language server binary. Basically just mashes `jsonnet-analyze` and `lang-srv` together.

### `crates/tests`

The end-to-end tests. Lexing, parsing, desugaring, static analysis, evaluation, and manifestation are all tested here, with full Jsonnet programs as input.

## Generic crates

These crates could possibly be pulled out from this repo and be reused more widely.

### `crates/always`

Like `assert!()` except it only panics in debug mode. Evaluates to the asserted condition otherwise.

This is handy if you don't want to hard fail in production:

```rs
if always!(internal_invariant_satisfied()) {
    // normal, good
} else {
    // oops, something that should "never happen" happened.
    // the internal invariant was NOT satisfied.
    // in this branch we can do something like return a fake value,
    // or `continue` in a loop, or return `None`, etc.
}
```

### `crates/diagnostic`

Common types for diagnostics (aka "errors") for programming language implementations.

### `crates/finite-float`

A finite float type (not nan or infinite).

### `crates/lang-srv`

A generic language server scaffold.

### `crates/lang-srv-state`

A trait for the state of a language server.

## Other code

Most of the code is contained in the Rust crates documented above, but some code lives elsewhere.

### `docs`

Documentation, like this!

### `img`

Images for the README.

### `editors`

Support for specific text editors via language client extensions/"glue code".

### `editors/vscode`

The VS Code client extension, in [TypeScript][ts].

### `xtask`

A [task runner][xtask] for the repo.

Allows you to invoke `cargo xtask <task>` to run the `<task>`.

### `.cargo`

Configuration for Cargo, Rust's package manager and build tool. Allows `cargo xtask` to work.

### `Cargo.toml`

At the top level, this sets up a "Cargo workspace" of all the crates in `crates` and `xtask`. Each crate also has its own `Cargo.toml` defining fundamental things about the crate like its name and dependencies.

### `Cargo.lock`

A lockfile for Cargo that records the exact versions of all dependencies that Cargo resolved.

### `rustfmt.toml`

Configuration for rustfmt, Rust's automatic code formatter. Before checking code into git, it must be formatted by rustfmt.

### `clippy.toml`

Configuration for clippy, Rust's linter.

### `.github`

Configuration for GitHub, like:

- PR templates
- Issue templates
- How to run CI

### `.gitignore`

Configuration for git, to ignore generated files like `/target` (the output folder for Rust).

### `.vscode`

Configuration for VS Code, like:

- How to launch an instance of VS Code with a local version of the extension for testing
- What files to ignore in the sidebar (e.g. `/target`)

[architecture]: https://matklad.github.io/2021/02/06/ARCHITECTURE.md.html
[ts]: https://www.typescriptlang.org
[xtask]: https://github.com/matklad/cargo-xtask
