# rjsonnet

An implementation of [Jsonnet][] in [Rust][], which aims to conform to the [spec][], and is built with the IDE use-case in mind.

## Install

- [VS Code Marketplace][vs-code-marketplace]
- [Open VSX][open-vsx]

## Screenshots

### Syntax highlighting, error tolerant parsing, and inline diagnostics

![syntax-hl-err-tolerant-diagnostics](/img/1-syntax-hl-err-tolerant-diagnostics.png)

### Go to (or peek) definition

![peek-def](/img/2-peek-def.png)

### Hover for type, standard library docs, and typechecking

![std-lib](/img/3-std-lib-doc-hover.png)

### Auto-complete object fields

![auto-complete](/img/4-auto-complete.png)

### Type annotations and signature help

![sig-help](/img/5-sig-help.png)

### Flow typing

![flow typing](/img/6-flow-typing.png)

## Ways to use

### VS Code

The primary intended way to use this is via the [language server][lsp] and its [official VS Code extension][vs-code-marketplace].

### Language server

You can also use the language server with any editor that supports language servers. You'll just have to write your own glue code between this server and your editor.

## Building from source

1. Get [Rust][rustup].
1. Clone or download the repo.
1. Enter the repo.

Now you can run things like:

```
$ cargo build
$ cargo test
$ cargo clippy
$ cargo fmt
$ cargo xtask ci
```

You can also build the VS Code extension:

1. Get [VS Code][vscode].
1. Get [Node][node].
1. Ensure other Jsonnet-related extensions like [Jsonnet LSP][carl] are disabled.
1. Open the repo in VS Code.
1. Click "Run and Debug" (by default, the triangular play button with bug on the left).
1. In the "Run and Debug" sidebar that appears, click the triangular play button by "extension" (you may need to select it from the drop-down). Another VS Code will pop up, with the extension enabled.
1. Open that VS Code on a folder with Jsonnet files.
1. Try out the IDE features like inline diagnostics and go-to-def.

## Credits

I took a lot of inspiration from the existing [Jsonnet LSP][carl] extension, but I wanted to try doing it myself, in Rust, from scratch, specifically for IDEs. I had already done so with Standard ML in [Millet][millet].

## Alternatives

### Jsonnet language servers

Some prior art:

- [Jsonnet LSP][carl], by Carl Verge
- [Jsonnet Language Server][grafana], by Grafana

These implementations are based off of the [Go implementation of Jsonnet][gojsonnet].

### Rust implementations of Jsonnet

- [jrsonnet](https://github.com/CertainLach/jrsonnet), by CertainLach
- [rsjsonnet](https://github.com/eduardosm/rsjsonnet), by eduardosm

Confusingly, the Python bindings for the former are called [rjsonnet](https://pypi.org/project/rjsonnet/).

These implementations are more targeted at implementing the Jsonnet language for evaluating/manifesting it to JSON. This project has some evaluation functionality implemented, but it is mainly intended to be a language server. This makes evaluation of Jsonnet into JSON less of a project goal.

## License

Like Rust itself and many other projects in the Rust community, this project is licensed under either the MIT license or the Apache license v2.0, at your option.

[Jsonnet]: https://jsonnet.org
[lsp]: https://microsoft.github.io/language-server-protocol
[node]: https://nodejs.org/en
[carl]: https://marketplace.visualstudio.com/items?itemName=cverge.jsonnet-lsp
[grafana]: https://marketplace.visualstudio.com/items?itemName=Grafana.vscode-jsonnet
[Rust]: https://www.rust-lang.org
[rustup]: https://rustup.rs
[spec]: https://jsonnet.org/ref/spec.html
[vscode]: https://code.visualstudio.com
[millet]: https://github.com/azdavis/millet
[vs-code-marketplace]: https://marketplace.visualstudio.com/items?itemName=azdavis.rjsonnet
[open-vsx]: https://open-vsx.org/extension/azdavis/rjsonnet
[gojsonnet]: https://github.com/google/go-jsonnet
