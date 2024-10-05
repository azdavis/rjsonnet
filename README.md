# rjsonnet

An implementation of [Jsonnet][] in [Rust][], which aims to conform to the [spec][], and is built with the IDE use-case in mind.

## Install

- [VS Code Marketplace][vs-code-marketplace]
- [Open VSX][open-vsx]

## Features

- Inline diagnostics
- Go-to-definition
- Hover for info
- Type-checking
- Auto-completions

## Demo

https://github.com/azdavis/rjsonnet/assets/6496454/8e2f5e46-f0ed-48b5-8811-ab05d00d4fe8

## Ways to use

### VS Code

The primary intended way to use this is via the [language server][lsp] and its official VS Code extension.

### Language server

You can also use the language server with any editor that supports language servers. You'll just have to write your own glue code between this server and your editor.

## Try it

### First time repo setup

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

### Language server + VS Code extension

1. Get [VS Code][vscode].
1. Get [Node][node].
1. Open the repo in VS Code.
1. Click "Run and Debug" (by default, the triangular play button with bug on the left).
1. In the "Run and Debug" sidebar that appears, click the triangular play button by "extension" (you may need to select it from the drop-down). Another VS Code will pop up, with the extension enabled.
1. Open that VS Code on a folder with Jsonnet files.
1. Ensure other Jsonnet-related extensions like [Jsonnet LSP][carl] are disabled.
1. Try out the IDE features like inline diagnostics and go-to-def.

## Credits

I took a lot of inspiration from the existing [Jsonnet LSP][carl] extension, but I wanted to try doing it myself, in Rust, from scratch, specifically for IDEs. I had already done so with Standard ML in [Millet][millet].

## Alternatives

- [Jsonnet LSP][carl], by Carl Verge
- [Jsonnet Language Server][grafana], by Grafana

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
