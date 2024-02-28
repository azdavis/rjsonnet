# rjsonnet

An implementation of [Jsonnet][] in [Rust][], which aims to conform to the [spec][], and is built with the IDE use-case in mind.

The primary intended way to use this is via the [language server][lsp] and its official VS Code extension.

However, you can also use the language server with any supported editor, you'll just have to write your own glue code between this server and your editor.

There is also a CLI that can translate Jsonnet files into JSON.

## Warning

This project is pre-MVP. Most IDE features are not implemented. Some **language** features haven't even been implemented. There are a lot of TODOs and probably a good amount of latent bugs.

## Features

- Inline diagnostics
- Go-to-definition
- Hover for info

## Try it

### First time repo setup

1. Get [Rust][rustup].
1. Clone or download the repo.
1. Enter the repo and run `cargo xtask ci`.

### Try the CLI only

Run:

```
$ cargo run --bin jsonnet-cli -- --manifest FOO.jsonnet
```

to turn `FOO.jsonnet` into JSON and report any errors.

### Try the language server

- Get [VS Code][vscode].
- Get [Node][node].
- Open the repo in VS Code.
- Click "Run and Debug" (by default, the triangular play button with bug on the left).
- Click the triangular play button by "extension" (you may need to select it from the drop-down). Another VS Code will pop up, with the extension enabled.
- Open that VS Code on a folder with Jsonnet files.
- Ensure other Jsonnet-related extensions like [Jsonnet LSP][other-ext] are disabled.
- Try out the IDE features like inline diagnostics and go-to-def.

## Credits

I took a lot of inspiration from the existing [Jsonnet LSP][other-ext] extension, but I wanted to try doing it myself, in Rust, from scratch, specifically for IDEs. I had already done so with Standard ML in [Millet][millet].

## License

Like Rust itself and many other projects in the Rust community, this project is licensed under either the MIT license or the Apache license v2.0, at your option.

[Jsonnet]: https://jsonnet.org
[lsp]: https://microsoft.github.io/language-server-protocol
[node]: https://nodejs.org/en
[other-ext]: https://marketplace.visualstudio.com/items?itemName=cverge.jsonnet-lsp
[Rust]: https://www.rust-lang.org
[rustup]: https://rustup.rs
[spec]: https://jsonnet.org/ref/spec.html
[vscode]: https://code.visualstudio.com
[millet]: https://github.com/azdavis/millet
