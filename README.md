# rjsonnet

An implementation of [Jsonnet][] in [Rust][], which aims to conform to the [spec][], and is built with the IDE use-case in mind.

The primary intended way to use this is via the [language server][lsp], but there is also a CLI that can translate Jsonnet files into JSON.

[Rust]: https://www.rust-lang.org
[Jsonnet]: https://jsonnet.org
[spec]: https://jsonnet.org/ref/spec.html
[lsp]: https://microsoft.github.io/language-server-protocol/
