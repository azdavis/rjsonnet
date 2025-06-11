# Manual

## CLI

rjsonnet is usually used via the language server extension, but there is also a CLI.

The CLI takes in a list of filenames, and outputs warnings and errors (if any).

### Removing unused variables

The CLI can also remove unused `local` variables with the `--rm-unused <flavor>` option. `<flavor>` can be `all` or `imports`. When `all`, it will remove all; when `imports`, it will only remove `local`s whose RHS is an `import`, `importstr`, or `importbin` expression.

There is also the `--rm-unused-comments` option to modify how comments nearby unused locals are treated. This may be `none` (default), `all`, `above`, or `below`. 'Nearby' means there are no intervening blank lines between the `local` and the comment.

## Config

You can configure the extension with some options.

**The config options are unstable and subject to change.**

### VS Code

Edit `.vscode/settings.json` or use the built-in settings editor for the following options.

<!-- @begin vscode-config -->

#### `rjsonnet.format.engine`

- Type: `string`
- Default: `"none"`
- Valid values:
  - `"none"`: No formatting.
  - `"bin-jsonnetfmt-stdio"`: Formatting provided by an executable that:
    - Is named `bin/jsonnetfmt` relative to the workspace root
    - Accepts a `-stdio` flag
    - Accepts a positional argument, the name of the file being formatted
    - Has its working directory set to the workspace root
    - Takes in from stdin the unformatted file contents
    - Outputs to stdout the formatted file contents

How to format files.

#### `rjsonnet.server.enable`

- Type: `boolean`
- Default: `true`

Enable the language server.

#### `rjsonnet.server.importDirs.extra`

- Type: `array`
- Default: `[".", "bazel-bin"]`

Extra directories in which to search for import paths.

#### `rjsonnet.server.manifest.enabled`

- Type: `boolean`
- Default: `false`

Whether to manifest into JSON.

Might be slow when enabled.

#### `rjsonnet.server.path`

- Type: `string`
- Default: `""`

Path to the `jsonnet-ls` executable.

When set to the empty string `""` (the default), use the path to the one that's pre-built and bundled with the extension.

#### `rjsonnet.unstable.server.logFilter`

- Type: `string`
- Default: `""`

**Note:** This is intended for debugging the language server itself. It is not intended for normal use. It may change without notice.

The log filter. Set to e.g. 'info' or 'debug' for more info printed to the output console.

<!-- @end vscode-config -->
