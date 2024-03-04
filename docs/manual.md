# Manual

## Config

You can configure the extension with some options.

**The config options are unstable and subject to change.**

### VS Code

Edit `.vscode/settings.json` or use the built-in settings editor for the following options.

<!-- @begin vscode-config -->

#### `rjsonnet.server.diagnostics.maxPerFile`

- Type: `number`
- Default: `5`

Max number of diagnostics to show per file.

#### `rjsonnet.server.diagnostics.show`

- Type: `string`
- Default: `"open"`
- Valid values:
  - `"all"`: Show all diagnostics.
  - `"open"`: Show diagnostics only on open files.
  - `"none"`: Show no diagnostics.

How to show diagnostics.

#### `rjsonnet.server.enable`

- Type: `boolean`
- Default: `true`

Enable the language server.

#### `rjsonnet.server.importDirs.extra`

- Type: `array`
- Default: `[]`

Extra directories in which to search for import paths.

#### `rjsonnet.server.manifest.enable`

- Type: `boolean`
- Default: `false`

Manifest into JSON.

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
