# Manual

## Config

You can configure the extension with some options.

**The config options are unstable and subject to change.**

### VS Code

Edit `.vscode/settings.json` or use the built-in settings editor for the following options.

<!-- @begin vscode-config -->

#### `rjsonnet.server.enable`

- Type: `boolean`
- Default: `true`

Enable the language server.

#### `rjsonnet.server.importDirs.extra`

- Type: `array`
- Default: `[]`

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
