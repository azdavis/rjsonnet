import * as path from "path";
import * as vscode from "vscode";
import {
  LanguageClient,
  type LanguageClientOptions,
  type ServerOptions,
} from "vscode-languageclient/node";

let client: LanguageClient | null = null;

export async function activate(cx: vscode.ExtensionContext) {
  if (client !== null) {
    return;
  }
  const config = vscode.workspace.getConfiguration("rjsonnet");
  if (!config.get("server.enable")) {
    return;
  }
  const ext = process.platform === "win32" ? ".exe" : "";
  const configPath = config.get("server.path");
  const serverOpts: ServerOptions = {
    command:
      typeof configPath === "string" && configPath.length !== 0
        ? configPath
        : cx.asAbsolutePath(path.join("out", "jsonnet-ls" + ext)),
  };
  const clientOpts: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "jsonnet" }],
    initializationOptions: {
      root_dirs: config.get("server.importDirs.extra"),
      log_filter: config.get("unstable.server.logFilter"),
      manifest: config.get("server.manifest.enabled"),
      format_engine: config.get("format.engine"),
    },
  };
  client = new LanguageClient("rjsonnet", serverOpts, clientOpts);
  await client.start();
}

export async function deactivate() {
  if (client === null) {
    return;
  }
  await client.stop();
  client = null;
}
