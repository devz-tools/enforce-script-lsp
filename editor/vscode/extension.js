const vscode = require('vscode');
const { LanguageClient, TransportKind } = require('vscode-languageclient/node');

let client;

async function activate(context) {
    const config = vscode.workspace.getConfiguration('enforceScriptLsp');
    const serverPath = config.get('serverPath');
    if (!serverPath) {
        vscode.window.showErrorMessage('Set enforceScriptLsp.serverPath to the Enforce Script LSP executable.');
        return;
    }

    // Extra source roots (vanilla/Expansion) passed to the server as
    // initializationOptions.dependencyRoots.
    const dependencyRoots = config.get('dependencyRoots') || [];

    client = new LanguageClient(
        'enforceScriptLspLocal',
        'Enforce Script LSP (Local)',
        { run: { command: serverPath, transport: TransportKind.stdio },
          debug: { command: serverPath, transport: TransportKind.stdio } },
        { documentSelector: [{ scheme: 'file', language: 'enforcescript' }],
          outputChannelName: 'Enforce Script LSP (Local)',
          initializationOptions: { dependencyRoots } }
    );
    context.subscriptions.push(client);
    await client.start();
}

async function deactivate() {
    if (client) await client.stop();
}

module.exports = { activate, deactivate };
