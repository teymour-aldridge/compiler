'use strict';

import { workspace, ExtensionContext } from 'vscode';
import { LanguageClient, LanguageClientOptions, ServerOptions } from 'vscode-languageclient';
import { Trace } from 'vscode-jsonrpc';

export function activate(context: ExtensionContext) {
    let serverExe = 'pseudocompiler-lsp';

    let serverOptions: ServerOptions = {
        run: { command: serverExe, args: [''] },
        debug: { command: serverExe, args: [''] }
    }

    let clientOptions: LanguageClientOptions = {
        documentSelector: [
            {
                pattern: '**/*.pseudo',
            }
        ],
        synchronize: {
            configurationSection: 'pseudocompiler',
            fileEvents: workspace.createFileSystemWatcher('**/*.pseudo')
        },
    }

    const client = new LanguageClient('pseudocompiler', 'Pseudocompiler', serverOptions, clientOptions);
    client.trace = Trace.Verbose;
    let disposable = client.start();

    context.subscriptions.push(disposable);
}
