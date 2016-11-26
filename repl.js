__ob_eval__ = require('./util')

require('repl').start({
    prompt: '',
    input: process.stdin,
    output: process.stdout,
    ignoreUndefined: true,
    useColors: false,
    terminal: false
})
