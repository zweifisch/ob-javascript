
module.exports = __ob_eval__ = function(path, eoe, outputPath) {
    result = eval(require('fs').readFileSync(path, {encoding:'utf8'}))
    write = function(obj) {
        if (outputPath) {
            if (obj instanceof Buffer) {
                require('fs').writeFile(outputPath, obj)
            } else if (obj && 'function' === typeof obj.pipe) {
                obj.pipe(require('fs').createWriteStream(outputPath))
            }
        } else {
            console.log(obj)
        }
        process.stdout.write(eoe)
    }
    if (result && 'function' === typeof result.then) {
        result.then(write, write)
    } else {
        write(result)
    }
}
