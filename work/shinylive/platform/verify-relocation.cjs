// Verify the patched loader's real relocation function, including failure cleanup.
// node work/shinylive/platform/verify-relocation.cjs /path/to/shinylive/webr/R.js
const fs = require('node:fs');
const assert = require('node:assert/strict');
const vm = require('node:vm');
const code = fs.readFileSync(process.argv[2], 'utf8');
const start = code.indexOf('var isImmutableGlobal=');
const end = code.indexOf(';var isSymbolDefined=', start);
assert(start >= 0 && end > start, 'recognized webR relocation functions');
const relocate = vm.runInThisContext(`(()=>{${code.slice(start, end)};return relocateExports;})()`);
const immutable = new WebAssembly.Global({value: 'i32'}, 17);
const mutable = new WebAssembly.Global({value: 'i32', mutable: true}, 23);
const fn = () => 9;
const originalLimit = Error.stackTraceLimit;
const out = relocate({immutable, mutable, fn}, 100);
assert.equal(out.immutable.value, 117);
assert.equal(immutable.value, 17);
assert.equal(out.mutable, mutable);
assert.equal(out.mutable.value, 23);
assert.equal(out.fn, fn);
assert.equal(Error.stackTraceLimit, originalLimit);
const badExports = {get broken() {throw new Error('unexpected relocation error');}};
assert.throws(() => relocate(badExports), /unexpected relocation error/);
assert.equal(Error.stackTraceLimit, originalLimit);
console.log('PASS: immutable offsets, mutable identity, function identity, stack restoration on success and failure');
