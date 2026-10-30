// Checks plain and RFC 5987 filenames through the actual download click handler.
const vm = require('node:vm'), fs = require('node:fs'), assert = require('node:assert/strict');
const code = fs.readFileSync(require('node:path').join(process.argv[2], 'metaui-static.js'),'utf8');
(async () => {
for (const [header, expected] of [
 ['attachment; filename="100% results.csv"', '100% results.csv'],
 ['attachment; filename="raw%20name.csv"', 'raw%20name.csv'],
 ["attachment; filename*=UTF-8''encoded%20name.csv", 'encoded name.csv'],
 ["attachment; filename*=UTF-8''bad%name.csv", 'bad%name.csv'],
 ['', 'download']]) {
 let click, downloaded;
 const context = {window:{parent:{location:{search:'',pathname:'/',hash:''}}}, location:{pathname:'/app_test/',search:''}, history:{replaceState(){},pushState(){}}, document:{addEventListener(type, fn){if(type==='click')click=fn},createElement(){return {click(){downloaded=this.download},remove(){}}},body:{appendChild(){}}}, fetch:async()=>({ok:true,headers:{get:()=>header},blob:async()=>({})}), URL:{createObjectURL:()=> 'blob:x'},setTimeout(){},alert:msg=>{throw Error(msg)}};
 vm.runInNewContext(code, context);
 click({target:{closest:()=>({classList:{contains:()=>false},href:'/download'})},preventDefault(){},stopImmediatePropagation(){}});
 await new Promise(resolve => setImmediate(resolve)); assert.equal(downloaded,expected);
}
console.log('5 download filename cases passed');
})();
