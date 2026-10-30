from pathlib import Path
import sys
root=Path(sys.argv[1]) if len(sys.argv)>1 else Path("/tmp/shinylive-speed/C/site")
p=root/'shinylive/webr/R.js';s=p.read_text();Path('/tmp/shinylive-speed/C/R.original.js').write_text(s)
s=s.replace('var Module = globalThis.Module;', '''var Module = globalThis.Module;
const mountOriginal = Module.mountImageUrl;
Module.mountImageUrl = function(...args) { const t=performance.now(); try {return mountOriginal(...args);} finally {console.log('CPROFILE', JSON.stringify({kind:'mount',path:args[0],ms:performance.now()-t}));} };
''',1)
s=s.replace('var module=binary instanceof WebAssembly.Module?binary:new WebAssembly.Module(binary);var instance=new WebAssembly.Instance(module,info);return postInstantiation(module,instance)', '''var ct=performance.now();var module=binary instanceof WebAssembly.Module?binary:new WebAssembly.Module(binary);var cm=performance.now()-ct;ct=performance.now();var instance=new WebAssembly.Instance(module,info);var im=performance.now()-ct;ct=performance.now();var answer=postInstantiation(module,instance);console.log('CPROFILE',JSON.stringify({kind:'dylink',path:libName,compile:cm,instantiate:im,post:performance.now()-ct}));return answer''')
p.write_text(s)
p=root/'shinylive/shinylive.js';s=p.read_text();Path('/tmp/shinylive-speed/C/shinylive.original.js').write_text(s)
s=s.replace('.mount_vfs_images <- function() {','.mount_vfs_images <- function() {\n  cat("CPHASE mount-start", format(Sys.time(), "%OS3"), "\\\\n")\n  on.exit(cat("CPHASE mount-end", format(Sys.time(), "%OS3"), "\\\\n"))')
p.write_text(s)
