from pathlib import Path
p=Path('/tmp/shinylive-speed/C/site/shinylive/webr/R.js');s=Path('/tmp/shinylive-speed/C/R.original.js').read_text()
old='updateTableMap(tableBase,metadata.tableSize);moduleExports=relocateExports(instance.exports,memoryBase);updateGOT(moduleExports);'
new='var pt=performance.now();updateTableMap(tableBase,metadata.tableSize);var tableMs=performance.now()-pt;pt=performance.now();moduleExports=relocateExports(instance.exports,memoryBase);var relocateMs=performance.now()-pt;pt=performance.now();updateGOT(moduleExports);console.log("POSTPROFILE",JSON.stringify({path:libName,table:tableMs,relocate:relocateMs,got:performance.now()-pt}));'
assert old in s;p.write_text(s.replace(old,new))
