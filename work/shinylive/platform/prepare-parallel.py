from pathlib import Path
import tarfile,json
root=Path('/tmp/shinylive-speed/C/parallel/shinylive/webr')
mods=root/'metaui-modules';mods.mkdir(exist_ok=True)
manifest=[]
for archive in root.glob('packages/*/*.tgz'):
 with tarfile.open(archive) as t:
  for m in t:
   if m.name.endswith('.so'):
    name=m.name.replace('/','_');(mods/name).write_bytes(t.extractfile(m).read());manifest.append({'path':m.name,'url':'metaui-modules/'+name})
# Base library includes sass, etc.
import gzip
raw=gzip.decompress((root/'library.data.gz').read_bytes())
for m in json.loads((root/'library.js.metadata').read_text())['files']:
 if m['filename'].endswith('.so'):
  name=m['filename'].strip('/');dest=name.replace('/','_');(mods/dest).write_bytes(raw[m['start']:m['end']]);manifest.append({'path':name,'url':'metaui-modules/'+dest})
(root/'metaui-modules.json').write_text(json.dumps(manifest))
p=root/'webr-worker.js';s=p.read_text();Path('/tmp/shinylive-speed/C/worker.original.js').write_text(s)
old='setTimeout(()=>{let t=`${X.baseUrl}R.js`;Tt(t)})'
new='''setTimeout(async()=>{globalThis.metauiModules={};try{const entries=await(await fetch(X.baseUrl+'metaui-modules.json')).json();await Promise.all(entries.map(async e=>{globalThis.metauiModules[e.path]=await WebAssembly.compileStreaming(fetch(X.baseUrl+e.url));}));}catch(e){console.warn('Precompile failed; using normal loader',e);}let t=`${X.baseUrl}R.js`;Tt(t)})'''
assert old in s;p.write_text(s.replace(old,new))
p=root/'R.js';s=Path('/tmp/shinylive-speed/C/R.original.js').read_text();old='var module=binary instanceof WebAssembly.Module?binary:new WebAssembly.Module(binary);';new='var module=globalThis.metauiModules?.[libName.replace(/^.*(?:packages|library)\\//, "")] || (binary instanceof WebAssembly.Module?binary:new WebAssembly.Module(binary));';assert old in s;p.write_text(s.replace(old,new))
print(len(manifest),'modules',sum(p.stat().st_size for p in mods.iterdir()))
