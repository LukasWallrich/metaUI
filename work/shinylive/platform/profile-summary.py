import json,collections,sys
j=json.load(open(sys.argv[1] if len(sys.argv)>1 else '/tmp/shinylive-speed/C/profile.json'));rows=[]
for x in j:
 if 'CPROFILE ' in x['text']:
  try: rows.append(json.loads(x['text'].split('CPROFILE ',1)[1]))
  except:pass
for r in rows:
 if r['kind']=='dylink':print(r)
print('SUM', {k:sum(r.get(k,0) for r in rows if r['kind']=='dylink') for k in ['compile','instantiate','post']})
print('MOUNT',sum(r.get('ms',0) for r in rows if r['kind']=='mount'))
