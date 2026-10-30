"""Summarise the complete helper audit; retain every mismatch, including NA patterns."""
import csv, math, os, sys
from pathlib import Path
src = Path(sys.argv[1] if len(sys.argv) > 1 else os.environ.get('METAUI_EQUIVALENCE_OUT', '/tmp/shinylive-speed/W2/equivalence')) / 'all-comparisons.csv'
out = Path(sys.argv[2] if len(sys.argv) > 2 else os.environ.get('METAUI_SUMMARY_OUT', '/tmp/shinylive-speed/W2/helper-summary'))
out.mkdir(parents=True, exist_ok=True)
r = list(csv.DictReader(src.open()))
def num(x):
    try: return float(x)
    except ValueError: return math.nan
summary = []
for kind in sorted({x['comparison'] for x in r}):
    for q in sorted({x['quantity'] for x in r if x['comparison']==kind and x['group'] in ('simulation','fixture')}):
        rr=[x for x in r if x['comparison']==kind and x['quantity']==q and x['group'] in ('simulation','fixture')]
        diffs=[num(x['difference']) for x in rr if math.isfinite(num(x['difference']))]
        summary.append(dict(comparison=kind,quantity=q,finite_pairs=len(diffs),max_abs_difference=max(diffs,default=math.nan)))
def write(name,rows,fields):
    with (out/name).open('w') as f:
        w=csv.DictWriter(f,fieldnames=fields,lineterminator="\n");w.writeheader();w.writerows(rows)
write('helper-ordinary-summary.csv',summary,list(summary[0]))
bad=[]
for x in r:
    reasons=[]
    if num(x['difference'])>1e-8: reasons.append('numeric')
    if math.isfinite(num(x['meta'])) != math.isfinite(num(x['candidate'])): reasons.append('finite/missing')
    if x['meta_status']!=x['candidate_status']: reasons.append('status')
    if x['meta_warnings']!=x['candidate_warnings']: reasons.append('warning')
    if x['meta_error']!=x['candidate_error']: reasons.append('error text')
    if reasons: bad.append(dict(x, differences='; '.join(reasons)))
write('helper-mismatches.csv',bad,list(r[0])+['differences'])
write('helper-all-comparisons.csv',r,list(r[0]))
print('Datasets:',len({x['dataset'] for x in r}))
for x in summary: print(x)
print('Mismatch datasets:',sorted({x['dataset'] for x in bad}))
