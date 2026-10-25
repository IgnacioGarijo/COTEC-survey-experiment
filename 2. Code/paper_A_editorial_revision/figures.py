"""Approved aggregate-only replacements for the two main numerical tables."""
from pathlib import Path
import csv
from io import BytesIO
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import numpy as np
R=Path(__file__).resolve().parents[2];O=R/'3. Output/paper_A_final_revision/B';F=R/'3. Output/paper_A/figures'
def rows(name):
 with (O/(name+'.csv')).open(encoding='utf8') as f:return list(csv.DictReader(f))
def save(fig,name):
 b=BytesIO();fig.savefig(b,format='png',dpi=230,bbox_inches='tight',facecolor='white');plt.close(fig);p=F/(name+'.png')
 if not p.exists() or p.read_bytes()!=b.getvalue():p.write_bytes(b.getvalue())
plt.rcParams.update({'font.size':11,'font.family':'DejaVu Sans','axes.spines.top':False,'axes.spines.right':False})
terms=['failed','competence','absent','disruptive','male','background']
labels=['Three or more failed subjects','Low basic competences','Frequent unjustified absence','Serious disciplinary problems','Male student','Complex social background']
r={x['term']:x for x in rows('attributes')};x=np.array([100*float(r[t]['estimate']) for t in terms]);se=np.array([100*float(r[t]['se']) for t in terms])
fig,ax=plt.subplots(figsize=(10,4.3));y=np.arange(6)
ax.errorbar(x,y,xerr=se,fmt='o',color='#215e70',capsize=3,ms=6,lw=1.8)
for v,e,yy in zip(x,se,y):ax.text(v+e+1.2,yy,f'{v:.2f}',va='center',fontsize=10)
ax.axvline(0,color='#777',lw=1);ax.set_yticks(y,labels);ax.invert_yaxis();ax.set(xlim=(-5,62),xlabel='Within-teacher coefficient (percentage points)');ax.grid(axis='x',alpha=.14);fig.tight_layout();save(fig,'attribute_coefficients')
r=rows('academic_categories');labels=['Neither academic difficulty','Low competences only','Three or more failures only','Both academic difficulties']
fig,axs=plt.subplots(1,2,figsize=(10,4.2),sharey=True,gridspec_kw={'wspace':.25})
y=np.arange(4)
for ax in axs:
 for yy in [1,2]:ax.axhspan(yy-.4,yy+.4,color='#f4efe6',zorder=0)
 ax.grid(axis='x',alpha=.13);ax.set_ylim(3.65,-.6)
x=np.array([100*float(a['estimate']) for a in r]);e=np.array([100*float(a['se']) for a in r]);d=np.array([100*float(a['disagreement']) for a in r])
axs[0].errorbar(x,y,xerr=e,fmt='o',capsize=3,color='#215e70',lw=1.5)
axs[1].scatter(d,y,color='#b46630',s=40)
for yy,v,ee,dd in zip(y,x,e,d):
 axs[0].text(v+ee+2,yy,f'{v:.2f}',va='center',fontsize=10)
 axs[1].text(dd+1.3,yy,f'{dd:.2f}',va='center',fontsize=10)
axs[0].set_yticks(y,labels);axs[0].set(xlim=(0,105),xlabel='Recommendations (%)',title='A. Recommend repetition')
axs[1].set(xlim=(0,58),xlabel='Same-profile disagreement (%)',title='B. Two teachers disagree')
fig.subplots_adjust(left=.29,bottom=.18,top=.87,right=.98);save(fig,'academic_profiles')
# Keep the table generation pipeline's descriptive headings, not opaque X labels.
p=R/'4. Latex/paper_A/tables/characterization.tex'
t=p.read_text(encoding='utf8').replace('$X_1$ & $X_2$ & $X_3$',r'\shortstack{Professional /\\demographic} & \shortstack{+ General\\beliefs} & \shortstack{+ Excess\\resources}')
p.write_text(t,encoding='utf8')
