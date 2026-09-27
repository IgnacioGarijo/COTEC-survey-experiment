"""Aggregate-only figures for integrated paper A."""
from pathlib import Path
import csv,json
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
ROOT=Path(__file__).resolve().parents[1]
OUT=ROOT/'3. Output/paper_A_2026-09-27';FIG=OUT/'figures'
def rows(name):
    with (OUT/'tables'/f'{name}.csv').open(encoding='utf-8-sig') as f:return list(csv.DictReader(f))
plt.rcParams.update({'font.size':11,'axes.spines.top':False,'axes.spines.right':False,'font.family':'DejaVu Sans'})
def save(fig,name):
    fig.savefig(FIG/f'{name}.png',dpi=230,bbox_inches='tight',facecolor='white');plt.close(fig)
# Nested, audited branches; no false flow through nonnested completion flags.
flow={r['stage']:int(r['n']) for r in rows('sample_flow')}
fig,ax=plt.subplots(figsize=(9.5,5));ax.set(xlim=(0,10),ylim=(0,5));ax.axis('off')
def box(x,y,text,color='#e9f0f3'):
    ax.text(x,y,text,ha='center',va='center',fontsize=11,bbox=dict(boxstyle='round,pad=.55',fc=color,ec='#215e70'))
def arrow(a,b):ax.annotate('',xy=b,xytext=a,arrowprops=dict(arrowstyle='->',color='#215e70',lw=1.5))
box(3,4.55,'Raw survey records\n4,942');box(7.5,4.55,'Screening exclusions\n389','#f5e9df');arrow((4.5,4.55),(6,4.55))
box(3,3.4,'Retained records\n4,553');arrow((3,4.12),(3,3.83))
box(7.5,3.4,'No retained recommendation\n1,848','#f5e9df');arrow((4.5,3.4),(5.7,3.4))
box(3,2.22,'Teachers with decisions\n2,705 (22,124 decisions)');arrow((3,2.98),(3,2.66))
box(7.5,2.22,'Incomplete selected belief items\n502','#f5e9df');arrow((4.8,2.22),(5.6,2.22))
box(3,1.08,'All selected belief items\n2,203');arrow((3,1.8),(3,1.52))
box(7.5,1.08,'Other covariates incomplete\n46','#f5e9df');arrow((4.5,1.08),(5.7,1.08))
box(3,-.03,'Common analysis sample\n2,157');arrow((3,.67),(3,.4));save(fig,'sample_flow')
# Recorded progression, denominators and timing distinct.
p=rows('task_progression');pos=[int(r['position']) for r in p]
fig,axs=plt.subplots(1,2,figsize=(9.5,3.5))
axs[0].plot(pos,[100*float(r['response_rate']) for r in p],'o-',color='#215e70');axs[0].set(xlabel='Recorded card position',ylabel='Available responses (%)',ylim=(0,102),xticks=pos,title='A. Response availability')
axs[1].plot(pos,[float(r['median_seconds']) for r in p],'o-',color='#215e70');axs[1].fill_between(pos,[float(r['p25']) for r in p],[float(r['p75']) for r in p],alpha=.15,color='#215e70');axs[1].set(xlabel='Recorded card position',ylabel='Page-submit time (seconds)',xticks=pos,title='B. Median and interquartile range')
fig.tight_layout();save(fig,'task_progression')
# Population margins, current common cases. Reference unchanged audited workbook.
s=json.loads((OUT/'reports/sample_aggregates.json').read_text(encoding='utf-8'))
with (ROOT/'3. Output/population_comparison_simulation_2026-09-26/comparison.csv').open(encoding='utf-8-sig') as f:rr=[r for r in csv.DictReader(f) if r['cohort']=='main']
def ref(v,c):return next(float(r['reference_pct']) for r in rr if r['variable']==v and r['category']==c)
def obs(v,c,cohort):
    a=s[cohort]['marginals'][v];return 100*a['counts'].get(c,0)/a['observed']
fig=plt.figure(figsize=(10,7.8));gs=fig.add_gridspec(3,2,height_ratios=[1,1.5,1],hspace=.72,wspace=.68)
ages=['20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65+'];prov=['Albacete','Ciudad Real','Cuenca','Guadalajara','Toledo']
panels=[(gs[0,0],'sex',['Women','Men'],['Women','Men'],'A. Sex/gender',100),(gs[0,1],'ownership',['Public','Private'],['Public','Private*'],'B. School ownership',100),(gs[1,0],'age_band',ages,ages,'C. Age',35),(gs[1,1],'province',prov,prov,'D. Province',45),(gs[2,:],'level',['Primary','Secondary'],['Infant/primary','Secondary'],'E. Educational stage (approximation)',100)]
for spec,v,cats,labels,title,limit in panels:
    ax=fig.add_subplot(spec);y=np.arange(len(cats))
    for k,(cohort,label,col,marker) in enumerate([(None,'Population 2023/24','#7d8791','s'),('main','Teachers with decisions','#215e70','o'),('complete','Complete cases','#b46630','^')]):
        ax.scatter([ref(v,c) if cohort is None else obs(v,c,cohort) for c in cats],y+(k-1)*.18,s=29,color=col,marker=marker,label=label)
    ax.set_yticks(y,labels);ax.set_ylim(len(cats)-.5,-.5);ax.set_xlim(0,limit);ax.set_xlabel('Percent');ax.set_title(title,loc='left',fontweight='bold',fontsize=12,pad=24);ax.grid(axis='x',alpha=.15)
    nref=int(next(float(r['reference_n']) for r in rr if r['variable']==v));ax.text(0,1.04,f"N: {nref:,} / {s['main']['marginals'][v]['observed']:,} / {s['complete']['marginals'][v]['observed']:,}",transform=ax.transAxes,fontsize=9,color='#52636c')
handles,labels=ax.get_legend_handles_labels();fig.legend(handles,labels,loc='upper center',ncol=3,frameon=False,fontsize=10);fig.subplots_adjust(left=.15,right=.98,top=.87,bottom=.07);save(fig,'population_grid')
# Optional completion after variance computation; a single uncertainty plot avoids tiny stacked labels.
if (OUT/'tables/variance_partition.csv').exists():
    v=rows('variance_partition');labels={'Student':'Student attributes','Professional':'Professional / demographic','Beliefs':'Beliefs and attitudes','Context':'Assignment context','Teacher_residual':'Residual teacher component','Decision_residual':'Remaining decision variation'}
    fig,axs=plt.subplots(2,1,figsize=(9,5),gridspec_kw={'height_ratios':[.7,3.5]});cols=['#215e70','#548d9d','#93b9b6','#b7b7b7','#be713f','#dfd6ca'];left=0
    for r,col in zip(v,cols):
        x=100*float(r['share']);axs[0].barh(0,x,left=left,color=col,height=.55)
        if x>5:axs[0].text(left+x/2,0,f'{x:.1f}%',ha='center',va='center',fontsize=10,color='white' if col in [cols[0],cols[4]] else 'black')
        left+=x
    axs[0].set_xlim(0,100);axs[0].set_yticks([]);axs[0].set_xticks([]);axs[0].set_title('Model-based partition on the linear-response scale',loc='left',fontsize=12)
    for i,(r,col) in enumerate(zip(v,cols)):
        x,lo,hi=[100*float(r[k]) for k in ('share','lo','hi')];axs[1].plot([lo,hi],[i,i],color=col,lw=2);axs[1].scatter(x,i,color=col,s=40);axs[1].text(max(x,hi)+1,i,f'{x:.2f}%',va='center',fontsize=10)
    axs[1].set_yticks(range(len(v)),[labels[r['component']] for r in v]);axs[1].invert_yaxis();axs[1].set_xlabel('Share of model-implied variance (%) and teacher-bootstrap 95% intervals');axs[1].set_xlim(0,70);axs[1].grid(axis='x',alpha=.15);fig.tight_layout();save(fig,'variance_partition')
# Show the actual dispersion across repeated partitions without an empty zero-to-.4 axis.
r=np.array([float(row['correlation']) for row in rows('split_half_partitions')])
fig,ax=plt.subplots(figsize=(7.7,3.6))
ax.hist(r,bins=30,color='#215e70',edgecolor='white')
ax.axvline(r.mean(),color='#be713f',lw=1.8,label=f'Mean correlation: {r.mean():.3f}')
ax.set_xlabel('Correlation between independently referenced response halves')
ax.set_ylabel('Random partitions');ax.legend(frameon=False);fig.tight_layout();save(fig,'split_half')
print('Figures written to',FIG)
