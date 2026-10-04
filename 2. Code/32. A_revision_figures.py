"""Aggregate-only figures for integrated paper A."""
from pathlib import Path
import csv,json
import numpy as np
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
ROOT=Path(__file__).resolve().parents[1]
OUT=ROOT/'3. Output/paper_A';FIG=OUT/'figures'
def rows(name):
    with (OUT/'tables'/f'{name}.csv').open(encoding='utf-8-sig') as f:return list(csv.DictReader(f))
plt.rcParams.update({'font.size':11,'axes.spines.top':False,'axes.spines.right':False,'font.family':'DejaVu Sans'})
def save(fig,name):
    fig.savefig(FIG/f'{name}.png',dpi=230,bbox_inches='tight',facecolor='white');plt.close(fig)
# Participation starts after minimal entries; mutually exclusive exit branches.
from matplotlib.patches import FancyBboxPatch
flow=json.loads((OUT/'reports/survey_exit_audit.json').read_text(encoding='utf8'))
fig,ax=plt.subplots(figsize=(10,7.3));ax.set(xlim=(0,12),ylim=(1.1,10.8));ax.axis('off')
def box(x,y,title,n,detail='',side=False):
    ax.add_patch(FancyBboxPatch((x-2.6,y-.57),5.2,1.14,boxstyle='round,pad=.03',facecolor='#f6ede5' if side else '#e9f0f3',edgecolor='#ae754b' if side else '#215e70',lw=1))
    ax.text(x,y+.23,title,ha='center',va='center',fontsize=10)
    ax.text(x,y-.08,f'{n:,}',ha='center',va='center',fontsize=17,fontweight='bold',color='#215e70')
    if detail:ax.text(x,y-.40,detail,ha='center',va='center',fontsize=8,color='#555555')
def arrow(a,b):ax.annotate('',xy=b,xytext=a,arrowprops=dict(arrowstyle='->',lw=1.2,color='#215e70'))
main=[(10,'Start substantive survey questions',flow['participation_denominator'],'Teaching-experience question or beyond'),(8,'Answer at least one card',flow['any_recorded_card'],'Includes example cards'),(6,'Reach the end of the decision task',flow['reached_decision_end'],''),(4,'Pass the screening rule',flow['decision_end_passed_filter'],''),(2,'Finish the survey',flow['completed_passed_filter'],'')]
branches=[(9,'Stop before any card decision',flow['before_decisions'],''),(7,'Stop during the decision task',flow['during_decisions'],'31 also fail the screening rule'),(5,'Excluded by the screening rule',flow['filter_exclusions_at_decision_end'],'Among those reaching the end of decisions'),(3,'Stop after the decision task',flow['after_decisions'],'')]
for y,title,n,note in main:box(2.8,y,title,n,note)
for y,title,n,note in branches:
    box(9,y,title,n,note,True);arrow((2.8,y+.38),(2.8,y-.38));arrow((2.8,y),(6.36,y))
fig.tight_layout(pad=.3);save(fig,'sample_flow')
# Author-approved histogram: same observations, wide solid bins, percentages.
with (OUT/'reports/harshness_histogram_candidate.csv').open(encoding='utf8') as f:hist=list(csv.DictReader(f))
fig,ax=plt.subplots(figsize=(8.3,3.8))
for r in hist:ax.bar(float(r['left']),float(r['share']),width=float(r['right'])-float(r['left']),align='edge',color='#215e70',edgecolor='white',linewidth=.5)
ax.axvline(0,color='#555555',ls='--',lw=.9);ax.set(xlim=(-100,75),xlabel='Harshness (percentage points)',ylabel='Teachers (%)',xticks=[-75,-50,-25,0,25,50,75]);fig.tight_layout();save(fig,'harshness_distribution')
# Recorded progression, denominators and timing distinct.
p=rows('task_progression');pos=[int(r['position']) for r in p]
fig,axs=plt.subplots(1,2,figsize=(9.5,3.5))
axs[0].plot(pos,[100*float(r['response_rate']) for r in p],'o-',color='#215e70');axs[0].set(xlabel='Recorded card position',ylabel='Cards answered (%)',ylim=(0,102),xticks=pos,title='A. Percentage of cards answered')
axs[1].plot(pos,[float(r['median_seconds']) for r in p],'o-',color='#215e70');axs[1].fill_between(pos,[float(r['p25']) for r in p],[float(r['p75']) for r in p],alpha=.15,color='#215e70');axs[1].set(xlabel='Recorded card position',ylabel='Page-submit time (seconds)',xticks=pos,title='B. Median and interquartile range')
fig.tight_layout();save(fig,'task_progression')
# Population margins, current common cases. Reference unchanged audited workbook.
s=json.loads((OUT/'reports/sample_aggregates.json').read_text(encoding='utf-8'))
with (ROOT/'3. Output/population/comparison.csv').open(encoding='utf-8-sig') as f:rr=[r for r in csv.DictReader(f) if r['cohort']=='main']
def ref(v,c):return next(float(r['reference_pct']) for r in rr if r['variable']==v and r['category']==c)
def obs(v,c,cohort):
    a=s[cohort]['marginals'][v];return 100*a['counts'].get(c,0)/a['observed']
fig=plt.figure(figsize=(10,7.8));gs=fig.add_gridspec(3,2,height_ratios=[1,1.5,1],hspace=.72,wspace=.68)
ages=['20-24','25-29','30-34','35-39','40-44','45-49','50-54','55-59','60-64','65+'];prov=['Albacete','Ciudad Real','Cuenca','Guadalajara','Toledo']
panels=[(gs[0,0],'sex',['Women','Men'],['Women','Men'],'A. Sex/gender',100),(gs[0,1],'ownership',['Public','Private'],['Public','Private*'],'B. School ownership',100),(gs[1,0],'age_band',ages,ages,'C. Age',35),(gs[1,1],'province',prov,['Province A','Province B','Province C','Province D','Province E'],'D. Province (anonymized)',45),(gs[2,:],'level',['Primary','Secondary'],['Infant/primary','Secondary'],'E. Educational stage (approximation)',100)]
for spec,v,cats,labels,title,limit in panels:
    ax=fig.add_subplot(spec);y=np.arange(len(cats))
    for k,(cohort,label,col,marker) in enumerate([(None,'Population 2023/24','#7d8791','s'),('main','Teachers with decisions','#215e70','o'),('complete','Complete cases','#b46630','^')]):
        ax.scatter([ref(v,c) if cohort is None else obs(v,c,cohort) for c in cats],y+(k-1)*.18,s=29,color=col,marker=marker,label=label)
    ax.set_yticks(y,labels);ax.set_ylim(len(cats)-.5,-.5);ax.set_xlim(0,limit);ax.set_xlabel('Percent');ax.set_title(title,loc='left',fontweight='bold',fontsize=12,pad=24);ax.grid(axis='x',alpha=.15)
    nref=int(next(float(r['reference_n']) for r in rr if r['variable']==v));ax.text(0,1.04,f"N: {nref:,} / {s['main']['marginals'][v]['observed']:,} / {s['complete']['marginals'][v]['observed']:,}",transform=ax.transAxes,fontsize=9,color='#52636c')
handles,labels=ax.get_legend_handles_labels();fig.legend(handles,labels,loc='upper center',ncol=3,frameon=False,fontsize=10);fig.subplots_adjust(left=.15,right=.98,top=.87,bottom=.07);save(fig,'population_grid')
# Approved six-component model; never reuse uncertainty from the old model.
v=rows('final_decomposition')
labels={'Student':'Student attributes + interactions','Professional':'Professional / demographic','Beliefs':'Beliefs','Harshness':'Observed harshness','Sensitivities':'Teacher attribute sensitivities','Residual':'Remaining decision variation'}
colors=['#215e70','#548d9d','#93b9b6','#be713f','#8c637b','#dfd6ca']
fig,axs=plt.subplots(2,1,figsize=(9,4.8),gridspec_kw={'height_ratios':[.6,3.3]});left=0
for r,col in zip(v,colors):
    x=100*float(r['share']);axs[0].barh(0,x,left=left,color=col,height=.6)
    if x>5:axs[0].text(left+x/2,0,f'{x:.1f}%',ha='center',va='center',fontsize=10,color='black' if col==colors[-1] else 'white')
    left+=x
axs[0].set(xlim=(0,100),yticks=[],xticks=[])
for i,(r,col) in enumerate(zip(v,colors)):
    x=100*float(r['share']);se=100*float(r.get('se',0))
    if se:axs[1].plot([x-se,x+se],[i,i],color=col,lw=2)
    axs[1].scatter(x,i,color=col,s=40);axs[1].text(x+se+1,i,f'{x:.2f}%'+(f' (SE {se:.2f})' if 'se' in r else ''),va='center',fontsize=10)
axs[1].set_yticks(range(len(v)),[labels[r['component']] for r in v]);axs[1].invert_yaxis();axs[1].set(xlabel='Share of model-implied variance (%)',xlim=(0,69));axs[1].grid(axis='x',alpha=.15);fig.tight_layout();save(fig,'variance_partition')
# Show the actual dispersion across repeated partitions without an empty zero-to-.4 axis.
r=np.array([float(row['correlation']) for row in rows('split_half_partitions')])
fig,ax=plt.subplots(figsize=(7.7,3.6))
ax.hist(r,bins=30,color='#215e70',edgecolor='white')
ax.axvline(r.mean(),color='#be713f',lw=1.8,label=f'Mean correlation: {r.mean():.3f}')
ax.set_xlabel('Correlation between independently referenced response halves')
ax.set_ylabel('Random partitions');ax.legend(frameon=False);fig.tight_layout();save(fig,'split_half')
print('Figures written to',FIG)
