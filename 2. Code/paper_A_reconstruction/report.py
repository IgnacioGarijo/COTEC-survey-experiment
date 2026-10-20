"""Build a self-contained Spanish comparison from aggregate CSVs only."""
from pathlib import Path
import argparse,base64,html,json
import pandas as pd

ROOT=Path(__file__).resolve().parents[2]
OUT=ROOT/'3. Output/paper_A_reconstruction'
FIG=OUT/'figures';FIG.mkdir(exist_ok=True)
MAIN=['A','B','C','D']
ALL=MAIN+['B_full8','C_full8','C_inclusive','C_example_screen']
LABEL={'A':'A · Actual','B':'B · Ordenadas, incluidas coincidencias','C':'C · Sin respuestas ambiguas','D':'D · Docentes sin coincidencias','B_full8':'B · Ocho valores guardados','C_full8':'C · Ocho guardados, retirando ambiguos','C_inclusive':'C · Sin filtro por decisiones','C_example_screen':'C · Filtro del ejemplo favorable específico'}
ATTR={'failed':'Tres o más suspensos','competence':'Competencias bajas','absent':'Absentismo','disruptive':'Disciplina','background':'Contexto social complejo','male':'Alumno varón'}
INTERACTIONS={'failed:competence':'Suspensos × competencias','failed:absent':'Suspensos × absentismo','failed:disruptive':'Suspensos × disciplina'}
COMP={'Student':'Atributos e interacciones','Professional':'Perfil docente','Beliefs':'Creencias','Harshness':'Harshness observado','Sensitivities':'Sensibilidades docentes','Residual':'Residuo'}

def read(name,s=None):return pd.read_csv(OUT/(s if s else 'tables')/(name+'.csv'))
def js(name,s):return json.loads((OUT/s/(name+'.json')).read_text(encoding='utf-8'))
def fmt(x,d=2):
    if pd.isna(x):return '—'
    return f'{float(x):,.{d}f}'.replace(',','§').replace('.',',').replace('§','.')
def integer(x):return fmt(x,0)
def value(s,term,file='attributes',model=None):
    z=read(file,s);z=z[z.term.eq(term)]
    if model is not None:z=z[z.model.eq(model)]
    return z.iloc[0]
def table(df):return '<div class="table-wrap">'+df.to_html(index=False,escape=True,border=0,na_rep='—')+'</div>'
def pic(name,caption):
    path=FIG/(name+'.png')
    return f'<figure><img alt="{html.escape(caption)}" src="data:image/png;base64,{base64.b64encode(path.read_bytes()).decode()}"><figcaption>{caption}</figcaption></figure>'
def build(final=False):
    summaries=pd.concat([read('summary',s) for s in ALL],ignore_index=True).set_index('scenario')
    diagnostics={s:js('decomposition_diagnostics',s) for s in ALL}
    status={s:js('bootstrap_status',s) if (OUT/s/'bootstrap_status.json').exists() else {} for s in MAIN}
    fingerprints={s:js('bootstrap_fingerprint',s) if (OUT/s/'bootstrap_fingerprint.json').exists() else {} for s in MAIN}
    v2_fingerprints={s:js('bootstrap_v2_fingerprint',s) if (OUT/s/'bootstrap_v2_fingerprint.json').exists() else {} for s in MAIN}
    # The B v1 run was discarded after the initialization audit improved logLik
    # by 4–7 units. Its still-present files must not supply report uncertainty.
    B_counts=status['B'].get('configuration_counts',{})
    B_v2_complete=v2_fingerprints['B'].get('method')=='clone-LOO-six-correlated-slopes-v2-numerical' and bool(B_counts) and all(key.startswith('v2-') for key in B_counts) and sum(B_counts.values())==199
    discarded={s:s=='B' and fingerprints[s].get('bootstrap_method')=='clone-LOO-six-correlated-slopes-v1' and not B_v2_complete for s in MAIN}
    usable={s:not discarded[s] and status[s].get('attempted')==199 and status[s].get('accepted',0)>=190 and (OUT/s/'decomposition.csv').exists() for s in MAIN}
    if final:
        assert all(usable.values()),'Incomplete or discarded main decomposition uncertainty'
        assert read('verification_checks').passed.all()
        final_checks=pd.read_csv(OUT/'diagnostics'/'final_deliverable_checks.csv')
        assert not final_checks.empty and final_checks.passed.all(),'Final deliverable checks incomplete'
        assert json.loads((OUT/'protection_verification.json').read_text())['unchanged']
        required=[OUT/s/'characterization_bootstrap.csv' for s in ALL]
        required += [OUT/s/name for s in MAIN for name in ['sequence_summary.csv','split_half_summary.csv','task_progression.csv']]
        required += [OUT/'tables'/name for name in ['participation_comparison.csv','score_changes_vs_A.csv','legacy_raw_progression_reproduced.csv']]
        assert all(path.exists() for path in required),'Incomplete appendix outputs'
        assert all(not read('characterization_bootstrap',s).empty and read('characterization_bootstrap',s).successful.eq(999).all() for s in ALL),'Incomplete characterization uncertainty'
    # Figure generation is separate: figures.R consumes aggregate CSV files only.
    parts={s:read('decomposition_point',s).set_index('component').share for s in MAIN}

    sections=[]
    def section(title,body):sections.append(f'<section><h2>{title}</h2>{body}</section>')
    A=summaries.loc['A'];C=summaries.loc['C']
    delta_repeat=100*(C.mean_repeat-A.mean_repeat)
    intro=f'''<p class="lead">La identificación por orden cambia la muestra y la distribución de las recomendaciones. Al retirar las respuestas ambiguas (C), quedan <strong>{integer(C.teachers)} registros docentes y {integer(C.decisions)} decisiones</strong>, frente a {integer(A.teachers)} y {integer(A.decisions)} en A.</p>
    <div class="cards"><div><b>{fmt(100*A.mean_repeat)} → {fmt(100*C.mean_repeat)} %</b><span>Repetición en las tarjetas conservadas; cambio {fmt(delta_repeat)} pp. No es una tasa poblacional.</span></div><div><b>{fmt(100*value('A','failed').estimate)} → {fmt(100*value('C','failed').estimate)} pp</b><span>Coeficiente de tres o más suspensos.</span></div><div><b>Covarianza singular en B, C y D</b><span>El modelo de seis pendientes converge en la frontera; se conserva y se documenta.</span></div></div>
    <p><strong>Recomendación:</strong> usar C como referencia experimental conservadora para decidir la revisión, manteniendo por ahora el filtro vigente. B explicita el supuesto sobre campos compartidos; D comprueba sensibilidad a excluir docentes completos. Resolver por separado el filtro y la correspondencia de las tarjetas antes de adoptar una versión definitiva.</p>
    <p>Se mantienen los efectos académicos dominantes y la escasa capacidad descriptiva de las covariables docentes. La corrección afecta especialmente a niveles descriptivos, distribución del harshness, grupos por terciles y diagnóstico del modelo mixto. Las conclusiones detalladas se apoyan en estimaciones nuevas e incertidumbre reconstruida.</p>'''
    section('Resumen para decidir',intro)
    defs=pd.DataFrame([
        ['A','Todas las respuestas actuales salvo los dos campos favorables ya omitidos','Referencia reproducida; mantiene mezcla de ejemplos y tarea'],
        ['B','Sólo tarjetas de la secuencia FL válida, incluidas las coincidentes con ejemplos','Supone utilizable el valor compartido; no se demuestra que sea respuesta posterior'],
        ['C','B retirando sólo las observaciones coincidentes con los ejemplos del registro','Conserva el resto de respuestas válidas y las respuestas parciales'],
        ['D','B retirando todo registro cuyo conjunto asignado coincide con sus ejemplos','Sensibilidad con otra composición de docentes y tarjetas']],columns=['Escenario','Regla','Interpretación'])
    section('Qué compara cada escenario',table(defs)+'<p>A–D mantienen el cribado actual de registros basado en los cuatro campos favorables. A omite globalmente los campos <code>alumno_1_1</code> y <code>alumno_2_1</code>; B, C y D los admiten cuando están asignados y cumplen su regla de procedencia. Por eso el paso de A a B no es una simple resta de ejemplos. Las comprobaciones con ocho valores guardados y con otros filtros se presentan aparte. Una secuencia válida identifica asignación; un valor guardado identifica almacenamiento; ninguna de las dos cosas demuestra por sí sola una nueva respuesta tras el ejemplo.</p><p>En C se retiran observaciones. D elimina registros completos con cualquier coincidencia, incluidas sus respuestas no ambiguas. La condición de D depende de la combinación de bloque asignado y variante de Ejemplo: cambia participantes, mezcla de perfiles y referencias, además del tratamiento de los campos compartidos.</p>')
    audit=read('audit_counts')
    audit_labels={
        'raw_records':('Registros brutos','Conjunto bruto'),
        'unique_ResponseId':('Identificadores de registro únicos','Conjunto bruto'),
        'valid_sequences':('Secuencias de ocho asignaciones válidas','Conjunto bruto'),
        'no_collisions_raw':('Registros sin coincidencias','Bruto con orden válido'),
        'one_collision_raw':('Registros con una coincidencia','Bruto con orden válido'),
        'two_collisions_raw':('Registros con dos coincidencias','Bruto con orden válido'),
        'eight_stored_no_collisions_raw':('Registros sin coincidencias y con ocho valores guardados','Bruto con orden válido'),
        'A_with_valid_order':('Registros A con orden válido','Muestra A'),
        'A_without_valid_order':('Registros A sin orden válido','Muestra A'),
        'A_adverse_examples_outside_order':('Respuestas de ejemplos adversos fuera de orden','Muestra A'),
        'raw_shared_fields':('Campos compartidos entre ejemplo y tarea','Bruto con orden válido'),
        'raw_shared_records':('Registros con campos compartidos','Bruto con orden válido'),
        'current_screen_exclusions':('Registros excluidos por el cribado vigente','Conjunto bruto')}
    audit['Comprobación']=audit.metric.map(lambda key:audit_labels.get(key,(key,'No especificada'))[0])
    audit['Población']=audit.metric.map(lambda key:audit_labels.get(key,(key,'No especificada'))[1])
    audit['Recuento']=audit.value.map(integer);audit=audit[['Comprobación','Población','Recuento']]
    section('Auditoría independiente de procedencia',table(audit)+'''<p>Los valores 1–2 de <code>Ejemplo</code> se asocian con <code>alumno_1_1</code> y <code>alumno_3_8</code>; 3–4 con <code>alumno_2_1</code> y <code>alumno_2_8</code>. La comprobación usa la asociación con campos fuera del orden y los atributos codificados, no sólo perfiles extremos. Mapeos excepcionales: <code>alumno_3_5 → Grupo 5_1</code> y <code>alumno_6_1 → Grupo 6_1</code>.</p>
    <p>Los campos compartidos tienen tiempos y clics positivos, pero también los tienen los ejemplos fuera de la secuencia. No existen en el export inspeccionado copias separadas por visita que permitan atribuir el valor a la presentación experimental. Los campos DO de la pregunta no equivalen a los FL de orden del bloque. <code>Calibracion</code> corresponde al control, no a superar atención.</p>
    <p>Las 706 coincidencias y 533 registros se refieren al conjunto bruto con orden. Las 2.292 respuestas de ejemplo conservadas corresponden a la muestra actual. Estos recuentos tienen denominadores distintos y no deben sumarse.</p>
    <p>La codificación vigente tiene 64 identificadores y 56 combinaciones distintas de atributos. Se mantiene para aislar la corrección de observaciones; sin el texto original completo de las tarjetas no se decide si las duplicaciones son deliberadas o errores. Se exportan las parejas y el soporte para verificarlas.</p>''')
    sample=summaries.loc[MAIN,['teachers','decisions','fe_teachers','fe_decisions','common_teachers','common_decisions','identifiers','coded_profiles']].reset_index();sample.columns=['Escenario','Docentes','Decisiones','Docentes FE','Decisiones FE','Docentes con X3 completo','Decisiones de docentes con X3 completo','IDs tarjeta','Perfiles codificados']
    for c in sample.columns[1:]:sample[c]=sample[c].map(integer)
    section('Muestras y soporte',table(sample)+pic('sample_comparison','Recuentos nativos por escenario; los casos completos se reconstruyen dentro de cada uno.')+'<p>Los 2.612 registros actuales con orden incluyen casos sin ninguna respuesta guardada en las ocho tarjetas. Por eso B tiene 2.594 registros con resultado. C conserva 2.587: retirar sólo sus respuestas ambiguas deja a siete registros adicionales sin resultado. No se impone completar las ocho tarjetas.</p>')
    observation_changes=[]
    for s in ['B','C','D']:
        counts=read('observation_changes_vs_A',s).groupby('change')['n'].sum()
        observation_changes.append({'Escenario':s,'Respuestas retiradas de A':integer(counts.get('removed',0)),'Respuestas incorporadas frente a A':integer(counts.get('added',0)),'Respuestas finales':integer(summaries.loc[s,'decisions'])})
    score_changes=read('score_changes_vs_A');change_C=score_changes[score_changes.scenario.eq('C')].iloc[0]
    section('Qué observaciones cambia la reconstrucción',table(pd.DataFrame(observation_changes))+f'<p>C no sólo resta ejemplos: recupera las respuestas a tarjetas favorables asignadas válidas que A omitía globalmente. Frente a A pierde {integer(change_C.lost_teachers)} registros docentes y recupera {integer(change_C.gained_teachers)}. Entre los {integer(change_C.shared_teachers)} docentes comunes, el {fmt(100*change_C.changed_tercile,3)} % cambia de tercil y el cambio absoluto medio de Hb es {fmt(100*change_C.mean_absolute_delta,4)} pp. Estas comparaciones de Hb usan las referencias propias de cada muestra nativa.</p>')
    attrrows=[]
    for term,label in ATTR.items():
        row={'Atributo':label}
        for s in MAIN:
            r=value(s,term);row[s]=f'{fmt(100*r.estimate)} [{fmt(100*r.lo)}; {fmt(100*r.hi)}]'
        row['C − A (pp)']=fmt(100*(value('C',term).estimate-value('A',term).estimate));attrrows.append(row)
    interaction_rows=[]
    for term,label in INTERACTIONS.items():
        for s in MAIN:
            r=value(s,term,'academic_interactions')
            interaction_rows.append({'Escenario':s,'Interacción':label,'Coeficiente (pp)':fmt(100*r.estimate),'SE (pp)':fmt(100*r.se),'IC 95 % (pp)':f'[{fmt(100*r.lo)}; {fmt(100*r.hi)}]','p Holm':f'{r.p_holm:.3g}'})
    int_C=value('C','failed:disruptive','academic_interactions');int_A=value('A','failed:disruptive','academic_interactions')
    section('Efectos within-teacher e interacciones',table(pd.DataFrame(attrrows))+pic('attribute_comparison','Coeficientes del modelo aditivo en puntos porcentuales e intervalos del 95 % con errores agrupados por docente.')+'<p>El modelo aditivo incluye los seis atributos y efectos fijos docentes. El modelo de interacciones añade los tres términos especificados abajo, con ajuste de Holm sobre esas tres pruebas. El soporte y rango de matrices se verifican por escenario; no se añaden ni retiran términos para aproximar resultados.</p>'+table(pd.DataFrame(interaction_rows))+f'<p>La revisión también afecta a las interacciones conductuales. En C, suspensos × disciplina es {fmt(100*int_C.estimate)} pp (p Holm {fmt(int_C.p_holm,4)}), frente a {fmt(100*int_A.estimate)} pp en A (p Holm {fmt(int_A.p_holm,3)}). D muestra interacciones positivas con absentismo y disciplina en una composición de docentes distinta. La comparación emparejada presenta la incertidumbre del cambio; comparar niveles de significación de dos modelos no prueba por sí solo una diferencia.</p>')
    acad=pd.concat([read('academic_categories',s).assign(Escenario=s) for s in MAIN]);acad=acad[['Escenario','academic','decisions','estimate','se','disagreement']];acad.columns=['Escenario','Perfil académico','Decisiones','Repetición (%)','SE tasa (pp)','Desacuerdo (%)'];acad['Decisiones']=acad['Decisiones'].map(integer)
    for c in ['Repetición (%)','SE tasa (pp)','Desacuerdo (%)']:acad[c]=acad[c].map(lambda v:fmt(100*v))
    section('Perfiles académicos y desacuerdo',table(acad)+'<p>Las tasas son medias observadas de cada categoría, con SE agrupados por docente. El desacuerdo compara dos docentes distintos ante el mismo identificador y pondera por frecuencia. No estandariza los otros atributos ni mide por sí solo persistencia, arbitrariedad o calidad de la decisión.</p>')
    shared=read('shared_teacher_summaries')
    shared_show=shared[['contrast','source','teachers','decisions','mean_repeat','mean_hb','sd_hb']].copy()
    shared_show.columns=['Comparación','Respuestas de','Docentes comunes','Decisiones','Repetición (%)','Media Hb (pp)','SD Hb (pp)']
    shared_show['Comparación']=shared_show['Comparación'].str.replace(' versus ',' frente a ',regex=False)
    for column in ['Docentes comunes','Decisiones']:shared_show[column]=shared_show[column].map(integer)
    for column in ['Repetición (%)','Media Hb (pp)','SD Hb (pp)']:shared_show[column]=shared_show[column].map(lambda v:fmt(100*v))
    shared_CD=shared[shared.contrast.eq('C versus D')].set_index('source')
    cd_columns=['teachers','decisions','mean_repeat','mean_hb','sd_hb','common_teachers']
    assert shared_CD.loc['C',cd_columns].eq(shared_CD.loc['D',cd_columns]).all(),'C-D shared summaries differ'
    cd=shared_CD.loc['C']
    shared_AC=shared[shared.contrast.eq('A versus C')].set_index('source')
    section('Qué cambia en la dispersión de harshness',f'<p>La SD de Hb en las muestras nativas pasa de {fmt(100*A.sd_hb)} pp (A) a {fmt(100*C.sd_hb)} pp (C). En cambio, al conservar los mismos docentes y reconstruir las referencias, pasa de {fmt(100*shared_AC.loc["A","sd_hb"])} a {fmt(100*shared_AC.loc["C","sd_hb"])} pp. La reducción total combina cambios de muestra y de respuestas; no debe atribuirse íntegramente a retirar ejemplos.</p>')
    section('Comparaciones en los mismos docentes',table(shared_show)+f'<p>Cada par se restringe a sus registros docentes comunes y reconstruye sus referencias y Hb en esa población. Esto separa la composición de docentes del cambio de respuestas utilizadas, sin convertir las diferencias en efectos causales.</p><p>En la intersección C–D, ambas fuentes contienen exactamente {integer(cd.teachers)} registros y {integer(cd.decisions)} respuestas: coinciden la tasa de repetición ({fmt(100*cd.mean_repeat)} %), la media de Hb ({fmt(100*cd.mean_hb)} pp) y su desviación estándar ({fmt(100*cd.sd_hb)} pp) al recalcular las referencias en ese mismo conjunto. D no retira respuestas adicionales dentro de estos docentes. Las diferencias entre las muestras nativas C y D reflejan la exclusión de otros {integer(C.teachers-summaries.loc["D","teachers"])} registros, la mezcla resultante de tarjetas y la reconstrucción de referencias. Es una sensibilidad de composición, no una segunda corrección de las respuestas de los docentes comunes.</p>')
    hr=summaries.loc[MAIN,['mean_repeat','mean_teacher_repeat','mean_hb','sd_hb','q10','q90','tercile_low','tercile_high']].reset_index();hr.columns=['Escenario','Repetición ponderada por respuesta (%)','Media de tasas docentes (%)','Media Hb (pp)','SD Hb (pp)','P10 Hb (pp)','P90 Hb (pp)','Corte tercil 1 (pp)','Corte tercil 2 (pp)']
    for c in hr.columns[1:]:hr[c]=hr[c].map(lambda v:fmt(100*v))
    changes=score_changes[score_changes.scenario.isin(MAIN)];show=changes[['scenario','shared_teachers','lost_teachers','gained_teachers','correlation','mean_absolute_delta','changed_tercile']].copy();show.columns=['Escenario','Docentes comunes','Pierde frente A','Recupera frente A','Correlación Hb','Cambio absoluto medio (pp)','Cambia tercil (%)']
    show['Correlación Hb']=show['Correlación Hb'].map(lambda v:fmt(v,3))
    for column in ['Cambio absoluto medio (pp)','Cambia tercil (%)']:show[column]=show[column].map(lambda v:fmt(100*v))
    section('Harshness, definiciones alternativas y terciles',table(hr)+pic('harshness_comparison','Distribuciones de Hb con clases comunes de 12,5 puntos de harshness centradas en cero. El eje vertical muestra porcentajes de docentes por clase; no son intervalos de confianza.')+table(show)+'<p>Las referencias leave-one-out, el harshness, sus terciles y Hc/Hd se recalculan para cada escenario. La media de Hb ponderada por respuestas es cero por construcción; su media no ponderada por docente no tiene que serlo. Las regresiones por tercil siguen siendo descriptivas porque el grupo se construye con las mismas decisiones.</p>'+table(pd.DataFrame([{'Escenario':s,'Tercil':r.tercile,'Docentes':integer(r.n),'Media Hb (pp)':fmt(100*r.mean_hb),'Suspensos (pp)':fmt(100*value(s,'failed','attributes_by_tercile',r.tercile).estimate)}for s in MAIN for r in read('tercile_descriptives',s).itertuples()]))+'<p>Las tablas completas de los tres terciles, sus seis pendientes y las cuatro definiciones alternativas están disponibles en los CSV por escenario.</p>')
    chars=[]
    for s in MAIN:
        for term,label in [('levelSecondary','Secundaria'),('edad','Edad'),('experiencia','Experiencia'),('resources_excess','Recursos excesivos')]:
            r=value(s,term,'characterization','X3_common');chars.append({'Escenario':s,'Variable':label,'Coeficiente (pp)':fmt(100*r.estimate),'HC3 SE (pp)':fmt(100*r.se),'Docentes con X3 completo':integer(r.n),'R² (%)':fmt(100*r.r2)})
    section('Características y creencias docentes',table(pd.DataFrame(chars))+'<p>Los modelos X1–X3 usan la misma muestra completa dentro de cada escenario. Se mantiene HC3. El bootstrap adicional de 999 sorteos vuelve a construir las referencias y Hb. Las creencias son posteriores a las decisiones y sus asociaciones no identifican mecanismos causales.</p>')
    decomp=[]
    for comp,label in COMP.items():
        row={'Componente':label}
        for s in MAIN:
            p=read('decomposition',s)if usable[s]else read('decomposition_point',s);r=p[p.component.eq(comp)].iloc[0]
            row[s]=fmt(100*r.share)+(f' (SE {fmt(100*r.se)})'if usable[s]else ' · incertidumbre pendiente')
        row['C − A (pp)']=fmt(100*(parts['C'][comp]-parts['A'][comp]));decomp.append(row)
    diagrows=[]
    for s in MAIN:
        d=diagnostics[s];b=status[s];diagrows.append({'Escenario':s,'Rango fijo':d['fixed_rank'],'Parámetros covarianza':d['covariance_parameters'],'Singular':bool(d['diagnostics']['singular']),'Gradiente combinado':fmt(d['diagnostics']['gradient'],6),'Hessiana mín.':fmt(d['diagnostics']['min_hessian'],3),'Fuera [0,1] (%)':fmt(100*d['outside_01']),'Bootstrap aceptadas':f"{b['accepted']}/199"if usable[s]else('v1 descartado; refit pendiente'if discarded[s]else'pendiente'),'Réplicas singulares':b.get('boundary_accepted','pendiente')if usable[s]else'pendiente'})
    pending=', '.join(s for s in MAIN if not usable[s])
    uncertainty_note=f'<p><strong>Estado provisional:</strong> se muestran los puntos de A–D; la incertidumbre de {pending} permanece pendiente.</p>'if pending else''
    if discarded['B']:uncertainty_note+='<p>El bootstrap B v1 se descartó porque la auditoría de inicialización encontró soluciones con mayor verosimilitud. Sus SE anteriores no se muestran ni se cuentan como réplicas aceptadas para este informe; B debe completarse con la nueva configuración v2.</p>'
    section('Descomposición y estado de su incertidumbre',table(pd.DataFrame(decomp))+pic('decomposition_comparison','Cuotas puntuales de la varianza implícita en el modelo, en porcentaje. Mismo modelo y reparto en todos los escenarios; los puntos B/C/D están en la frontera de la covarianza.')+table(pd.DataFrame(diagrows))+uncertainty_note+'''<p>Se mantienen seis atributos y sus 15 interacciones, X1, las creencias añadidas a X1 (X3 menos X1), Hb observado y seis pendientes correlacionadas centradas en las decisiones de docentes con X3 completo. No se introduce intercepto aleatorio ni bloque de ramas experimentales. Se usa REML y reparto Shapley por proyección del mismo predictor fijo. También se exportan los componentes sin normalizar y el denominador.</p>
    <p><strong>Singularidad:</strong> la matriz de covarianzas corregida está en la frontera. Esto no equivale a que el optimizador no haya encontrado una solución, pero impide describirla como seis dimensiones aleatorias no degeneradas. Se conserva la especificación. El bootstrap de 199 sorteos por escenario describe la dispersión del estimador, reconstruyendo referencias y Hb y refitando el modelo; los rangos percentiles no acreditan cobertura nominal en la frontera. Se registran fallos, avisos y ajustes singulares, sin sustituir semillas para conseguir éxitos.</p>
    <p>Las copias de un docente remuestreado reciben identificadores de cluster distintos. La referencia resta una copia de su respuesta. Se comprueba algebraicamente esta construcción frente a una expansión explícita. A y las réplicas v1 conservadas requieren código de optimizador cero y cuotas finitas; no verifican Hessiana en cada réplica. Las nuevas réplicas v2 usan inicialización automática y tolerancias estándar, y requieren además gradiente combinado inferior a 0,002 y Hessiana positiva, con un intento BOBYQA si hace falta. B v1 se descarta por obtener menor log-verosimilitud REML que las soluciones alternativas auditadas; sus 199 réplicas se vuelven a estimar. En C y D se conservan las réplicas v1 auditadas y se identifica la configuración de cada réplica restante. No se afirma que todas las réplicas tengan Hessiana verificada. La auditoría de inicializaciones y tolerancias se documenta en <a href="tables/optimizer_control_audit.csv">optimizer_control_audit.csv</a> y en los estados por escenario.</p>
    <p>Harshness se construye con las propias respuestas. El reparto describe estructura dentro de muestra y no es exactitud predictiva, atribución causal, estabilidad temporal ni un criterio para clasificar profesores. Los porcentajes fuera de [0,1] son predicciones condicionales del modelo lineal y no se recortan para descomponer.</p>''')
    if (OUT/'tables/paired_differences.csv').exists():
        pair=read('paired_differences');p=pair[pair.contrast.eq('C - A')&pair.metric.str.startswith(('attribute:','interaction:'))].copy();p['Métrica']=p.metric.map(lambda x:ATTR.get(x.removeprefix('attribute:'),INTERACTIONS.get(x.removeprefix('interaction:'),x)));p['Cambio (pp)']=p.delta.map(lambda v:fmt(100*v));p['Rango bootstrap 95 % (pp)']=[f'[{fmt(100*l)}; {fmt(100*h)}]'for l,h in zip(p.lo,p.hi)];p['Sorteos válidos']=p.successful
        section('Cuánto cambia: comparación emparejada',table(p[['Métrica','Cambio (pp)','Rango bootstrap 95 % (pp)','Sorteos válidos']])+'<p>Se usan los mismos sorteos de registros docentes origen para A–D, con 999 repeticiones y semilla 20261020. Las referencias se vuelven a calcular dentro de cada escenario. Los tamaños realizados pueden variar en escenarios anidados. Se comparan cambios de estimación, no “significativo frente a no significativo”. El CSV incluye B−A, C−A, D−A, C−B y D−C. Las diferencias de cuotas del modelo mixto se presentan descriptivamente; sus SE marginales no se convierten en SE de diferencias.</p>')
    sens=summaries.loc[ALL,['teachers','decisions','common_teachers','mean_repeat','sd_hb','r2_full']].reset_index();sens['scenario']=sens.scenario.map(LABEL);sens.columns=['Escenario','Docentes','Decisiones','Docentes con X3 completo','Repetición (%)','SD Hb (pp)','R² X3 (%)']
    for c in sens.columns[1:4]:sens[c]=sens[c].map(integer)
    for c in sens.columns[4:]:sens[c]=sens[c].map(lambda v:fmt(100*v))
    section('Sensibilidades de completitud y filtro',table(sens)+table(read('screening_rule_comparison'))+'''<p><strong>Ocho guardados:</strong> B_full8 y C_full8 condicionan a disponer de ocho valores guardados en las posiciones asignadas; no se interpretan como prueba de recontestación ni se fuerzan ocho decisiones en C. Son sensibilidades de selección y no sustituyen las muestras parciales.</p>
    <p><strong>Filtro:</strong> en el conjunto bruto, el filtro vigente excluye 389 registros y el favorable específico de Ejemplo excluye 379. C_example_screen sustituye el cribado vigente de cuatro campos por el favorable específico de Ejemplo. Frente a C recupera diez registros con resultado, 79 respuestas y siete docentes con X3 completo. La procedencia del campo usado para cribar sigue siendo ambigua si se reutilizó. C_inclusive elimina todo filtro basado en esas decisiones. Ambas sensibilidades reconstruyen los resultados y mantienen separado el problema del cribado.</p>
    <p>Las sensibilidades suplementarias tienen estimaciones puntuales y diagnósticos del mismo modelo mixto; el remuestreo intensivo de descomposición se concentra en A–D. Todas tienen HC3 y bootstrap de caracterización nuevos. Sus pendientes, soporte, tasas y grupos se exportan por separado. No se usan SE antiguos para estimaciones nuevas.</p>''')
    seq=pd.DataFrame([dict(Escenario=s,**read('sequence_summary',s).iloc[0].to_dict())for s in MAIN]);seq=seq[['Escenario','complete_eight','observed_switch','permutation_mean','p']];seq.columns=['Escenario','Secuencias con 8 valores usados','Cambios observados (%)','Referencia de permutación (%)','p bilateral']
    seq['Secuencias con 8 valores usados']=seq['Secuencias con 8 valores usados'].map(integer)
    for column in ['Cambios observados (%)','Referencia de permutación (%)']:seq[column]=seq[column].map(lambda v:fmt(100*v))
    seq['p bilateral']=seq['p bilateral'].map(lambda v:fmt(v,3))
    split_rows=[]
    for s in MAIN:
        r=read('split_half_summary',s).iloc[0]
        split_rows.append({'Escenario':s,'Docentes elegibles':integer(r.teachers),'Mínimo de respuestas':integer(r.min_responses),'Particiones':integer(r.partitions),'Correlación media':fmt(r['mean'],4),'Rango central de particiones':f'[{fmt(r.p025,4)}; {fmt(r.p975,4)}]'})
    section('Consistencia entre mitades de respuestas',table(pd.DataFrame(split_rows))+'<p>Se usan 1.000 particiones de respuestas y referencias externas disjuntas, con las semillas documentadas por escenario. La correlación compara mitades del mismo encuentro. El rango central describe sensibilidad a particiones; no es un intervalo de confianza muestral ni evidencia de estabilidad temporal. La composición de docentes elegibles y las referencias cambian entre escenarios.</p>')
    permutation_A=read('sequence_summary','A').iloc[0].p
    section('Participación, progresión y apéndices',pic('progression_comparison','La referencia actual usa órdenes brutos y excluye seis IDs globalmente; B/C/D usan sus elegibles. Denominadores distintos, identificados explícitamente.')+table(seq)+f'''<p>Se reconstruye el denominador de participación desde experiencia: 4.037 registros, apartando 905 entradas mínimas. Las marcas Grupo1–Grupo8 indican cierre del bloque; B2_complet y finalizar encuesta son indicadores distintos. Se mantienen casos parciales. Se exportan el flujo de elegibilidad y resultado por escenario, composición poblacional, disponibilidad de creencias y covariables, tiempos y progresión.</p>
    <p>En C se eliminan coincidencias específicas del registro, en vez de retirar seis identificadores globalmente. El cambio de denominador temporal se presenta aparte de los resultados analíticos. Los tiempos son envíos de página, no lectura activa ni prueba de fatiga. Las transiciones adyacentes sólo cuentan posiciones consecutivas; la permutación se aplica a secuencias completas con ocho resultados utilizados. El nuevo cálculo Monte Carlo para A da p = {fmt(permutation_A,3)}, frente a 0,397 en el manuscrito; ambos mantienen la misma lectura de falta de evidencia de compensación. No se afirma identidad de sorteos con la simulación archivada.</p>
    <p>La comparación poblacional conserva la fuente 2023/24 y los márgenes del informe vigente. El código postal de los registros retenidos usa la limpieza actual; los recuperados al retirar el filtro sólo disponen del código declarado sin esa corrección. Las provincias de esa sensibilidad requieren esa cautela adicional.</p>''')
    conflict=lambda s:read('academic_categories',s).query("academic == 'Many failures / adequate competence'").iloc[0]
    conclusions=pd.DataFrame([
        ['Suspensos y competencias dominan','Se mantiene',f"C: {fmt(100*value('C','failed').estimate)} y {fmt(100*value('C','competence').estimate)} pp"],
        ['Absentismo y disciplina añaden información','Se mantiene con cambios de magnitud',f"C: {fmt(100*value('C','absent').estimate)} y {fmt(100*value('C','disruptive').estimate)} pp"],
        ['Interacciones de suspensos con conducta','Se refuerzan en C; D cambia la composición',f"C: suspensos × disciplina {fmt(100*int_C.estimate)} pp, p Holm {fmt(int_C.p_holm,4)}; comparar cambios con su incertidumbre emparejada"],
        ['Contexto complejo modera la repetición','Se mantiene en C; menor magnitud',f"A {fmt(100*value('A','background').estimate)}; C {fmt(100*value('C','background').estimate)} pp"],
        ['Género sin efecto medio detectable','Se mantiene con incertidumbre','No equivale a demostrar ausencia de sesgos'],
        ['Mayor desacuerdo en suspensos con competencias adecuadas','Se mantiene',f"C: {fmt(100*conflict('C').disagreement)} % de pares"],
        ['X1–X3 explican menos del 5 % de Hb observado','Se mantiene en A–D',f"C: R² {fmt(100*C.r2_full)} %"],
        ['Distribución y terciles de harshness','Necesitan actualización','Cambian referencias, población y composición de tarjetas'],
        ['Cuotas exactas y matriz no singular del modelo actual','Requieren revisión sustantiva','B/C/D convergen con covarianza singular; los porcentajes son descriptivos'],
        ['Flujo, recuentos y apéndices','Necesitan actualización','Separar ejemplos, asignación, valores guardados y cierre del bloque']
    ],columns=['Afirmación','Balance','Evidencia o límite'])
    section('Qué conservaría y qué revisaría del manuscrito',table(conclusions)+'<p>Esta comparación no ha editado el paper A, el B ni sus PDF. C es la referencia recomendada para evaluar la retirada de respuestas ambiguas con las exclusiones vigentes. La sensibilidad del filtro debe decidirse con su justificación sustantiva y la evidencia de procedencia; D no se elige por su parecido con A ni por conservar resultados significativos.</p>')
    ver=read('verification_checks');passed=int(ver.passed.sum())
    links=''.join(f'<li><a href="{s}/summary.csv">{LABEL[s]}: resumen</a> · <a href="{s}/attributes.csv">atributos</a> · <a href="{s}/characterization.csv">caracterización</a> · <a href="{s}/decomposition_point.csv">descomposición</a></li>'for s in ALL)
    section('Verificación, reproducibilidad y archivos',f'<p>{passed} comprobaciones numéricas y de procedencia superadas. A se contrasta por claves con los resultados congelados. Se verifican referencias, expansión de clones, rangos, convergencia y sumas. R y lme4 se fijan también en los procesos paralelos. Se guardan semillas, versiones, huellas y estados de réplica.</p><ul>{links}</ul><p><a href="tables/verification_checks.csv">Verificaciones</a> · <a href="tables/paired_differences.csv">Diferencias emparejadas</a> · <a href="tables/participation_comparison.csv">Participación</a> · <a href="tables/screening_fields.csv">Cribado</a> · <a href="tables/card_mapping.csv">Mapeo de tarjetas</a> · <a href="tables/bootstrap_start_audit.csv">Auditoría de inicialización</a> · <a href="protection_verification.json">Integridad de archivos protegidos</a></p><p>Código aislado: <code>2. Code/paper_A_reconstruction/</code>. Datos por registro, secuencias y modelos: subcarpeta <code>local/</code>, excluida de Git. Sólo se versionan código, copias documentales y resultados agregados.</p>')
    title='Reconstrucción del paper A: tarjetas experimentales y comparación'
    doc=f'''<!doctype html><html lang="es"><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1"><title>{title}</title><style>
    :root{{--ink:#20313b;--muted:#52636c;--accent:#215e70}}*{{box-sizing:border-box}}body{{margin:0;background:#f3f5f6;color:var(--ink);font:16px/1.6 system-ui,sans-serif}}main{{max-width:1180px;margin:auto;padding:42px 32px 70px;background:white}}h1{{font-size:34px;line-height:1.2;max-width:950px}}h2{{font-size:23px;margin-top:45px;border-top:2px solid #dce5e9;padding-top:22px}}p{{max-width:1050px}}.meta,figcaption{{color:var(--muted);font-size:13px}}.lead{{font-size:19px}}.cards{{display:grid;grid-template-columns:repeat(3,1fr);gap:16px;margin:25px 0}}.cards>div{{padding:20px;border:1px solid #dce5e9;background:#f5f9fa}}.cards b{{display:block;font-size:20px;line-height:1.3;margin-bottom:10px}}.cards span{{font-size:14px}}.table-wrap{{overflow:auto;margin:20px 0}}table{{border-collapse:collapse;width:100%;font-size:13px;line-height:1.4}}th{{background:#eaf0f3;text-align:left}}th,td{{padding:10px 12px;border-bottom:1px solid #d9e1e4;vertical-align:top}}tr:nth-child(even){{background:#f8fafb}}figure{{margin:28px 0}}img{{max-width:100%;height:auto}}a{{color:#17647e}}code{{background:#f0f3f5;padding:2px 4px;font-size:13px}}@media(max-width:700px){{main{{padding:24px 16px}}.cards{{grid-template-columns:1fr}}h1{{font-size:27px}}}}@media print{{body{{background:white}}main{{padding:0;max-width:none}}h2{{break-after:avoid}}tr,img{{break-inside:avoid}}.table-wrap{{overflow:visible}}a{{color:inherit;text-decoration:none}}}}
    </style><main><h1>{title}</h1><p class="meta">20 de octubre de 2026 · {'Comparación completa'if final else'Informe provisional: cálculos en curso'} · Unidad: registro con ResponseId único, sin deduplicación entre personas.</p>{''.join(sections)}</main></html>'''
    (OUT/'informe_comparacion.html').write_text(doc,encoding='utf-8')
    print('Report built:',OUT/'informe_comparacion.html')

if __name__=='__main__':
    p=argparse.ArgumentParser();p.add_argument('--final',action='store_true');build(p.parse_args().final)
