"""Aggregate survey-exit audit; preserves current samples and manuscript assets.

Run from any directory. Only aggregate outputs are written to reports/.
The last recorded page is a lower bound on progress, not the page of exit.
"""
from pathlib import Path
import json
import re
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt

ROOT = Path(__file__).resolve().parents[1]
OUT = ROOT / '3. Output/paper_A/reports'
r = pd.read_parquet(ROOT / '1. Data/raw data/df_named.parquet')
clean = pd.read_parquet(ROOT / '1. Data/processed data/cleandata.parquet')
assert r.ResponseId.is_unique and clean.id.is_unique
cards = [c for c in r if re.fullmatch(r'Grupo [1-8](\.[1-8])?_1', c)]
assert len(cards) == 64 and 'Grupo 6_1' in cards
used = [c for c in cards if c not in ['Grupo 1.1_1', 'Grupo 2.1_1']]
screen = ['Grupo 1.1_1', 'Grupo 2.1_1', 'Grupo 5.1_1', 'Grupo 8.1_1']
answered = r[cards].isin(['pasa', 'repite'])
assert (answered == r[cards].notna()).all().all()
any_card = answered.any(axis=1)
analysis_card = answered[used].any(axis=1)
excluded = r[screen].eq('repite').any(axis=1)
sample = analysis_card & ~excluded
assert set(r.loc[~excluded, 'ResponseId']) == set(clean.id)
assert sample.sum() == 2705 and excluded.sum() == 389
assert not (excluded & ~any_card).any()
assert sum(answered.loc[sample, used].sum(axis=1)) == 22124
clean_used = [f'alumno_{i}_{j}' for i in range(1, 9) for j in range(1, 9)
              if (i, j) not in [(1, 1), (2, 1)]]
assert set(r.loc[sample, 'ResponseId']) == set(clean.loc[clean[clean_used].notna().any(axis=1), 'id'])

# Questionnaire order: merge branch alternatives into common conceptual stages.
# For each timing block, take both its submit field and its substantive answers.
# This avoids interpreting an unshown branch or an unanswered optional item as exit.
def block(timer):
    i = r.columns.get_loc(timer)
    j = i + 1
    while j < len(r.columns) and not ('_First Click' in r.columns[j]):
        j += 1
    return [c for c in r.columns[i:j]
            if c == timer or not any(x in c for x in
                ['_Click Count', '_DO_', '_Browser', '_Version', '_Operating System', '_Resolution'])]

stages = [
    ('Introducción/consentimiento', ['time_T0.1_Page Submit']),
    ('Etapa educativa', ['time_1_Page Submit']),
    ('Cursos y especialidad', ['time_1aPrimaria_Page Submit', 'time_1aSecundaria_Page Submit', 'time_1bSecundaria_Page Submit']),
    ('Experiencia docente', ['time_2_Page Submit']),
    ('Antigüedad en el centro', ['time_3_Page Submit']),
    ('Centros anteriores', ['time_4_Page Submit']),
    ('Género del docente', ['time_5_Page Submit']),
    ('Edad', ['time_6_Page Submit']),
    ('Tutoría: sí/no', ['time_7_Page Submit']),
    ('Tutoría: curso del grupo', ['time_8a1_Page Submit']),
    ('Tutoría: número de alumnos', ['time_8a2_Page Submit']),
    ('Tutoría: posibles repetidores', ['time_8a3_Page Submit']),
    ('Tutoría: repetidores previos', ['time_8a4_Page Submit']),
    ('Código postal del centro', ['time_8C.Posta_Page Submit']),
    ('Titularidad del centro', ['time_9_Page Submit']),
    ('Situación laboral (rama pública)', ['time_10Publico_Page Submit']),
    ('Enseñanzas del centro', ['time_11_Page Submit']),
    ('Líneas de primaria/secundaria', ['time_12A_Page Submit', 'time_12B_Page Submit']),
    ('Grupos a los que enseña', ['time_13_Page Submit']),
    ('Alumnos en el grupo mayor', ['time_14_Page Submit']),
    ('Horas de guardia', ['time_15_Page Submit']),
    ('Alumnos absentistas', ['time_16_Page Submit']),
    ('Alumnos conflictivos', ['time_17_Page Submit']),
    ('Programas del centro', ['time_18_Page Submit']),
    ('Género de la dirección', ['time_19_Page Submit']),
    ('Experiencia de la dirección', ['time_20_Page Submit...198']),
    ('Porcentaje de desfavorecidos', ['time_21_Page Submit...203']),
    ('Pantalla de instrucciones posterior', ['time_inst_rankingfin_Page Submit']),
]
seen = []
mapping = []
for n, (label, timers) in enumerate(stages, 1):
    fields = list(dict.fromkeys(c for timer in timers for c in block(timer)))
    # Last instruction block ends before the first card. No embedded fields included.
    seen.append(r[fields].notna().any(axis=1).to_numpy())
    mapping.append(dict(order=n, stage=label, fields=' | '.join(fields)))
seen = np.column_stack(seen)
last = np.where(seen, np.arange(1, len(stages)+1), 0).max(axis=1)
no_card = ~any_card
assert (last[any_card] == len(stages)).all()
assert not r.loc[no_card, ['time_presentacion_p_Page Submit', 'time_R1_Page Submit',
                           'time_iniciobloque3_Page Submit']].notna().any().any()
labels = ['Sin envío de la introducción ni respuestas'] + [s[0] for s in stages]
dur = pd.to_numeric(r['Duration (in seconds)'], errors='coerce')
detail = []
for i, label in enumerate(labels):
    m = no_card & (last == i)
    detail.append(dict(order=i, last_recorded_stage=label, n=int(m.sum()),
                       pct_no_card=100*m.sum()/no_card.sum(),
                       elapsed_median_seconds=dur[m].median()))
pd.DataFrame(detail).to_csv(OUT/'survey_exit_last_stage.csv', index=False)
pd.DataFrame(mapping).to_csv(OUT/'survey_exit_field_map.csv', index=False)

groups = [
    ('Sin enviar la introducción', last == 0),
    ('Solo introducción, sin etapa educativa', last == 1),
    ('Etapa/cursos/especialidad, sin experiencia', (last >= 2) & (last <= 3)),
    ('Datos personales y tutoría', (last >= 4) & (last <= 13)),
    ('Datos del centro y alumnado', (last >= 14) & (last <= 26)),
    ('Fin del bloque inicial / instrucciones', last >= 27),
]
summary = []
for label, mask in groups:
    m = mask & no_card
    summary.append(dict(stage=label, n=int(m.sum()), pct_no_card=100*m.sum()/no_card.sum(),
                        pct_raw=100*m.sum()/len(r),
                        median_elapsed_minutes=dur[m].median()/60,
                        p25_elapsed_minutes=dur[m].quantile(.25)/60,
                        p75_elapsed_minutes=dur[m].quantile(.75)/60,
                        finished=int(r.loc[m,'Finished'].eq('True').sum())))
summary = pd.DataFrame(summary)
assert summary.n.sum() == no_card.sum()
summary.to_csv(OUT/'survey_exit_groups.csv', index=False)

# Universal milestones, using later progress to handle gaps in individual fields.
milestones = [('Registros de encuesta', np.ones(len(r), dtype=bool)),
              ('Introducción enviada o avance posterior', last >= 1),
              ('Etapa educativa o avance posterior', last >= 2),
              ('Experiencia o avance posterior', last >= 4),
              ('Código postal o avance posterior', last >= 14),
              ('Fin del bloque inicial o avance posterior', last >= 27),
              ('Instrucciones posteriores o tarjetas', last >= 28),
              ('Alguna decisión registrada (incluye ejemplos)', any_card)]
mil = []
previous = len(r)
for label, mask in milestones:
    n = int(np.sum(mask))
    mil.append(dict(stage=label, n=n, lost_since_previous=previous-n,
                    lost_pct_previous=100*(previous-n)/previous))
    previous = n
pd.DataFrame(mil).to_csv(OUT/'survey_exit_milestones.csv', index=False)

flow = dict(raw=len(r), no_recorded_card=int(no_card.sum()), any_recorded_card=int(any_card.sum()),
            screen_exclusions=int(excluded.sum()), after_screen=int((any_card & ~excluded).sum()),
            only_nonanalysis_fields=int((any_card & ~analysis_card & ~excluded).sum()),
            analysis_sample=int(sample.sum()), screen_with_analysis_fields=int((excluded & analysis_card).sum()),
            screen_without_analysis_fields=int((excluded & ~analysis_card).sum()),
            sample_finished=int((sample & r.Finished.eq('True')).sum()),
            sample_not_finished=int((sample & ~r.Finished.eq('True')).sum()),
            excluded_finished=int((excluded & r.Finished.eq('True')).sum()))
final_module = r['time_iniciobloque3_Page Submit'].notna()
finished = r.Finished.eq('True')
assert not (finished & ~final_module).any()
flow.update(sample_final_module=int((sample & final_module).sum()),
            sample_before_final_module=int((sample & ~final_module).sum()),
            sample_final_module_unfinished=int((sample & final_module & ~finished).sum()))
assert flow['raw'] == flow['no_recorded_card'] + flow['any_recorded_card']
assert flow['any_recorded_card'] == flow['screen_exclusions'] + flow['only_nonanalysis_fields'] + flow['analysis_sample']
assert flow['no_recorded_card'] + flow['only_nonanalysis_fields'] == 1848
# The author-approved participation denominator excludes minimal starts, not
# respondents inferred to have any particular intention or a duplicate identity.
engaged = last >= 4
group_end = r[[f'Grupo{i}' for i in range(1,9)]].notna().any(axis=1)
assert not (group_end & ~any_card).any()
assert not (finished & ~group_end).any()
# Validate the end-of-card-block marker against the assigned-card fields.
# The marker indicates reaching the end, not answering every single card.
source = (ROOT/'2. Code/1.limpieza_datos.R').read_text(encoding='utf8')
card_map = {f'{g}.{j}': c for g,j,c in re.findall(r'alumno_(\d)_(\d) = `([^`]+)`',source)}
assert len(card_map) == 64
assigned_n = pd.Series(0,index=r.index)
assigned_answers = assigned_n.copy()
for c in r:
    match = re.fullmatch(r'FL_\d+_DO_Grupo([1-8]\.[1-8])',c)
    if match:
        assigned_n += r[c].notna().astype(int)
        assigned_answers += (r[c].notna() & r[card_map[match[1]]].notna()).astype(int)
assert (assigned_n[group_end] == 8).all()
assert not ((assigned_answers == 8) & ~group_end).any()
pd.crosstab([assigned_n,assigned_answers],group_end).to_csv(OUT/'survey_exit_end_marker_check.csv')
before = engaged & ~any_card
during = any_card & ~group_end
passed_end = group_end & ~excluded
after = passed_end & ~finished
flow.update(minimal_starts=int((~engaged).sum()), participation_denominator=int(engaged.sum()),
            before_decisions=int(before.sum()), during_decisions=int(during.sum()),
            during_also_failed_filter=int((during & excluded).sum()),
            reached_decision_end=int(group_end.sum()),
            filter_exclusions_at_decision_end=int((group_end & excluded).sum()),
            decision_end_passed_filter=int(passed_end.sum()),
            after_decisions=int(after.sum()), completed_passed_filter=int((passed_end & finished).sum()),
            end_marker_with_missing_card_answers=int((group_end & (assigned_answers<8)).sum()))
assert flow['minimal_starts'] == 905 and flow['participation_denominator'] == 4037
assert engaged.sum() == before.sum() + during.sum() + (group_end & excluded).sum() + after.sum() + (passed_end & finished).sum()
assert excluded.sum() == (during & excluded).sum() + (group_end & excluded).sum()
(OUT/'survey_exit_audit.json').write_text(json.dumps(flow, indent=2), encoding='utf8')

# Flag availability is descriptive: these fields are not necessarily page submissions.
flags = ['Intro','consentimiento_pass','started','B1_end','Treatment','B2_complet','end']
pd.DataFrame([dict(cohort=name, n=int(mask.sum()), **{c:int(r.loc[mask,c].notna().sum()) for c in flags})
              for name,mask in [('no_card',no_card),('screen_excluded',excluded),('analysis_sample',sample)]]
             ).to_csv(OUT/'survey_exit_flags.csv', index=False)

# Independent timing summary, not a proxy for active time or a diagnosis of fatigue.
pd.DataFrame([dict(cohort=name, n=int(mask.sum()), **{
    f'elapsed_q{int(q*100)}_seconds':float(dur[mask].quantile(q)) for q in [.25,.5,.75,.9]},
    over_24h=int((dur[mask]>86400).sum())) for name, mask in
    [('no_card',no_card),('screen_excluded',excluded),('analysis_sample',sample)]]
).to_csv(OUT/'survey_exit_elapsed.csv', index=False)

plt.rcParams.update({'font.family':'DejaVu Sans','font.size':11,'axes.spines.top':False,'axes.spines.right':False})
fig, ax = plt.subplots(figsize=(10.5,4.8))
y = np.arange(len(summary))
ax.barh(y,summary.n,color=['#aebac0','#aebac0','#5c8b98','#215e70','#215e70','#b46630'])
for i,row in summary.iterrows():
    ax.text(row.n+8,i,f'{row.n:,} ({row.pct_no_card:.1f} %)'.replace(',', '.'),va='center')
ax.set(yticks=y,yticklabels=summary.stage,xlim=(0,summary.n.max()*1.3),xlabel='Registros sin ninguna decisión en tarjetas')
ax.invert_yaxis()
ax.set_title(f'Último avance registrado antes de las tarjetas (n = {no_card.sum():,})'.replace(',', '.'),loc='left',fontweight='bold')
fig.text(.02,.015,'Incluye ejemplos y filtros al identificar decisiones. Último registro ≠ pantalla exacta de abandono.',fontsize=9,color='#555555')
fig.tight_layout(rect=(0,.04,1,1))
fig.savefig(OUT/'survey_exit_stages.png',dpi=190,bbox_inches='tight');plt.close(fig)

from matplotlib.patches import FancyBboxPatch
fig,ax=plt.subplots(figsize=(11,9))
ax.set(xlim=(0,12),ylim=(.8,11));ax.axis('off')
def box(x,y,title,n,detail='',side=False):
    ax.add_patch(FancyBboxPatch((x-2.55,y-.58),5.1,1.16,boxstyle='round,pad=.04',
        facecolor='#f6ede5' if side else '#e9f0f3',edgecolor='#ae754b' if side else '#215e70',lw=1.2))
    ax.text(x,y+.25,title,ha='center',va='center',fontsize=11)
    ax.text(x,y-.06,f'{n:,}'.replace(',','.'),ha='center',va='center',fontsize=19,fontweight='bold',color='#215e70')
    if detail:ax.text(x,y-.40,detail,ha='center',va='center',fontsize=9,color='#555555')
def arrow(a,b):
    ax.annotate('',xy=b,xytext=a,arrowprops=dict(arrowstyle='->',lw=1.4,color='#215e70'))
main = [(10,'Empiezan a responder la encuesta',flow['participation_denominator'],'Desde la pregunta de experiencia docente'),
        (8,'Responden alguna tarjeta',flow['any_recorded_card'],'Incluye las tarjetas de ejemplo'),
        (6,'Llegan al final de las decisiones',flow['reached_decision_end'],''),
        (4,'Superan el filtro',flow['decision_end_passed_filter'],''),
        (2,'Terminan la encuesta',flow['completed_passed_filter'],'')]
branches = [(9,'Abandonan antes de las decisiones',flow['before_decisions'],''),
            (7,'Abandonan durante las decisiones',flow['during_decisions'],'31 de ellos también fallan el filtro'),
            (5,'Excluidos por fallar el filtro',flow['filter_exclusions_at_decision_end'],'Entre quienes llegan al final de las decisiones'),
            (3,'Abandonan después de las decisiones',flow['after_decisions'],'')]
for y,title,n,box_detail in main:box(2.8,y,title,n,box_detail)
for y,title,n,box_detail in branches:
    box(9,y,title,n,box_detail,True)
    arrow((2.8,y+.38),(2.8,y-.38));arrow((2.8,y),(6.4,y))
fig.suptitle('Participación y abandono de la encuesta',fontsize=17,fontweight='bold',y=.97)
fig.text(.04,.085,'Se apartan previamente 905 entradas con avance mínimo; no se cuentan como abandono en este flujo.',fontsize=10)
fig.text(.04,.058,'Total de fallos del filtro: 389 = 31 entre quienes abandonan durante las decisiones + 358 al final.',fontsize=10)
fig.text(.04,.031,'La muestra de análisis sigue siendo 2.705: también aprovecha decisiones de encuestas incompletas.',fontsize=10)
fig.subplots_adjust(top=.91,bottom=.12,left=.02,right=.98)
fig.savefig(OUT/'survey_flow_candidate.png',dpi=190,bbox_inches='tight');plt.close(fig)

def fmt(x, digits=1):
    return f'{x:.{digits}f}'.replace('.', ',')

report = '''ABANDONO Y AVANCE REGISTRADO EN LA ENCUESTA
Análisis complementario, 3 de octubre de 2026. No modifica muestras, manuscrito ni figuras canónicas.
Código reproducible: 2. Code/48. A_survey_exit_audit.py

CRITERIO DE PRESENTACIÓN ACTUALIZADO POR EL AUTOR
El diagrama empieza en 4.037 registros, después de apartar 905 entradas con avance mínimo.
El criterio operativo es haber llegado a la pregunta de experiencia docente o más allá.
Los 905 no se clasifican como agotamiento/abandono dentro de este flujo. De ellos, 167 sí responden
etapa/cursos/especialidad: por precisión se denominan avance mínimo, no ausencia absoluta de respuestas.
Una visita exploratoria o una posterior respuesta desde otro dispositivo son posibilidades, no hechos
verificados: no se ha hecho una identificación ni deduplicación entre personas/dispositivos.

FLUJO ACTUALIZADO
4.037 empiezan a responder
  - 934 abandonan antes de registrar ninguna decisión
3.103 responden alguna tarjeta (incluidos ejemplos)
  - 175 no alcanzan el final de las decisiones; 31 de ellos también fallan el filtro
2.928 llegan al final del bloque de decisiones
  - 358 fallan el filtro
2.570 llegan al final de las decisiones y superan el filtro
  - 216 no terminan la encuesta
2.354 terminan la encuesta y superan el filtro.
Las ramas son excluyentes: 934 + 175 + 358 + 216 + 2.354 = 4.037.
Se mantienen las 389 exclusiones originales: 31 en la rama durante + 358 en la rama filtro.
No restar 389 después de restar los 175: contaría dos veces a 31 participantes.
Los 216 abandonos posteriores se cuentan entre quienes superan el filtro; hay otros 45 entre los 358
excluidos por filtro, que se asignan a esa rama para evitar solapamiento. No se afirma que el filtro
expulsara automáticamente de la encuesta: es una regla de selección del análisis.

IDENTIFICACIÓN DEL FINAL DE LAS DECISIONES
Se usa la marca de cierre Grupo1--Grupo8: 2.928 tienen exactamente una de ellas.
Todos tienen ocho posiciones asignadas y toda persona con ocho respuestas en esos campos tiene marca
de cierre. Todas las encuestas terminadas y todos los registros B2_complet tienen marca de cierre.
75 de quienes llegan al final no tienen respuesta en los ocho campos asignados: final del bloque no
equivale a ocho respuestas completas. Por eso la caja dice llegan al final, no contestan las ocho tarjetas.
El indicador B2_complet (2.785) es posterior y no se utiliza para fijar el final de las tarjetas.
El CSV survey_exit_end_marker_check.csv conserva el contraste agregado de posiciones, respuestas y marca.
La muestra de análisis sigue siendo 2.705; el flujo muestra participación, no una nueva muestra de casos completos.
De los 144 que abandonan durante sin fallar el filtro, 135 aportan decisiones analizables y 9 solo los
campos excluidos del resultado. Por ello 2.570 + 135 = 2.705 y 2.354 + 216 + 135 = 2.705.

AUDITORÍA COMPLETA DE LAS ENTRADAS (FUERA DEL DIAGRAMA PRINCIPAL)
PREGUNTA Y RESULTADO PRINCIPAL
Se distingue una entrada que no supera la introducción de una respuesta parcial tras varias preguntas.
De 4.942 registros, 1.839 no tienen ninguna respuesta a tarjetas, contando ejemplos/filtros como respuestas.
Entre estos 1.839, 738 (40,1 %) no registran la primera pregunta sustantiva (etapa educativa).
Si se amplía la definición de arranque mínimo a etapa, cursos y especialidad, son 905 (49,2 %).
Los restantes 934 (50,8 %) sí han llegado a la pregunta de experiencia docente o más adelante.
No hay una única explicación: aproximadamente la mitad se pierde al arrancar y la otra mitad tras avanzar.

MÉTODO
La unidad es el registro con ResponseId único, no una persona única verificada entre distintas entradas.
Se utiliza el parquet original sin excluir previamente a quienes fallan el filtro.
Se reconstruye el último bloque con respuesta o campo Page Submit registrado, siguiendo el orden del cuestionario.
Se unen las alternativas de primaria/secundaria; las preguntas de tutoría y empleo público se tratan como ramas.
El CSV survey_exit_field_map.csv documenta exactamente los campos de cada etapa. No se exportan datos individuales.
El último envío indica hasta dónde hay evidencia de avance. La salida puede ocurrir en la siguiente pantalla,
sin que esta deje una respuesta guardada. Por eso no se atribuye el abandono a una pregunta específica.
El porcentaje Progress no se usa como reloj ni como número de preguntas: contiene saltos por las ramas.
El campo Intro presenta faltantes incluso entre personas que completan; se usa también el envío y avance posterior.

ÚLTIMO AVANCE ENTRE LOS 1.839 REGISTROS SIN TARJETAS
Etapa | n | % sin tarjetas | Mediana de duración registrada (minutos)
'''
for row in summary.itertuples():
    report += f'{row.stage} | {row.n} | {fmt(row.pct_no_card)} | {fmt(row.median_elapsed_minutes,2)}\n'
report += '''
La duración es tiempo transcurrido registrado por la plataforma, no tiempo de atención activa.
Hay 31 registros sin tarjetas con duración superior a 24 horas; se prefieren medianas y no se eliminan casos.
Los 550 sin envío inicial tienen mediana de 22 segundos; los 188 con solo introducción, de 28 segundos.
Los grupos que avanzan por datos personales/tutoría, centro y cierre del bloque tienen medianas de
2,32, 4,08 y 5,27 minutos, respectivamente. Esto es compatible con participación sustantiva antes de parar.

DETALLE ÚTIL DE LAS PÉRDIDAS POSTERIORES AL ARRANQUE
- 282 tienen su último registro entre la pregunta de tutoría sí/no y el final de sus preguntas derivadas.
  No significa que una pregunta de tutoría cause esas salidas: algunos no tenían que ver sus derivadas.
- 122 tienen su último registro en horas de guardia, antes de guardar la respuesta de absentismo.
- Otros 62 tienen como último registro absentismo, antes de guardar el de alumnado conflictivo.
- 198 llegan al final del bloque inicial: 27 se quedan en porcentaje de desfavorecidos y 171 registran
  también el campo time_inst_rankingfin_Page Submit, pero no guardan ninguna respuesta a tarjetas.
Estos son puntos de pérdida observada, no motivos de salida. No se puede distinguir fatiga, interrupción,
falta de tiempo o decisión de no continuar a partir de los campos disponibles.

RETENCIÓN CON DENOMINADORES COMPARABLES
4.942 entradas -> 4.392 con introducción enviada -> 4.204 con etapa educativa -> 4.037 con experiencia
-> 3.683 con código postal -> 3.301 al final del bloque inicial -> 3.274 tras instrucciones -> 3.103 con tarjetas.
Cada hito incluye evidencia de avance posterior. Los hitos son anidados; no se comparan ramas condicionales
como si todas las personas tuvieran que responderlas. De las 4.204 que empiezan las preguntas sustantivas,
1.101 (26,2 %) no llegan a guardar una decisión y 3.103 (73,8 %) sí lo hacen.
Entre quienes llegan a experiencia, 354 de 4.037 (8,8 %) se pierden antes del código postal.
Entre quienes llegan al código postal, 382 de 3.683 (10,4 %) se pierden antes del final del bloque inicial.
Entre quienes llegan a ese final, 198 de 3.301 (6,0 %) no llegan a ninguna tarjeta.

DESPUÉS DE ENTRAR EN LAS DECISIONES
De los 2.705 de la muestra analítica, 2.354 (87,0 %) tienen Finished=True y 351 no.
Entre estos últimos, 263 no registran el envío de la introducción del módulo final y 88 sí.
No terminar la encuesta no elimina sus decisiones ya disponibles. Tampoco equivale a carecer de creencias:
la falta de una creencia puede deberse a omisión/ramificación, no necesariamente a una salida.
Esta auditoría no redefine la completitud de las ocho tarjetas ni confunde ejemplos con tarjetas experimentales.

CONCILIACIÓN CON LA MUESTRA ANALÍTICA (NO ES EL NUEVO DIAGRAMA)
4.942 registros
  - 1.839 sin ninguna respuesta a tarjetas
  = 3.103 con alguna respuesta a tarjetas, incluidos ejemplos/filtros
  - 389 excluidos por la regla vigente del filtro
  = 2.714 con respuesta y sin exclusión
  - 9 con respuestas únicamente en los dos campos que no se utilizan como resultados
  = 2.705 profesores en el análisis de decisiones
  - 502 con alguna creencia seleccionada ausente
  = 2.203 con creencias completas
  - 46 con otras covariables ausentes
  = 2.157 con todas las covariables del análisis.

Los 389 excluidos sí responden alguna tarjeta; 313 incluso terminan toda la encuesta. No son abandonos.
382 de ellos tienen respuesta en algún campo utilizado para el análisis y 7 solo en campos fuera del análisis.
Por ello no se debe mover sin más la caja de 389 debajo de la caja de 2.705 ni debajo de una caja de 3.087
con al menos una decisión analizable. El nodo anterior adecuado es 3.103 con cualquier decisión registrada.
Los dos campos omitidos del resultado son Grupo 1.1_1 y Grupo 2.1_1, como en la construcción vigente.
La regla de filtro busca repite en Grupo 1.1_1, Grupo 2.1_1, Grupo 5.1_1 y Grupo 8.1_1.
No se altera esa regla ni se resuelve aquí la ambigüedad histórica entre ejemplos y perfiles experimentales.
Los 1.848 del diagrama anterior equivalen a 1.839 sin ninguna tarjeta más 9 sin resultado analizable.

RECOMENDACIÓN EDITORIAL
Usar survey_flow_candidate.png, que comienza en 4.037, con una nota que informa de las 905 entradas
apartadas. Mantener la auditoría de las 4.942 entradas y el gráfico de 1.839 solo como respaldo interno.
En el apéndice, denominarlo participación y abandono observado; no atribuir motivos ni intenciones.
Las exclusiones por filtro son una selección analítica, no necesariamente abandono. Mencionar el
solapamiento de 31 casos sin duplicarlos y conservar la muestra analítica de 2.705.
No hace falta incorporar la tabla de 29 etapas al manuscrito: se conserva para auditoría interna.

COMPROBACIONES
ResponseId único; 64 campos de tarjetas identificados, incluyendo el nombre excepcional Grupo 6_1.
Reproducción exacta de los IDs de los 2.705 analizados y las 22.124 decisiones de la base limpia.
Los 389 excluidos tienen respuesta a tarjetas. Las categorías de avance suman 1.839 sin solapamiento.
Todas las personas con tarjetas registran el último campo de instrucciones del bloque inicial.
Ninguna persona sin tarjetas registra los campos posteriores de presentación de política, ranking R1 o módulo final.
Los 4.942 registros se reconstruyen exactamente con las ramas del diagrama. Sin nuevas exclusiones.
'''
(OUT/'survey_exit_report.txt').write_text(report,encoding='utf8')
print(summary.to_string(index=False))
print(json.dumps(flow,indent=2))
print(pd.DataFrame(detail).query('n > 0').to_string(index=False))
