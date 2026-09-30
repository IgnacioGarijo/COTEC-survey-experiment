"""Report paired held-out-response validation without changing the manuscript."""
import csv
from pathlib import Path

OUT = Path(__file__).resolve().parents[1] / '3. Output/paper_A/reports'

def read(name):
    with (OUT / name).open(encoding='utf-8-sig', newline='') as f:
        return list(csv.DictReader(f))

def number(x, digits=5):
    return f'{float(x):.{digits}f}'.replace('.', ',')

def percent(x):
    return number(100 * float(x), 2)

s = read('within_teacher_cv_summary.csv')[0]
u = {r['metric']: r for r in read('within_teacher_cv_uncertainty.csv')}
d = read('within_teacher_cv_diagnostics.csv')
folds = read('within_teacher_cv_partitions.csv')
support = read('within_teacher_cv_support.csv')[0]
assert int(s['rounds']) == 30 and len(d) == 60 and len(folds) == 30
assert all(r['optimizer_status'] == '0' for r in d)
assert abs(float(s['mse_intercept']) - float(s['mse_four']) - float(u['raw_difference']['estimate'])) < 1e-10
positive = float(u['raw_difference']['lo']) > 0
conclusion = ('La comparación favorece al modelo de cuatro pendientes: reduce el error en respuestas que no se utilizaron para estimarlo. '
              'Esto respalda interpretar las pendientes como patrones docentes que reaparecen entre tarjetas, por encima de la severidad general.'
              if positive else
              'La comparación no ofrece evidencia suficientemente clara de una mejora del modelo de cuatro pendientes: el intervalo de la diferencia de error no queda completamente por encima de cero.')
clip_relative = (float(s['mse_intercept_clipped']) - float(s['mse_four_clipped'])) / float(s['mse_intercept_clipped'])
max_gradient = max(float(r['scaled_gradient']) for r in d if r['scaled_gradient'] not in ('', 'NA'))
audit_note = ''
if (OUT / 'within_teacher_cv_optimizer_audit.csv').exists():
    audit = read('within_teacher_cv_optimizer_audit.csv')
    max_error_change = max(abs(float(a['refined_mse']) - float(a['old_mse'])) for a in audit)
    assert all(a['optimizer_status'] == '0' and a.get('positive_hessian') == 'TRUE' and float(a['min_hessian_eigenvalue']) > 0 for a in audit)
    audit_note = (f'Se reestimaron también los {len(audit)} ajustes con mayor gradiente escalado utilizando glmmTMB para el mismo modelo gaussiano con REML, inicializado con las estimaciones de lme4. '
                  f'La mayor variación del error reservado fue {number(max_error_change, 8)}. '
                  'Los ajustes de esta segunda implementación tienen Hessiana positiva y código de optimización cero. '
                  'Los errores principales de la tabla conservan los ajustes originales; esta auditoría comprueba su estabilidad numérica. '
                  'La inicialización importa: el primer ajuste glmmTMB con valores iniciales automáticos produjo una Hessiana no positiva. '
                  'Antes se intentó un refinamiento BOBYQA: dos de los tres ajustes alcanzaron el límite de evaluaciones, aunque apenas cambiaron las predicciones; se conserva ese intento en within_teacher_cv_bobyqa_attempt.csv. '
                  'Por tanto, esta comprobación acredita concordancia de predicciones con una inicialización informada, no ausencia de sensibilidad numérica a todos los valores iniciales.')
recommendation = ('La evidencia conjunta del bootstrap, las sensibilidades de especificación y esta validación permite defender el modelo lineal completo como descomposición descriptiva principal, explicando sus supuestos. '
                  'La comprobación binomial anterior sigue sin confirmar los porcentajes y debe documentarse; no convierte esta mejora predictiva en una validación del 14,88 % o del 73,14 % como cantidades invariantes al modelo.'
                  if positive else
                  'Mantendría por ahora la ampliación como exploratoria. La precisión del bootstrap no sustituye a evidencia de que las sensibilidades docentes aportan información reproducible en otras respuestas.')
report = f'''# Validación de respuestas reservadas dentro de profesor

30 de septiembre de 2026. Análisis interno; manuscrito sin cambios.

## Resultado

**{conclusion}**

| Modelo | Error cuadrático medio en tarjetas reservadas |
|---|---:|
| Atributos y covariables + severidad general docente | {number(s['mse_intercept'])} |
| Mismos regresores + severidad y cuatro sensibilidades docentes | {number(s['mse_four'])} |

La reducción relativa del error es **{percent(u['raw_relative']['estimate'])} %**, con un intervalo bootstrap aproximado del 95 % de **{percent(u['raw_relative']['lo'])}–{percent(u['raw_relative']['hi'])} %**. La diferencia absoluta de errores (sencillo menos completo) es {number(u['raw_difference']['estimate'])}, con SE {number(u['raw_difference']['se'])} e intervalo {number(u['raw_difference']['lo'])}–{number(u['raw_difference']['hi'])}.

El modelo ampliado mejora en **{s['rounds_improved']} de las {s['rounds']} particiones**. La mejora absoluta entre particiones oscila entre {number(s['min_improvement'])} y {number(s['max_improvement'])}. Ese rango no se interpreta como intervalo de confianza, porque las particiones comparten profesores y respuestas.

## Diseño y ausencia de reutilización de la respuesta reservada

Se utilizan los mismos 2.157 profesores y 18.608 decisiones del análisis principal. Cada profesor dispone de {support['min_cards']} a {support['max_cards']} tarjetas distintas; no hay perfiles duplicados dentro de profesor en esta muestra.

En cada una de 30 particiones se reserva una tarjeta de cada profesor. Se construye un calendario aleatorio equilibrado para que las tarjetas de cada docente se reserven aproximadamente el mismo número de veces. La selección no utiliza las respuestas. Cada partición tiene 2.157 respuestas de evaluación y 16.451 de entrenamiento.

Ambos modelos se reestiman desde cero con exactamente las mismas respuestas de entrenamiento y se evalúan con las mismas respuestas reservadas. Se recalculan efectos fijos, varianzas, covarianzas y efectos individuales. Los atributos de las pendientes se centran usando exclusivamente la media del entrenamiento. No se reutilizan los efectos docentes ni parámetros iniciales estimados con toda la muestra; no se introduce el harshness calculado con respuestas completas.

Los regresores fijos se mantienen: seis atributos, interacción suspensos × competencias, características profesionales y demográficas, creencias y contexto experimental. La ampliación permite sensibilidades docentes a suspensos, falta de competencias, absentismo y comportamiento disruptivo, además del intercepto. Las covarianzas entre efectos docentes se estiman libremente.

## Qué se evalúa

Se predice la recomendación de un profesor del que se conocen otras respuestas dentro de esta encuesta. La comparación pregunta si sus sensibilidades a los atributos aportan información sobre una decisión que no participó en la estimación, más allá de su severidad general.

Esto no evalúa profesores completamente nuevos ni decisiones fuera de la encuesta. Tener otras respuestas del mismo docente en el entrenamiento es deliberado y corresponde a la pregunta evaluada. Cada profesor pesa lo mismo en el error medio.

La medida principal es el error cuadrático de las predicciones lineales originales. No se transforma automáticamente en porcentaje de decisiones acertadas, porcentaje de varianza atribuida a criterios, ni incremento que se pueda sumar al R² o al 73,14 % de la descomposición.

## Incertidumbre, sensibilidad y ajuste numérico

Se promedian primero las pérdidas reservadas de cada profesor sobre las 30 particiones. Después se remuestrean 3.999 veces esos pares de pérdidas por profesor. Así se mantiene la comparación emparejada y no se tratan las 64.710 evaluaciones repetidas como observaciones independientes.

El intervalo describe incertidumbre del contraste de pérdidas condicionada a los modelos de validación estimados. No es un bootstrap anidado que vuelva a entrenar los 60 modelos en cada réplica ni sustituye la evaluación en una nueva muestra.

Como sensibilidad, se limitan ambas predicciones al intervalo [0,1] antes de calcular el error:

| Modelo | Error cuadrático con predicciones limitadas a [0,1] |
|---|---:|
| Severidad general | {number(s['mse_intercept_clipped'])} |
| Severidad y cuatro sensibilidades | {number(s['mse_four_clipped'])} |

La reducción relativa en esta sensibilidad es {percent(clip_relative)} %. La diferencia absoluta tiene un intervalo aproximado de {number(u['clipped_difference']['lo'])} a {number(u['clipped_difference']['hi'])}.

Todos los ajustes devuelven código de optimización cero; el script reintenta con otro optimizador si detecta problemas de convergencia. Hubo {s['singular_intercept']} ajustes singulares del modelo de intercepto y {s['singular_four']} del modelo ampliado, conservados en la comparación. El mayor gradiente escalado registrado es {number(max_gradient, 6)}; los mensajes y advertencias de cada ajuste se guardan en el CSV de diagnósticos.

{audit_note}

## Consecuencia para el paper

{recommendation}

La interpretación concreta es la siguiente: si un profesor parece dar más peso a un atributo según sus otras tarjetas, esa información ayuda —en la medida cuantificada arriba— a anticipar otra de sus respuestas. Esto es evidencia sobre la utilidad de la estructura de pendientes, no una identificación definitiva de la fracción exacta de varianza que corresponde a criterios personales.

## Reproducción

- `Rscript "2. Code/42. A_within_teacher_validation.R"`: 30 particiones emparejadas. Semilla de particiones 930420 y del bootstrap 930421. El cálculo puede continuar desde el objeto local guardado; verifica la huella de los datos y el calendario.
- `python "2. Code/43. report_within_teacher_validation.py"`: genera este informe.
- `Rscript --vanilla "2. Code/44. audit_cv_optimization.R"`: contrasta los tres ajustes con mayor gradiente escalado con otra implementación; guarda `within_teacher_cv_optimizer_audit.csv`.
- Resultados agregados: `within_teacher_cv_summary.csv`, `within_teacher_cv_uncertainty.csv`, `within_teacher_cv_partitions.csv`, `within_teacher_cv_diagnostics.csv`, `within_teacher_cv_support.csv` y `R_session_within_teacher_cv.txt`.
- Predicciones y pérdidas individuales permanecen en `within_teacher_cv_objects.rds`, ignorado por Git.

La elección de particiones depende del objetivo de generalización en datos jerárquicos: Roberts, D. R., et al. (2017). Cross-validation strategies for data with temporal, spatial, hierarchical, or phylogenetic structure. *Ecography, 40*, 913–929. https://doi.org/10.1111/ecog.02881
'''
(OUT / 'within_teacher_cv_report.md').write_text(report, encoding='utf-8')
print(conclusion)
print(f"Relative error reduction: {percent(u['raw_relative']['estimate'])}%; positive CI: {positive}.")
