"""Generate internal comparison from saved model outputs; manuscript unchanged."""
from pathlib import Path
import csv

out = Path(__file__).resolve().parents[1] / '3. Output/paper_A/reports'
def read(name):
    with (out / name).open(encoding='utf-8-sig', newline='') as f:
        return list(csv.DictReader(f))
def fmt(x, d=2):
    return f'{float(x):.{d}f}'.replace('.', ',')
parts = read('all_attributes_harshness_partition.csv')
diagnostics = read('all_attributes_harshness_diagnostics.csv')
models = ['Current','Pairwise_context','Pairwise_no_context','Six_no_context','Harshness_six']
values = {(r['model'],r['component']): float(r['share']) for r in parts}
diag = {r['model']:r for r in diagnostics}
assert all(abs(sum(float(r['share']) for r in parts if r['model']==m)-1)<1e-8 for m in models)
rows = []
for key,label in [('Student','Atributos'),('Professional','Características profesionales/demográficas'),('Beliefs','Creencias'),('Context','Contexto experimental'),('General','Intercepto docente restante'),('Harshness','Harshness observado'),('Criteria','Sensibilidades docentes'),('Residual','Residuo')]:
    rows.append('| '+label+' | '+' | '.join(fmt(100*values[(m,key)]) if (m,key) in values and not (key=='General' and m=='Harshness_six') else '—' for m in models)+' |')
rows.append('| Total representado | '+' | '.join(fmt(100*(1-values[(m,'Residual')])) for m in models)+' |')
diagnostic_rows = []
for m in models:
    a=diag[m]
    diagnostic_rows.append(f"| {m} | {a['optimizer_status']} | {a['singular']} | {a['min_cov_eigenvalue']} | {a['convergence_gradient']} | {fmt(100*float(a['outside_01']))} |")
coef=diag['Harshness_six']['harshness_coefficient']
report = '''# Interacciones, seis sensibilidades y harshness: comparación interna

1 de octubre de 2026. Manuscrito sin modificaciones. Estimaciones puntuales nuevas; no se han transferido intervalos ni validaciones de especificaciones anteriores.

La especificación solicitada se ha podido estimar con rango fijo completo, covarianza no singular, Hessiana positiva, código de optimización cero y gradiente combinado inferior a 0,002. El total representado es 78,78 %; las cuotas de harshness y sensibilidades son 12,70 % y 17,10 %. La versión intermedia con intercepto y seis pendientes es singular y debe considerarse exploratoria: el ajuste no distingue todas las dimensiones de su covarianza como positivas. No es una prueba de que las sensibilidades de género o contexto social sean individualmente cero.

El ajuste sin ramas y con cuatro pendientes tiene código cero y Hessiana positiva, pero gradiente combinado 0,0054; el intermedio de seis pendientes alcanza 0,0203. Se conservan como comparadores puntuales, sin atribuirles el mismo diagnóstico numérico que a la especificación solicitada. No se usa REML-AIC para comparar modelos con efectos fijos diferentes.

## Especificaciones y muestra

Mismos 18.608 registros y 2.157 profesores en todas las columnas. Se comprueba la identidad de respuestas, orden e identificadores respecto al modelo anterior. Se conservan las características docentes y creencias anteriores.

1. **Actual:** seis atributos, suspensos × competencias, ramas experimentales; intercepto docente y cuatro pendientes correlacionadas.
2. **15 interacciones:** incorpora todos los pares de los seis atributos (15 términos); mantiene ramas, intercepto y cuatro pendientes.
3. **Sin ramas:** igual que 2, retirando el bloque de contexto experimental.
4. **Seis sensibilidades:** igual que 3, permitiendo además pendientes docentes de género y contexto social. Intercepto y seis pendientes correlacionadas.
5. **Harshness:** igual que 4, sustituyendo el intercepto aleatorio por el indicador observado hb como regresor fijo con coeficiente estimado. Se mantienen seis pendientes aleatorias correlacionadas, sin intercepto aleatorio.

No se sustituyen las pendientes por interacciones con harshness ni se estiman pendientes aleatorias de las 15 interacciones. Los atributos se centran globalmente sólo en la parte aleatoria, como en el modelo anterior. Se estima por REML. Los 2.157 docentes tienen ambos valores observados de cada atributo: hay variación intraprofesor en los seis, aunque esta comprobación marginal no garantiza por sí sola identificar conjuntamente todas las dimensiones de heterogeneidad.

## Resultados

Porcentajes del total de varianza construido por cada modelo:

| Componente | Actual | 15 interacciones | Sin ramas | Seis sensibilidades | Harshness |
|---|---:|---:|---:|---:|---:|
'''+ '\n'.join(rows)+f'''

El coeficiente estimado de hb en la última especificación es {fmt(coef,6)}. Un aumento de 0,10 en hb corresponde a un cambio de {fmt(10*float(coef))} puntos porcentuales en el predictor lineal fijo, manteniendo el resto constante. Esta asociación no constituye validación independiente del indicador: hb se construye con las mismas decisiones.

## Regla de descomposición

La parte fija se divide entre atributos, características, creencias y, donde corresponde, contexto o harshness, mediante Shapley de proyecciones del predictor fijo completo. Las 15 interacciones pertenecen al bloque de atributos. Se promedia la contribución adicional sobre todos los órdenes de bloques; las cuotas suman exactamente la varianza del predictor fijo. No son diferencias de R² de modelos mixtos reestimados.

Se añade la varianza del intercepto docente (si existe), la media sobre perfiles de a' Sigma a para las pendientes, y la varianza residual. Las covarianzas entre pendientes se conservan dentro del componente conjunto de sensibilidades. El centrado global anula la contribución media de las covarianzas intercepto-pendiente. Los componentes se normalizan por su suma, que puede cambiar entre especificaciones.

La última columna es una descripción híbrida condicionada a hb: combina un resumen empírico de las respuestas con componentes de varianza estimados. La cuota de harshness no estima exactamente la misma cantidad que la varianza del intercepto aleatorio. El indicador conserva fluctuaciones de las decisiones utilizadas para construirlo; su inclusión también puede alterar las varianzas estimadas de pendientes y residuo. El reparto normalizado suma 100 %, pero no es por ello una descomposición empírica única ni una validación de los supuestos de independencia del modelo. No debe presentarse el aumento del total como una mejora predictiva demostrada.

Se conserva el hb ya utilizado en el paper, calculado con referencias de otros docentes ante la misma tarjeta, antes de restringir la muestra por covariables completas. No se transforma en un nuevo índice ni se recalcula por ramas experimentales. Retirar las ramas significa que el modelo deja de separar posibles diferencias por asignación del resto de componentes.

## Diagnósticos

| Modelo | Código optimizador | Singular | Mínimo autovalor covarianza | Gradiente combinado | Predicciones fuera de [0,1], % |
|---|---:|---|---:|---:|---:|
'''+ '\n'.join(diagnostic_rows)+'''

El CSV de diagnósticos contiene también Hessiana, rango fijo, columnas eliminadas, advertencias y R² empírico del ajuste condicional. Un ajuste singular indica que alguna combinación de efectos tiene varianza aproximadamente cero; no prueba que todas las nuevas pendientes sean nulas. Las pendientes no se seleccionan por significación de sus efectos medios. Los diagnósticos de singularidad siguen la definición de lme4: https://lme4.github.io/lme4/reference/isSingular.html.

La comparación previa de 30 particiones reservadas corresponde al modelo de cuatro pendientes con la interacción académica y controles experimentales. No valida automáticamente estas nuevas especificaciones. Para validar predictivamente un modelo con hb habría que reconstruir el indicador y sus referencias usando sólo las respuestas de entrenamiento.

## Reproducción

- `Rscript "2. Code/45. A_all_attributes_harshness.R"`: estima las tres especificaciones nuevas y reutiliza las dos anteriores; `--reuse` permite reconstruir tablas a partir de modelos guardados.
- `python "2. Code/46. report_all_attributes_harshness.py"`: genera este informe.
- Tablas agregadas: `all_attributes_harshness_partition.csv`, `all_attributes_harshness_diagnostics.csv`, `all_attributes_harshness_support.csv`, `all_attributes_harshness_covariances.csv` y `all_attributes_harshness_coefficients.csv`.
- Objetos de modelos en `all_attributes_harshness_models.rds`, ignorados por Git.
'''
(out / 'all_attributes_harshness_report.md').write_text(report,encoding='utf-8')
print('\n'.join(rows))
