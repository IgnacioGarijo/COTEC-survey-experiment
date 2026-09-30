# Interacciones, seis sensibilidades y harshness: comparación interna

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
| Atributos | 48,43 | 48,54 | 48,55 | 48,54 | 48,63 |
| Características profesionales/demográficas | 0,14 | 0,14 | 0,14 | 0,13 | 0,18 |
| Creencias | 0,20 | 0,20 | 0,19 | 0,19 | 0,17 |
| Contexto experimental | 0,04 | 0,04 | — | — | — |
| Intercepto docente restante | 9,45 | 9,48 | 9,51 | 9,57 | — |
| Harshness observado | — | — | — | — | 12,70 |
| Sensibilidades docentes | 14,88 | 15,07 | 15,08 | 15,36 | 17,10 |
| Residuo | 26,86 | 26,53 | 26,53 | 26,21 | 21,22 |
| Total representado | 73,14 | 73,47 | 73,47 | 73,79 | 78,78 |

El coeficiente estimado de hb en la última especificación es 0,968477. Un aumento de 0,10 en hb corresponde a un cambio de 9,68 puntos porcentuales en el predictor lineal fijo, manteniendo el resto constante. Esta asociación no constituye validación independiente del indicador: hb se construye con las mismas decisiones.

## Regla de descomposición

La parte fija se divide entre atributos, características, creencias y, donde corresponde, contexto o harshness, mediante Shapley de proyecciones del predictor fijo completo. Las 15 interacciones pertenecen al bloque de atributos. Se promedia la contribución adicional sobre todos los órdenes de bloques; las cuotas suman exactamente la varianza del predictor fijo. No son diferencias de R² de modelos mixtos reestimados.

Se añade la varianza del intercepto docente (si existe), la media sobre perfiles de a' Sigma a para las pendientes, y la varianza residual. Las covarianzas entre pendientes se conservan dentro del componente conjunto de sensibilidades. El centrado global anula la contribución media de las covarianzas intercepto-pendiente. Los componentes se normalizan por su suma, que puede cambiar entre especificaciones.

La última columna es una descripción híbrida condicionada a hb: combina un resumen empírico de las respuestas con componentes de varianza estimados. La cuota de harshness no estima exactamente la misma cantidad que la varianza del intercepto aleatorio. El indicador conserva fluctuaciones de las decisiones utilizadas para construirlo; su inclusión también puede alterar las varianzas estimadas de pendientes y residuo. El reparto normalizado suma 100 %, pero no es por ello una descomposición empírica única ni una validación de los supuestos de independencia del modelo. No debe presentarse el aumento del total como una mejora predictiva demostrada.

Se conserva el hb ya utilizado en el paper, calculado con referencias de otros docentes ante la misma tarjeta, antes de restringir la muestra por covariables completas. No se transforma en un nuevo índice ni se recalcula por ramas experimentales. Retirar las ramas significa que el modelo deja de separar posibles diferencias por asignación del resto de componentes.

## Diagnósticos

| Modelo | Código optimizador | Singular | Mínimo autovalor covarianza | Gradiente combinado | Predicciones fuera de [0,1], % |
|---|---:|---|---:|---:|---:|
| Current | 0 | FALSE | 0.00163798555125883 | NA | 16,99 |
| Pairwise_context | 0 | FALSE | 0.00182862653003501 | NA | 17,20 |
| Pairwise_no_context | 0 | FALSE | 0.00180911061009755 | 0.00541393468341079 | 17,19 |
| Six_no_context | 0 | TRUE | 2.73969186051023e-18 | 0.0202751471078955 | 17,51 |
| Harshness_six | 0 | FALSE | 0.0040137098283052 | 0.00156763761754835 | 25,54 |

El CSV de diagnósticos contiene también Hessiana, rango fijo, columnas eliminadas, advertencias y R² empírico del ajuste condicional. Un ajuste singular indica que alguna combinación de efectos tiene varianza aproximadamente cero; no prueba que todas las nuevas pendientes sean nulas. Las pendientes no se seleccionan por significación de sus efectos medios. Los diagnósticos de singularidad siguen la definición de lme4: https://lme4.github.io/lme4/reference/isSingular.html.

La comparación previa de 30 particiones reservadas corresponde al modelo de cuatro pendientes con la interacción académica y controles experimentales. No valida automáticamente estas nuevas especificaciones. Para validar predictivamente un modelo con hb habría que reconstruir el indicador y sus referencias usando sólo las respuestas de entrenamiento.

## Reproducción

- `Rscript "2. Code/45. A_all_attributes_harshness.R"`: estima las tres especificaciones nuevas y reutiliza las dos anteriores; `--reuse` permite reconstruir tablas a partir de modelos guardados.
- `python "2. Code/46. report_all_attributes_harshness.py"`: genera este informe.
- Tablas agregadas: `all_attributes_harshness_partition.csv`, `all_attributes_harshness_diagnostics.csv`, `all_attributes_harshness_support.csv`, `all_attributes_harshness_covariances.csv` y `all_attributes_harshness_coefficients.csv`.
- Objetos de modelos en `all_attributes_harshness_models.rds`, ignorados por Git.
