# Comparación y revisión crítica de los dos manuscritos

22 de septiembre de 2026. Comparación del manuscrito anterior (`paper_preregistered_rewrite.tex` y su PDF comentado), los nuevos papers A/B, sus tablas y los scripts 5 y 9–13. Este informe evalúa y propone; **no modifica los manuscritos ni ejecuta los análisis propuestos**. Todas las mejoras planteadas utilizan exclusivamente los datos existentes.

**Diagnóstico:** la separación mejora la coherencia, pero mi reescritura comprimió demasiado algunas exposiciones de resultados y dedicó demasiado espacio a cautelas metodológicas. Hay análisis conservados fuera de los PDF y algunas omisiones reales. La entrega anterior no debería haberse descrito como si toda la batería original estuviera reproducida íntegramente en los dos artículos.

## 1. Por qué parecen más cortos

El anterior tiene 32 páginas; los nuevos, 16 y 17. También cambié de 12 a 11 puntos y reduje el espaciado de líneas de 1,35 a 1,16. Por tanto, las páginas no son directamente comparables. El original tiene 19 tablas —una contiene el esquema experimental— y 3 figuras; A tiene 5 tablas y 4 figuras, y B, 11 tablas y 5 figuras.

La reducción de cada artículo resulta de separar los temas, agrupar especificaciones, abreviar literatura y trasladar detalles a CSV. **El problema no es alcanzar un número de páginas: falta desarrollar mejor qué enseñan algunos resultados y mostrar parte del material ya disponible.**

## 2. Qué se conserva, elimina, añade o modifica

| Bloque original | Destino y cambio real |
|---|---|
| Seis atributos del alumno | Conservados en A, con los mismos coeficientes principales. Se añaden denominadores y comprobación en control. |
| Distribución de harshness y terciles | Conservados en A; pendientes por tercil pasan al anexo. Se retiran las afirmaciones de normalidad, tipos estables y mecanismos ideológicos demostrados. |
| Caracterización OLS: X1, X2 y recursos | Reestimada. El PDF A muestra los coeficientes del modelo completo y resume el ajuste de los otros; las especificaciones completas están en CSV. Se pierde la comparación visual de las tres columnas. |
| Cuatro índices Ha–Hd | A conserva Hb y compara selectivamente Ha/Hc. **Hd no se reestima en la nueva batería.** La tabla de alternativas muestra solo cuatro predictores, aunque el CSV contiene todos para Ha/Hb/Hc. |
| Random forest, importancia y SHAP | Sustituidos por validación fuera de muestra y SHAP del mismo modelo validado. Desaparecen los contrastes SHAP con dirección y escala comparables a OLS; la importancia por permutación queda en CSV. |
| Gráfico de edad, etapa, esfuerzo y recursos | Eliminado del nuevo PDF. La información básica permanece en regresiones, pero se pierde una visualización útil. |
| Estudios I–III de políticas | Trasladados a B y reorganizados por preguntas, contrastes y celdas; ya no son una sucesión de H1–H3. Los ajustes por preferencias posteriores quedan como secundarios. |
| H4\|1: decisiones consecutivas | Antes no estimado; ahora se recupera el orden y se incorpora en A una exploración con permutaciones. |
| H4\|2: asignación y preferencias posteriores | Conservado en B, cambiando logits por diferencias de probabilidades y añadiendo corrección por comparaciones múltiples. |
| H4\|3: preferencias por brazo | Las favoritas se recalculan en CSV, pero no se presentan como tabla en B. **La distribución de menos favoritas por brazo no se vuelve a tabular en la nueva batería.** |
| MDE y equivalencia | Reestimados para los contrastes generales y varias comparaciones de políticas. El PDF concentra equivalencia en tres contrastes generales. **No se reproduce la equivalencia de todos los antiguos contrastes por alineamiento.** |
| Serie nacional de repetición, contexto y literatura | Se elimina la serie y su uso como supuesto respaldo de ineficacia de reformas. Se abrevia PISA y parte de la literatura de consecuencias; se corrige Allen y se añaden referencias metodológicas. |

**Adiciones sustantivas:** desacuerdo entre respuestas al mismo perfil (25,9%); propiedades del índice; validación predictiva separando docentes; bootstrap que reconstruye los referentes; disponibilidad de resultados desde la asignación; balance, ponderación y cotas de selección; contrastes y pruebas conjuntas de políticas; temporalidad del ranking; diagramas y trazabilidad reproducible.

**Conclusiones retiradas:** «las políticas no afectan de ninguna manera», «los docentes tienen sesgos ideológicos exógenos» y «una distribución de respuestas demuestra tipos estables». No se eliminaron por ser incómodas: excedían lo que acreditaban los análisis.

## 3. Decisiones metodológicas significativas que tomé

1. **Conservar tus decisiones de muestra y codificación.** Mantuve las respuestas utilizadas, el filtro favorable y los vectores de atributos, sin reparar los perfiles duplicados ni imponer ocho respuestas por docente. Permanecen 2.705 docentes y 22.124 decisiones. Los efectos fijos utilizan 22.006 decisiones porque 118 docentes solo aportan una y no identifican variación intradocente. Ya había efectos fijos en el análisis anterior; no son una innovación de la reescritura.

2. **Eliminar la exigencia general de tener favorita observada.** El código anterior filtraba esa variable antes de muchos análisis docentes. Ahora cada pregunta usa las observaciones que necesita: 2.705 para el resultado, frente a 2.546 con ranking, y 2.156 casos completos para comparar predictores. Control–Policy pasa de 1.011 a 1.105 observaciones. Esto puede cambiar resultados, no solo su presentación.

3. **Separar explicación y predicción.** Comparé X1/X2/recursos en la misma muestra. En validación, los promedios de tarjeta y las transformaciones se calculan solo con docentes de entrenamiento. El R² predictivo medio es 3,02% para OLS completo y 0,61% para el bosque. Un coeficiente significativo no equivale a predecir bien a otro docente [1]. El bosque usa parámetros fijos; no es una búsqueda exhaustiva del mejor predictor.

4. **Cambiar inferencia y lectura de políticas.** Usé errores HC3 a nivel docente, agrupación por docente en modelos de tarjetas, bootstrap y Holm por familias de contrastes. Corregí la dirección Revelation−Policy. La ampliación de muestra y nueva inferencia hacen que Policy−Control **ya no supere equivalencia ±3 puntos**; los otros dos contrastes generales sí, uno muy ajustadamente. No hay equivalencia general de todas las políticas [2].

5. **Distinguir preferencias previas y posteriores.** No eliminé los modelos originales de alineamiento, pero dejé de tratarlos todos como comparaciones causales equivalentes. El momento del ranking importa; recibir una política favorita y recordar que es favorita tampoco son el mismo tratamiento.

6. **Preservar edad, experiencia y recursos; precisar sus significados.** Mantuve edad y experiencia conjuntamente y el índice de recursos. Reformulé el indicador contractual como declaración de empleo público permanente para no llamar temporales a contratos privados no observados. Cambié el énfasis de «harshness» hacia propensión relativa, sin cambiar la fórmula de Hb.

7. **Omitir Hd y simplificar SHAP fueron decisiones mías.** Hd descuenta etapa y titularidad al construir el resultado y luego las utiliza para caracterizarlo: responde a otra pregunta y puede absorber justamente sus asociaciones. Eso justifica tratarlo con cautela, **no ocultar su desaparición**. Mantendría su definición y una sensibilidad claramente etiquetada. Los SHAP direccionales también pueden recuperarse, evitando confundirlos con coeficientes causales.

## 4. Dictamen de revisor y mejoras con los datos actuales

**Paper A: revisión mayor.** La pregunta conjunta es defendible; todavía necesita demostrar mejor qué significa el desacuerdo medido y dónde se concentra. **Paper B: revisión mayor, con mayor riesgo de rechazo editorial.** Ahora tiene mucha precisión metodológica, pero su contribución sustantiva queda demasiado apoyada en promedios pequeños. Estas valoraciones son juicio editorial, no predicciones de aceptación.

### Prioridades para A

**A1. Distinguir dispersión observada de coherencia docente.** Compararía la varianza de Hb con una referencia obtenida redistribuyendo respuestas entre docentes dentro del mismo perfil y contexto, preservando sus tarjetas y números de respuestas. Pregunta: ¿hay más concentración de recomendaciones en determinados docentes de la que produciría ese referente sin coherencia personal? Es una comprobación interna, no estabilidad temporal. No requiere reemplazar tus terciles por una clasificación jerárquica. Añadiría sensibilidad por número de respuestas, sin cambiar la muestra principal.

**A2. Mostrar qué casos generan desacuerdo.** El 25,9% agregado es insuficiente. Presentaría probabilidades e intervalos para perfiles interpretables —por ejemplo, suspensos con competencias conservadas y viceversa— y unas pocas interacciones académicas justificadas. Así se estudia si los docentes usan criterios compensatorios o acumulativos. No propongo contar atributos negativos ni inventar combinaciones ausentes. Debe explicarse qué comparaciones permite el soporte observado [3].

**A3. Fortalecer la conclusión predictiva.** Añadiría un ajuste del bosque dentro de los grupos de entrenamiento y un comparador sencillo regularizado o con relaciones suaves. Compararía errores de predicción de forma pareada e incorporaría incertidumbre del procedimiento; tres particiones no son un intervalo de confianza. Mostraría por separado el resultado de X1 con su muestra disponible y la comparación de todos los modelos en casos completos. Un R² pequeño de un índice ruidoso no demuestra que todas las diferencias docentes sean impredecibles.

**A4. Recuperar exposición sustantiva.** Restauraría la tabla OLS de tres columnas, perfiles gráficos de edad/experiencia y creencias, y una lectura más desarrollada de los terciles. Eliminaría repeticiones de cautelas y expresiones propias del proceso de revisión, como «corrigiendo una inversión habitual» al comentar Allen: el artículo debe presentar evidencia, no narrar nuestras correcciones.

### Prioridades para B

**B1. Estudiar cambios en criterios, además de cambios en la media.** Una política podría aumentar recomendaciones en unos perfiles y reducirlas en otros. Estimaría unas pocas interacciones tratamiento×atributos académicos, con pruebas conjuntas y corrección de multiplicidad. Aquí los efectos fijos de profesor sí permiten estudiar diferencias en pendientes, aunque absorben el efecto principal del tratamiento. Esta es una vía concreta para enriquecer la contribución sin buscar indiscriminadamente subgrupos significativos.

**B2. Completar la evidencia sobre preferencias.** Recuperaría favoritas y menos favoritas por brazo, sus intervalos y todos los contrastes relevantes. Separaría la predicción sustantiva sobre cada política del efecto adicional del recordatorio. El resultado de formación con p ajustado 0,0507 debe mostrar magnitud e incertidumbre, sin convertir cruzar 0,05 en la historia del artículo.

**B3. Hacer comparables los análisis de sensibilidad.** El contraste por docente y el de tarjetas tienen ponderaciones distintas; propondría una versión por tarjetas con peso inverso al número de respuestas del docente para distinguir ponderación de ajuste por perfil. Añadiría una sensibilidad gradual a cuánto difieren las respuestas no observadas, además de cotas extremas, dejando claro el resultado al que se aplica. La ponderación debería reconstruirse dentro del bootstrap si se quieren intervalos que incorporen su estimación.

**B4. Cerrar el argumento de equivalencia.** Justificaría sustantivamente los márgenes, completaría las comparaciones de alineamiento y evaluaría la sensibilidad del resultado limítrofe al bootstrap con más réplicas. Tener un intervalo dentro de ±3 sobre las respuestas observadas no acredita el mismo límite para todos los inicialmente asignados [2].

**Orden recomendado:** recuperar primero resultados ya existentes que quedaron fuera; después A1–A2 y B1, que aportan contenido científico; luego reforzar predicción e inferencia. Mantendría los dos papers separados. No recomendaría alargarlos con contexto legal, nuevas advertencias o bibliografía decorativa, ni condicionaría su mejora a repetir el experimento.

## Referencias metodológicas de apoyo

[1] S. Mullainathan y J. Spiess, “Machine Learning: An Applied Econometric Approach”, *Journal of Economic Perspectives*, 31(2), 87–106, 2017. [Artículo](https://www.aeaweb.org/articles?id=10.1257/jep.31.2.87).

[2] D. Lakens, “Equivalence Tests: A Practical Primer for t Tests, Correlations, and Meta-Analyses”, *Social Psychological and Personality Science*, 8(4), 355–362, 2017. [Artículo](https://journals.sagepub.com/doi/10.1177/1948550617697177).

[3] J. Hainmueller, D. J. Hopkins y T. Yamamoto, “Causal Inference in Conjoint Analysis: Understanding Multidimensional Choices via Stated Preference Experiments”, *Political Analysis*, 22(1), 1–30, 2014. [Artículo](https://doi.org/10.1093/pan/mpt024).
