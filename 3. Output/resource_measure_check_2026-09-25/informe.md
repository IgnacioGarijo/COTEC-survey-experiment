# Comprobación exploratoria del indicador de recursos — 25/09/2026

**Recomendación:** sustituir el compuesto en la especificación principal por el ítem «Se dedican demasiados recursos a los estudiantes repetidores», con una etiqueta que describa exactamente esa opinión. Conservar en el anexo el compuesto original y las especificaciones con el ítem de ineficacia solo y con ambos ítems separados. Esta es una recomendación posterior a la comprobación solicitada, todavía no un cambio autorizado del manuscrito.

## Preguntas y alcance

El bloque 19 pide acuerdo de 0 a 10 con cinco afirmaciones:

1. Mi trabajo tiene un impacto positivo en los estudiantes.
2. Hay estudiantes que han pasado de curso sin haber alcanzado las competencias necesarias.
3. Los estudiantes que pasan de curso están adecuadamente preparados para el siguiente nivel académico.
4. Se dedican demasiados recursos a los estudiantes repetidores.
5. Hagamos lo que hagamos, los recursos adicionales dedicados a los repetidores no son efectivos.

Los ítems 4 y 5 constituyen el compuesto actual. Las versiones referidas al centro y a la comunidad autónoma son alternativas: 1.205 docentes responden la primera y 1.207 la segunda; ninguno responde ambas. Por tanto, no son cuatro indicadores independientes. Hay 293 docentes sin respuesta al bloque entre los 2.705 de la muestra analítica general.

Fuente del texto: etiquetas de las preguntas 19.impacto_centro_1–5 y 19.impacto_region_1–5 de la exportación del cuestionario; correspondencia verificada en `2. Code/1.limpieza_datos.R`. Se mantiene la combinación centro/región utilizada por el análisis existente.

Solo dos preguntas hablan directamente de recursos. Las restantes miden percepciones sobre preparación/promoción y sobre el impacto del propio trabajo. Combinar más preguntas cambiaría el constructo. Los ítems 4 y 5 tampoco son equivalentes: juzgar excesivo el gasto es una valoración de su asignación; juzgarlo ineficaz es una creencia sobre su efectividad.

## ¿Funciona un indicador ampliado?

Diagnósticos con los 2.412 docentes con respuestas completas al bloque. Se orientan las preguntas 1 y 3 en sentido inverso (10 menos respuesta) para que expresen menor impacto/preparación. Se estandariza cada ítem y se promedian los ítems completos, sin imputación.

| Composición | Alfa estandarizado |
|---|---:|
| Dos originales: exceso + ineficacia | 0,651 |
| Tres: originales + promoción sin competencias | 0,470 |
| Cuatro: anteriores + falta de preparación para siguiente nivel | 0,435 |
| Cinco: anteriores + bajo impacto del profesor | 0,447 |

La correlación de los dos originales es 0,483 (Spearman: 0,494). Su coeficiente Spearman–Brown es 0,651, igual al alfa estandarizado de dos ítems. Esto muestra relación entre ellos, pero no establece por sí solo una escala validada ni permite estimar la fiabilidad de cada ítem individual. Tener dos ítems no invalida automáticamente un compuesto; la decisión requiere atender al contenido y al uso del indicador [1].

La pregunta de promoción sin competencias correlaciona solo 0,085 y 0,117 con los dos ítems originales. La falta de preparación correlaciona −0,031 y 0,044, respectivamente. La ampliación agrupa contenidos que covarían poco. El patrón se reproduce por separado en los marcos centro y región: alfa de dos ítems 0,664 y 0,639; de cuatro, 0,448 y 0,421.

Como diagnóstico adicional, los modelos exploratorios de un factor para cuatro y cinco ítems asignan cargas bajas a las preguntas añadidas (aproximadamente 0,05–0,16). En el modelo de tres ítems no hay grados de libertad para evaluar ajuste; no se interpreta su ajuste como validación. Estos diagnósticos usan correlaciones Pearson y no constituyen una validación psicométrica completa. Las correlaciones Spearman corroboran la débil conexión entre los contenidos.

**No recomiendo ampliar el indicador con estas preguntas.** Tampoco se seleccionan combinaciones o signos buscando maximizar el alfa o la relación con el resultado.

## Regresiones comparables

Resultado principal: Hᵇ, propensión relativa a recomendar repetición, ajustada mediante referencias de tarjeta que excluyen la respuesta del propio docente. Se reproducen los controles X₂ existentes: etapa, experiencia, antigüedad en el centro, género, edad, titularidad, nombramiento público permanente, número de grupos, impacto percibido, empatía y atribución al esfuerzo. Mismos 2.156 docentes en todas las especificaciones, incluidos los compuestos ampliados. OLS con intervalos HC3; estimaciones siguientes en puntos porcentuales por una desviación estándar del predictor. Los p-valores son nominales de esta comparación exploratoria.

| Especificación | Coeficiente | IC 95 % | p | R² del modelo |
|---|---:|---|---:|---:|
| Sin variable de recursos (X₂) | — | — | — | 3,061 % |
| Compuesto original | 2,058 | [1,213; 2,903] | <0,001 | 4,218 % |
| Solo exceso de recursos | 2,534 | [1,716; 3,352] | <0,001 | 4,857 % |
| Solo ineficacia | 0,950 | [0,097; 1,803] | 0,029 | 3,308 % |
| Ambos: exceso | 2,677 | [1,767; 3,587] | <0,001 | 4,877 % |
| Ambos: ineficacia | −0,310 | [−1,252; 0,632] | 0,518 | mismo modelo |
| Compuesto de tres ítems | 2,006 | [1,149; 2,862] | <0,001 | 4,147 % |
| Compuesto de cuatro ítems | 1,667 | [0,802; 2,531] | <0,001 | 3,787 % |

En unidades originales, cada punto adicional de acuerdo (escala 0–10) con «demasiados recursos» se asocia con 0,884 puntos porcentuales adicionales en Hᵇ, IC [0,598; 1,169], manteniendo los controles. Para ineficacia por separado: 0,334 puntos, IC [0,034; 0,633].

La estandarización de los ítems emplea los 2.412 casos disponibles, siguiendo el criterio del compuesto original; las regresiones usan los 2.156 casos comunes. La asociación del ítem de exceso se conserva con la tasa bruta de recomendaciones (Hᵃ: 2,586 puntos por SD) y con el resultado ajustado por atributos mediante logit (Hᶜ: 2,548). Al incorporar también marco centro/región y celda de asignación, el coeficiente Hᵇ de exceso es 2,655; al incluir ambos ítems, exceso es 2,820 e ineficacia −0,360. Las tablas CSV contienen todos los coeficientes e intervalos, no solo las variables de recursos.

## Comprobación predictiva

Cinco particiones de cinco grupos de docentes, con las mismas semillas y asignaciones del análisis OLS previo. Referencias de tarjetas y estandarizaciones se reconstruyen exclusivamente dentro del entrenamiento. Se utilizan idénticos grupos para comparar todas las alternativas. No se ajustan hiperparámetros ni se vuelven a ejecutar bosques o SHAP.

| Variables de recursos añadidas a X₂ | R² fuera de muestra medio | Rango entre particiones |
|---|---:|---:|
| Ninguna | 1,997 % | [1,886; 2,144] % |
| Compuesto original | 3,065 % | [2,955; 3,286] % |
| Solo exceso | 3,684 % | [3,513; 3,943] % |
| Solo ineficacia | 2,141 % | [2,025; 2,317] % |
| Ambos por separado | 3,586 % | [3,346; 3,858] % |
| Compuesto de tres | 2,986 % | [2,846; 3,216] % |
| Compuesto de cuatro | 2,629 % | [2,479; 2,835] % |

Los rangos describen dependencia de la partición, no intervalos de confianza. La pequeña ventaja predictiva de exceso solo apoya la parsimonia, pero no es una prueba formal de superioridad ni una evaluación independiente de la selección final entre alternativas: esta comparación ya utiliza los resultados de validación para informar la recomendación. El poder predictivo global sigue siendo modesto.

## Recomendación editorial

Usar «Perceived excess resources allocated to repeaters» o una etiqueta equivalente para el ítem 4. Puede expresarse por un punto de la escala 0–10, con el equivalente por SD como complemento. Evitar seguir llamándolo «resource skepticism», ya que un único ítem no mide toda la amplitud de ese concepto.

Conservar en el anexo las tres comprobaciones: compuesto original, ineficacia sola y ambos ítems separados. Incluir ambos en el modelo principal también sería defendible si se desea que las dos opiniones tengan protagonismo sustantivo, pero la segunda añade poca información condicional en esta prueba. Preferir exceso solo se fundamenta conjuntamente en claridad conceptual, parsimonia y resultados; no solo en significación estadística. Un ítem individual mide una opinión concreta y no debe presentarse como una escala latente más fiable que el compuesto.

No se ha modificado ningún manuscrito, PDF, generador o tabla existente. Si se adopta la recomendación, habrá que armonizar más adelante tablas, texto y análisis predictivos que utilicen la variable, conservando trazabilidad de esta decisión exploratoria.

## Reproducción y verificaciones

Ejecutar desde la raíz del proyecto: `Rscript --vanilla "2. Code/23. resource_measure_check.R"`.

El script lee los objetos congelados del análisis existente y escribe exclusivamente en esta carpeta. Verifica 2.705 docentes, 2.156 casos comunes, rango de respuesta, reproducción exacta del compuesto y de todos los coeficientes de la especificación publicada en el borrador, cobertura de tarjetas en entrenamiento y ausencia de predicciones perdidas. La validación reproduce los R² OLS anteriores para X₂ y X₃. Solo aparecen avisos de configuración regional al iniciar R; no avisos de estimación tras configurar la codificación. Se guardan sesión, hashes de entrada y resultados agregados, sin identificadores individuales.

[1] R. Eisinga, M. te Grotenhuis y B. Pelzer, “The reliability of a two-item scale: Pearson, Cronbach, or Spearman-Brown?”, International Journal of Public Health, vol. 58, pp. 637–642, 2013. https://doi.org/10.1007/s00038-012-0416-3.
