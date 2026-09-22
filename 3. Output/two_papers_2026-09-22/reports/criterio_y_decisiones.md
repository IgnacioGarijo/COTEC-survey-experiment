# Ampliación analítica para la segunda versión

22 de septiembre de 2026. Análisis posteriores al plan inicial y al informe editorial; no se presentan como hipótesis prerregistradas. Se conservan las decisiones de muestra, tarjetas, codificación, edad/experiencia, recursos y terciles de los autores. Ninguna ampliación sustituye el resultado principal por una muestra elegida por sus resultados.

## Preguntas y familias de análisis

- **A: estructura del juicio.** Redistribución de respuestas dentro de identificador de tarjeta y celda de asignación, con exposiciones y número de respuestas fijos (1.999 permutaciones). Se compara la varianza entre docentes, también con ocho o más respuestas y solo en control. Es una referencia de intercambiabilidad dentro del encuentro, no una estimación de fiabilidad temporal.
- **A: conflicto entre señales.** Cuatro categorías observadas de suspensos y competencias; medias con agrupación por docente y desacuerdo calculado dentro de cada identificador. Tres interacciones: suspensos × competencias, absentismo y disciplina; Holm en esas tres. No se suman características negativas ni se extrapolan perfiles ausentes.
- **A: caracterización.** Se recupera Hd con logit de atributos, etapa y titularidad de tres categorías; los cuatro resultados se comparan en la misma muestra. Se recuperan todas las columnas OLS y gráficos descriptivos continuos.
- **A: predicción.** Cinco particiones de cinco grupos externos; ajuste en tres grupos internos, con referentes y recursos reconstruidos también dentro de cada división interna. Bosque: seis combinaciones de mtry/tamaño mínimo, 250 árboles durante selección y 500 en ajuste externo. Ridge: diez penalizaciones. Comparación pareada con corrección aproximada por solapamiento, no intervalos basados en tratar como independientes los grupos. X1 con muestra disponible se presenta por separado.
- **B: criterios académicos.** Dos interacciones de procedimiento × atributo académico para cada uno de los tres contrastes entre brazos. Holm en los seis coeficientes y, por separado, en las tres pruebas conjuntas. Las especificaciones con efectos fijos de tarjeta y pesos por docente son comprobaciones del mismo contraste, no nuevas pruebas independientes. Los efectos fijos de docente absorben el efecto principal del brazo, pero permiten estimar sus diferencias de pendientes.
- **B: preferencias.** Favoritas y menos favoritas, intervalos Wilson y 18 contrastes entre brazos, con una familia Holm de 18. Se mantiene separada la familia anterior de seis efectos de asignación a una política sobre su ranking posterior. Los contrastes entre brazos que cambian el momento de elicitación comparan procedimientos de medición, no una medida tomada en el mismo instante.
- **B: selección y precisión.** Modelo de tarjetas con peso 1/n por docente; sensibilidad bidimensional de resultados ausentes en Ha; bootstrap de 1.999 muestras desde todos los asignados, reconstruyendo referentes de tarjetas y reestimando el logit de observación. La imputación mediana inicial de covariables de selección se mantiene fija; se reestiman los coeficientes y los pesos. Los intervalos son percentiles. Las comparaciones de alineamiento conservan su estatus causal o descriptivo original, también al evaluar equivalencia.

## Cambios sustantivos en la lectura

La variación entre docentes excede claramente la referencia de respuestas intercambiables. El desacuerdo es máximo cuando se acumulan suspensos sin déficits de competencias. Esto refuerza la contribución de A sin convertir sus terciles en tipos estables.

En B hay una señal de menor pendiente de suspensos con el recordatorio; la prueba individual y la conjunta tienen distinta fuerza tras multiplicidad. La equivalencia ±3 puntos de Revelation–Policy depende del método y no se sostiene con el bootstrap ampliado. El artículo debe reconocer ambos resultados, sin convertir ni la significación ni la no significación en toda su narrativa.

## Reproducibilidad y límites

Scripts 15–17; entradas locales de scripts 9–11. Los CSV y figuras agregados de la primera versión se copian para que las versiones sean independientes en su presentación. No se publican datos individuales ni objetos RDS. Semillas y ajustes están en los scripts. El ajuste predictivo y las permutaciones no reparan el soporte original ni identifican efectos de reformas implementadas.

Referencia nueva comprobada en el editor: C. Nadeau y Y. Bengio, *Inference for the Generalization Error*, Machine Learning 52, 239–281 (2003), https://doi.org/10.1023/A:1024068626366. Los intervalos corregidos se usan como aproximación de sensibilidad, especialmente porque el resultado se reconstruye en cada grupo de entrenamiento.
