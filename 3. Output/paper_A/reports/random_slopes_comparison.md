# Análisis complementario: diferencias docentes en la valoración de atributos

29 de septiembre de 2026. No modifica el manuscrito ni los resultados principales.

Actualización del 30 de septiembre: la comprobación de incertidumbre y el contraste binomial están en [random_slopes_validation.md](random_slopes_validation.md). La estabilidad del modelo lineal no se ha traducido en una validación del reparto mediante el modelo binomial; la recomendación final de ese informe matiza la valoración exploratoria que sigue.

## Pregunta y especificaciones

¿Cuánta variación atribuye el modelo a diferencias entre profesores en cómo responden a suspensos, falta de competencias, absentismo y comportamiento disruptivo, además de su severidad general?

Se conserva la muestra de 18.608 decisiones de 2.157 profesores y los regresores fijos de la descomposición vigente: seis atributos, interacción suspensos × competencias, características docentes, creencias y contexto experimental. Se amplía el intercepto aleatorio con pendientes aleatorias primero para los dos atributos académicos y después para los cuatro atributos. Se estiman las covarianzas entre intercepto y pendientes y entre pendientes, no un conjunto de regresiones independientes por profesor. Los 2.157 docentes presentan variación observada en cada uno de esos cuatro atributos.

Las pendientes se centran en la media global de cada atributo, ponderando por decisiones. Así, el intercepto describe la tendencia general en el perfil promedio de referencia. Para cada tarjeta se calcula la varianza aleatoria z'Σz y se promedia sobre la distribución observada. El centrado hace que la covarianza intercepto–pendientes promedie cero; las covarianzas entre pendientes permanecen dentro del componente agregado de diferencias de criterio. La partición no ignora correlaciones ni suma simplemente las varianzas de las pendientes. La separación entre severidad y criterios se refiere a esta distribución de tarjetas y a este centrado.

La varianza fija, la varianza docente media y la varianza residual se normalizan por su suma. Es una extensión de la descomposición vigente, siguiendo Johnson (2014); no es un R² predictivo validado ni una suma con la cuota del harshness observado.

## Comparación principal

Porcentaje de la varianza total modelizada:

| Componente | Actual: intercepto | Dos pendientes académicas | Cuatro pendientes |
|---|---:|---:|---:|
| Variables observadas: atributos, características, creencias y contexto | 49,16 | 48,85 | 48,81 |
| Severidad general residual del profesor | 8,12 | 9,09 | 9,45 |
| Diferencias docentes en la ponderación de atributos | — | 11,19 | 14,88 |
| Residuo de cada respuesta | 42,72 | 30,88 | 26,86 |
| Total recogido por componentes fijos y docentes | **57,28** | **69,12** | **73,14** |

El modelo de dos pendientes tiene una matriz de covarianzas singular, con un autovalor próximo a cero. Su separación de todas las dimensiones aleatorias no está plenamente sostenida; se presenta como comparación exploratoria, no como especificación preferida. El modelo de cuatro pendientes es no singular y el optimizador termina sin avisos de convergencia.

Al pasar del modelo académico al de cuatro pendientes, la fracción atribuida a diferencias de criterio aumenta 3,69 puntos y el total recogido aumenta 4,02 puntos. Como se reestiman todos los parámetros, esto es una comparación entre modelos: no constituye una cuota exclusiva y aditiva de absentismo y comportamiento.

En el modelo de cuatro pendientes, las desviaciones estándar estimadas de la sensibilidad docente son 29,76 puntos porcentuales para suspensos; 25,77 para falta de competencias; 10,46 para absentismo; y 13,17 para comportamiento disruptivo. Son dispersiones estimadas de pendientes, no efectos promedio, errores estándar ni mediciones precisas de la sensibilidad de cada docente. No se deben sumar ni interpretar como porcentajes de varianza explicada.

El AIC REML pasa de 13.540,12 en el modelo vigente a 10.692,69 en el de cuatro pendientes, con el mismo diseño de efectos fijos y muestra. El mejor ajuste persiste tras penalizar el mayor número de parámetros, dentro de la familia lineal utilizada. No se comparan AIC REML entre modelos con distintas covariables fijas o muestras.

## Estabilidad de la estimación

| Comprobación | Diferencias de criterio | Total recogido |
|---|---:|---:|
| Cuatro pendientes, covarianzas libres | 14,88 % | 73,14 % |
| Cuatro pendientes independientes en la parametrización centrada | 15,42 % | 73,17 % |
| Cuatro pendientes y todas las 15 interacciones fijas entre atributos | 15,07 % | 73,47 % |
| Solo docentes con al menos ocho respuestas (2.022; 17.663 decisiones) | 14,78 % | 73,31 % |
| Otro optimizador, con cálculo de derivadas | 14,88 % | 73,14 % |

Todas estas especificaciones de cuatro pendientes son no singulares y terminan sin mensajes de fallo de convergencia. El resultado agregado cambia poco al eliminar las covarianzas, aunque ese modelo tiene peor ajuste; esto no justifica asumir que las sensibilidades sean independientes. Las interacciones fijas adicionales tampoco absorben el componente de criterios heterogéneos.

Estas comprobaciones evalúan estabilidad ante especificación y optimización, no precisión muestral. No se han calculado nuevos intervalos, errores estándar ni contrastes de frontera para las varianzas. El rango entre especificaciones no es un intervalo de confianza.

## Relación con harshness y lectura sustantiva

La alternativa OLS con harshness observado, calculada previamente, tiene un R² dentro de muestra de 62,12 % y una cuota Shapley de harshness de 13,29 %. Ese indicador asigna un único valor a cada profesor. El nuevo modelo permite además que cambie su respuesta a cada atributo; distingue diferencias de nivel y diferencias de criterio mediante una estructura multinivel.

El 73,14 % modelizado no debe compararse con el 62,12 % como si ambos fueran el mismo R²: el primero suma varianzas estimadas; el segundo mide el ajuste efectivo de una regresión sobre las respuestas observadas. Como diagnóstico separado, el ajuste condicional dentro de muestra obtenido usando los efectos aleatorios estimados es 79,83 % en el modelo de cuatro pendientes, frente a 60,29 % en el modelo vigente. Estos ajustes usan las mismas respuestas para estimar los efectos individuales y no acreditan predicción fuera de muestra. No se utilizan para la tabla principal.

La lectura sustantiva del análisis es que las divergencias no parecen limitarse a una tendencia general a recomendar repetición: el modelo atribuye una fracción considerable a diferencias en la respuesta a los atributos, incluso manteniendo el componente promedio y sus interacciones. Aproximadamente 14,9 % corresponde a esa heterogeneidad de criterio y 9,4 % a diferencias generales adicionales entre profesores, bajo esta especificación.

## Recomendación y límites

El modelo con cuatro pendientes es un complemento prometedor: permite responder directamente si los profesores discrepan en los criterios que aplican. Mantendría harshness como descripción observable y presentaría este resultado, si se incorpora, como una descomposición complementaria de la heterogeneidad docente. No sumaría su 14,88 % a la cuota Shapley de harshness de otra regresión.

La decisión es binaria y el análisis conserva el modelo lineal de trabajo para facilitar la comparación. En el modelo de cuatro pendientes, un 16,99 % de los valores ajustados condicionales queda fuera de [0,1]; no deben interpretarse como probabilidades calibradas. La estabilidad mostrada es dentro de esta familia lineal. Antes de tratar el porcentaje como una cifra definitiva para publicación, sería necesario cuantificar su incertidumbre y contrastar la estructura con una especificación binomial, teniendo en cuenta que la escala de varianza cambia. Los resultados no identifican por qué un profesor pondera un atributo de otra manera ni establecen persistencia de su criterio fuera de la encuesta.

## Reproducción y referencia

Ejecutar desde la raíz: `Rscript "2. Code/38. A_random_slopes_comparison.R"`. Reestima los modelos; `--reuse` sirve únicamente para regenerar tablas de los modelos locales guardados sin recalcularlos. Entradas: `analysis_objects.rds` y `variance_models.rds`; salidas: `random_slopes_comparison.csv`, `random_slopes_covariances.csv`, `random_slopes_support.csv`, `R_session_random_slopes.txt` y el objeto local ignorado por Git `random_slopes_models.rds`.

Johnson, P. C. D. (2014). Extension of Nakagawa & Schielzeth's R²GLMM to random slopes models. *Methods in Ecology and Evolution, 5*, 944–946. https://doi.org/10.1111/2041-210X.12225
