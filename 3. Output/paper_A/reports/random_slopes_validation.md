# Comprobación final del modelo con cuatro pendientes docentes

30 de septiembre de 2026. Análisis interno; no modifica el manuscrito.

## Dictamen

**La precisión por remuestreo del modelo lineal es razonable, pero la comprobación binomial no valida el reparto de varianza. No recomendaría sustituir todavía la descomposición principal por este modelo presentando el 73,14 % como una cifra robusta a la forma de modelizar la decisión.**

La estructura de diferencias de criterio sigue siendo sustantivamente interesante. Esta conclusión no demuestra que tales diferencias no existan: limita la interpretación cuantitativa que podemos dar al reparto actual. Tampoco demuestra que un modelo binomial estimado mediante otro procedimiento tenga que fracasar.

## 1. Incertidumbre del modelo lineal

Se remuestrearon profesores completos, con todas sus respuestas y covariables. Cada copia de un profesor recibe un identificador nuevo; los cuatro atributos se recentran en cada muestra y se reestima el modelo completo. La muestra original contiene 18.608 decisiones de 2.157 docentes. Se obtuvieron 199 ajustes válidos de 199 réplicas, con 5 ajustes singulares. Los bloques fijos y todos los componentes se recalculan en cada réplica.

Unidades: porcentajes de varianza; errores estándar e intervalos en puntos porcentuales.

| Componente | Estimación (%) | SE bootstrap (pp) | Intervalo percentil aproximado del 95 % |
|---|---:|---:|---:|
| Atributos del alumno e interacción académica | 48,43 | 0,73 | 47,04–49,87 |
| Características profesionales y demográficas | 0,14 | 0,05 | 0,08–0,27 |
| Creencias y actitudes | 0,20 | 0,06 | 0,12–0,34 |
| Contexto de asignación | 0,04 | 0,03 | 0,03–0,16 |
| Severidad general residual | 9,45 | 0,43 | 8,49–10,23 |
| Diferencias de criterio | 14,88 | 0,48 | 13,96–15,87 |
| Residuo de la decisión | 26,86 | 0,61 | 25,60–27,94 |
| Total recogido por el modelo | 73,14 | 0,61 | 72,06–74,40 |

Los intervalos son aproximados, con precisión Monte Carlo limitada por 199 réplicas. No se interpretan como pruebas de hipótesis de varianzas en una frontera, ni el rango de sensibilidad entre especificaciones se confunde con un intervalo de confianza. El bootstrap cuantifica incertidumbre muestral bajo el procedimiento lineal; no corrige una forma funcional inadecuada.

## 2. Contraste con una decisión binaria

Se estimó un modelo binomial con enlace logit, manteniendo los mismos regresores y cuatro pendientes aleatorias correlacionadas. Se utilizó máxima verosimilitud aproximada por Laplace con glmmTMB. El ajuste principal devuelve código de convergencia cero y Hessiana positiva. Un segundo optimizador, inicializado en el ajuste obtenido, reproduce prácticamente la función objetivo y los porcentajes. Se estimó además una versión con covarianzas aleatorias diagonales, también con código cero y Hessiana positiva. Esto acredita comprobaciones numéricas concretas, no la validez global del modelo.

Las varianzas aleatorias del logit completo son extremadamente elevadas: la desviación estándar de su intercepto es aproximadamente 25,64 unidades logit, frente a 1,50 en el modelo logístico de solo intercepto. Sus probabilidades condicionales se aproximan mucho a cero o uno.

Para comparar con la escala observada, se integraron las probabilidades sobre la distribución estimada de efectos docentes. La suma aleatoria para cada tarjeta es normal con varianza z'Σz, por lo que la integración de sus momentos es unidimensional aunque haya cinco efectos docentes. Las cuadraturas de Gauss–Hermite de 60 y 120 nodos no eran suficientemente precisas con esas dispersiones; se sustituyeron para la interpretación final por integración adaptativa con tolerancia relativa 1e-8. Los resultados de todos los métodos se conservan para auditar esta diferencia. `nodes=0` identifica la integración adaptativa en el CSV. Esta integración precisa calcula los momentos a partir de parámetros ya estimados: no sustituye la aproximación de Laplace utilizada para estimar esos parámetros.

Porcentaje de varianza en la escala de las decisiones, no en una escala latente:

| Modelo | Variación de la probabilidad media entre perfiles/covariables | Variación docente dentro de esos perfiles | Residuo Bernoulli | Total recogido |
|---|---:|---:|---:|---:|
| Logístico: intercepto | 49,53 | 12,36 | 38,11 | 61,89 |
| Logístico: cuatro pendientes | 17,99 | 78,22 | 3,79 | 96,21 |
| Logístico: cuatro pendientes independientes | 26,04 | 69,64 | 4,32 | 95,68 |

En el modelo lineal completo, las cifras comparables agregadas eran 48,81 %, 24,33 %, 26,86 % y 73,14 %. La versión binomial no reproduce ese reparto: la diferencia no es solo comparar una escala latente con una observada. En el logit, la probabilidad media integrada también depende de la varianza aleatoria específica de cada perfil; por ello su primer componente no debe identificarse sin matices con la misma atribución Shapley de los efectos fijos del modelo lineal. No se intenta trasladar la separación lineal de severidad/criterios a probabilidades mediante una suma ingenua.

## 3. Comprobación de reproducción de un rasgo básico de los datos

La tasa observada de recomendaciones de repetición es **47,41 %**. Se simularon 999 réplicas completas por modelo, manteniendo las tarjetas y covariables observadas, generando un vector docente compartido por todas las tarjetas de cada profesor y después decisiones Bernoulli. Se mantienen fijos los parámetros estimados. Es un diagnóstico de ajuste, no un contraste formal que incorpore incertidumbre de parámetros.

| Modelo | Tasa marginal esperada (%) | Rango central del 95 % de tasas simuladas (%) |
|---|---:|---:|
| Logístico: intercepto | 47,38 | 46,61–48,12 |
| Logístico: cuatro pendientes | 52,27 | 51,10–53,35 |
| Logístico: cuatro pendientes independientes | 50,73 | 49,65–51,74 |

El modelo de intercepto reproduce aproximadamente la tasa observada. Los dos modelos logísticos con cuatro pendientes la sobrestiman; la tasa observada queda por debajo del rango central de sus réplicas. Las medias simuladas concuerdan con las calculadas por integración adaptativa, dentro de la precisión Monte Carlo. Este desajuste observable refuerza la decisión de no dar por superada la comprobación binomial, incluso aunque los optimizadores devuelvan convergencia.

No se ha identificado de forma concluyente si el problema procede principalmente de la aproximación de Laplace, de la distribución normal de los efectos docentes, de una parametrización demasiado flexible para estas respuestas binarias, o de una combinación de estos factores. La literatura documenta que la estimación de GLMM binarios depende de la información por efecto aleatorio y del método de integración; eso motiva una comprobación adicional, no demuestra por sí solo la causa del resultado presente.

## 4. Qué hace exactamente la descomposición lineal

Para la respuesta del profesor i a la tarjeta j, el modelo suma:

`decisión = parte media con variables observadas + severidad adicional del profesor + respuesta adicional del profesor a los cuatro atributos + residuo`.

La primera parte contiene los seis atributos, la interacción suspensos × competencias, las características docentes, las creencias y el contexto experimental. La severidad adicional asigna un nivel común a las respuestas de cada profesor. Las cuatro pendientes permiten, por ejemplo, que un profesor reaccione más a los suspensos y menos al absentismo, aunque su severidad general sea parecida a la de otro profesor.

El modelo estima conjuntamente los efectos medios y una distribución de cinco efectos docentes (un intercepto y cuatro sensibilidades). No ajusta sin restricciones cinco coeficientes independientes a las pocas respuestas de cada profesor: las estimaciones individuales se apoyan en la distribución común.

Para construir los porcentajes se calculan cuatro cantidades:

1. **Variación de la parte observada:** la varianza del predictor que utiliza solo los efectos fijos. Se reparte entre atributos, características, creencias y contexto mediante Shapley: se proyecta ese predictor sobre cada combinación de bloques y se promedian sus incrementos en todos los órdenes. Es reparto descriptivo de información compartida.
2. **Severidad general adicional:** la varianza estimada del intercepto docente, definido en el perfil medio de referencia.
3. **Diferencias de criterio:** la varianza de la combinación de pendientes para cada perfil, promediada sobre las tarjetas observadas. Incluye las covarianzas entre pendientes. Al centrar los atributos globalmente, la covarianza entre intercepto y pendientes promedia cero; la distinción entre nivel y criterios depende de ese centrado y de la distribución de referencia.
4. **Residuo de cada respuesta:** la varianza residual estimada por el modelo lineal.

Se suman esas cuatro cantidades y se divide cada una por el total. Así se obtiene el 100 %. El 14,88 % describe la cuota que ese modelo atribuye a criterios heterogéneos; no es el porcentaje de profesores que tienen criterios distintos. El 73,14 % no es exactitud de clasificación, ni rendimiento ante decisiones nuevas, ni una cantidad que pueda sumarse al R² de la regresión con harshness.

El harshness observado sigue siendo un resumen útil de severidad. No es simultáneamente el intercepto aleatorio ni la suma de estas cuatro sensibilidades; no se ha introducido como un componente adicional en esta descomposición.

## 5. Decisión para el paper

Mantener la descomposición vigente mientras se resuelve la estimación binomial de la estructura ampliada. Conservar el modelo lineal de cuatro pendientes como análisis exploratorio documentado, sin presentar sus porcentajes como ya validados. Una siguiente investigación metodológica tendría que comprobar una estimación binomial con integración más precisa y/o regularización explícita y repetir la comprobación de ajuste. No se han hecho modificaciones del texto ni incorporado estos resultados al paper.

## Reproducción y fuentes

- `Rscript "2. Code/39. A_random_slopes_bootstrap.R" 199 8`: bootstrap; conserva réplicas completas y puede continuar el cálculo. Semilla independiente `930000 + réplica`.
- `Rscript --vanilla "2. Code/40. A_random_slopes_logit.R" --audit --calibration`: ajustes binomiales, integración y reproducción de tasas. `--reuse` reutiliza modelos guardados; `--calibration-only` utiliza la tabla de integración ya calculada. Semilla de simulación 930401.
- `python "2. Code/41. report_random_slopes_validation.py"`: genera este informe con las tablas finales.
- Los CSV `random_slopes_uncertainty`, `random_slopes_bootstrap_draws`, `random_slopes_logit_diagnostics`, `random_slopes_logit_partition`, `random_slopes_logit_covariances` y `random_slopes_logit_calibration`, y las sesiones de R, quedan junto al informe. Los modelos con información individual se guardan localmente en RDS ignorados por Git.

Johnson, P. C. D. (2014). Extension of Nakagawa & Schielzeth's R²GLMM to random slopes models. *Methods in Ecology and Evolution, 5*, 944–946. https://doi.org/10.1111/2041-210X.12225

Documentación de glmmTMB sobre diagnóstico de convergencia y Hessiana: https://glmmtmb.github.io/glmmTMB/articles/troubleshooting.html

Capanu, M., Gönen, M., & Begg, C. B. (2013). An assessment of estimation methods for generalized linear mixed models with binary outcomes. *Statistics in Medicine, 32*(26), 4550–4566. https://doi.org/10.1002/sim.5866
