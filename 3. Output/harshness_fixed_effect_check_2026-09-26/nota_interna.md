# Harshness y efectos fijos: comprobación interna, 26/09/2026

Se reproduce el modelo lineal de los seis atributos con efectos fijos de profesor del manuscrito. Muestra: 22.006 respuestas de 2.587 docentes; se excluyen los 118 docentes con una sola respuesta, igual que en el modelo original. Se verifica la reproducción de sus seis coeficientes con tolerancia 1e-10.

Para cada docente, el efecto fijo estimado satisface alpha_j = promedio_i(y_ij - x_ij' beta). Se verifica esta identidad frente a fixef(). Se compara con H^b original, que resta la media de otros docentes para cada tarjeta y conserva las referencias originales de toda la muestra. Ambos indicadores se centran por su media no ponderada en los mismos 2.587 docentes para representar las diferencias sin una constante arbitraria. Esta operación es exclusiva del gráfico y no cambia el indicador del manuscrito.

| Muestra de comparación | N | Pearson | Spearman | DE H^b (pp) | DE efecto fijo (pp) | Raíz del error cuadrático entre medidas centradas (pp) |
|---|---:|---:|---:|---:|---:|---:|
| Docentes del modelo FE | 2.587 | 0,99840 | 0,97676 | 18,964 | 18,949 | 1,073 |
| Al menos ocho respuestas | 2.369 | 0,99839 | 0,97065 | 18,688 | 18,673 | 1,059 |

Las distribuciones y la correlación lineal son muy semejantes. La correlación de rangos no es idéntica y los indicadores no son algebraicamente iguales: H^b ajusta por referencia empírica de tarjeta, mientras que alpha utiliza la predicción aditiva de los atributos. La semejanza muestra que, en estos datos y en esta muestra, el ordenamiento lineal general cambia poco entre ambas formas de ajuste. No constituye validación independiente ni identifica la dispersión de un rasgo latente: ambas medidas reutilizan las mismas respuestas y contienen error de estimación.

No comparar las desviaciones estándar anteriores directamente con la distribución completa de H^b (2.705 docentes): la muestra es distinta. Tampoco interpretar un efecto fijo negativo sin tener en cuenta la normalización/intercepto. El gráfico utiliza diferencias respecto a la media de cada medida.

Relación conceptual con el marco propuesto: las diferencias no observadas eta_j en diagnóstico, valoración de la repetición y costes pueden contribuir al efecto fijo empírico. Sin embargo, alpha_j resume también diferencias asociadas a características observadas del docente que son constantes dentro de él, contexto de asignación y otras diferencias de nivel. No es una medida aislada de eta_j. Si las diferencias docentes modifican el peso de los atributos, un único intercepto tampoco las recoge enteramente.

Recomendación: mantener H^b como indicador principal, con su distribución en el bloque de diferencias entre docentes, y conservar esta comparación solo para comprensión interna, como pidió el usuario. No añadir otra distribución o anexo al paper por defecto.

Sobre la comprobación pendiente entre mitades: comenzar por H^b y su distribución es narrativamente razonable. El ajuste por tarjeta ya atiende a la composición observada de los casos; las mitades repetidas preguntan si la posición relativa del docente se reproduce en subconjuntos distintos de sus respuestas. No eliminan por sí solas todas las diferencias de asignación ni garantizan el resultado. Los referentes de tarjeta compartidos requieren el cuidado metodológico ya registrado en A-001. Esta comprobación aún no se ha ejecutado.

Archivos: harshness_vs_fixed_effect.png y distributions.png. No se exportan identificadores individuales. Reproducción desde raíz: Rscript --vanilla "2. Code/24. harshness_fixed_effect_check.R". Se guardan resultados agregados, hashes de entrada y sesión R. Manuscritos, PDF y resultados existentes permanecen intactos.
