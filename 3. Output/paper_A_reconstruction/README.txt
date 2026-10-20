RECONSTRUCCIÓN AISLADA DEL PAPER A

No sustituye el manuscrito ni las salidas vigentes. baseline_snapshot conserva una copia de los documentos y agregados actuales. protected_manifest.json fija sus huellas y las de los inputs protegidos. Las ediciones previas del autor siguen en sus rutas originales.

Escenarios principales: A referencia actual reproducida; B respuestas guardadas de tarjetas asignadas por FL, aceptando explícitamente campos compartidos; C B sin observaciones coincidentes con los ejemplos específicos del registro; D docentes sin coincidencia alguna. Se mantiene el filtro legacy en A–D.

Sensibilidades: B_full8, C_full8 (ocho valores guardados en la secuencia, no prueba de recontestación); C_inclusive sin filtro de decisión; C_example_screen con filtro del ejemplo favorable específico. Cada escenario reconstruye referencias, medidas, terciles y casos completos.

Los modelos de descomposición corregidos convergen en la frontera (covarianza singular). Se conservan seis pendientes correlacionadas, 15 interacciones, covariables actuales y harshness observado; sin intercepto aleatorio ni bloque experimental. La dispersión bootstrap es descriptiva y condicionada al estimador; no establece cobertura nominal en la frontera. No se elige una solución por sus porcentajes.

Código: 2. Code/paper_A_reconstruction/. R 4.4.0 y paquetes ya instalados. Los workers deben usar --vanilla y las mismas librerías/versiones que el proceso principal; no restaurar renv ni instalar paquetes para esta comparación. El remuestreo inicial se canceló antes de guardar réplicas al detectar que la activación automática de renv cargaba lme4 1.1.38 en workers, frente a 1.1.35.3 en el proceso principal. Las ejecuciones aceptadas verifican lme4 1.1.35.3 en ambos.

Datos individuales, tabla docente/posición y modelos: local/, excluida de Git. No publicar ni versionar su contenido. Los CSV fuera de local/ son agregados y parámetros de modelos. La unidad analítica es el registro de encuesta con ResponseId único, no una deduplicación demostrada entre personas.

El informe HTML es autónomo para texto, tablas y gráficos; los enlaces a CSV requieren conservar esta carpeta. Consultar diagnostics/ y los estados por escenario para semillas, fallos y convergencia. Los 999 sorteos emparejados comparan diferencias sin tratar las estimaciones como independientes. El remuestreo de la descomposición principal usa 199 sorteos por escenario y reconstruye referencias, scores, centrado y asignación en cada uno.

Los documentos supporting_analyses conservados fuera del manuscrito no forman un segundo paper ni se reescriben. Se recalculan aquí consistencia por mitades y bootstrap de caracterización porque dependen de la identificación de tarjetas. No se han reentrenado los modelos predictivos archivados fuera del manuscrito ni se presentan como validados para las nuevas muestras.

EJECUCIÓN Y REANUDACIÓN
Desde la raíz del repositorio:
  python "2. Code/paper_A_reconstruction/run.py" --status
  python "2. Code/paper_A_reconstruction/run.py" --workers 4
No iniciar el runner mientras otros procesos de esta reconstrucción sigan activos. Las fuentes numéricas se fijan en diagnostics/numerical_source_manifest.json; un cambio exige revisar explícitamente los caches. El runner completa etapas pendientes y verifica los archivos protegidos antes y después. No llama a la pipeline canónica.

AUDITORÍA NUMÉRICA ADICIONAL
La comprobación preespecificada de sorteos 1, 50 y 199 detectó que iniciar nloptwrap exactamente en la covarianza singular de B podía detenerse en un objetivo REML peor, aunque devolviese código 0. Sus 199 réplicas v1 se conservan únicamente en B/numerical_audit_v1, excluidas de la incertidumbre aceptada. B se recalcula con inicio automático. C/D conservan las réplicas v1 cuyos controles de inicio concuerdan y completan el resto con inicio automático; cada fila identifica la configuración. La nueva configuración comprueba además gradiente combinado y Hessiana, con bobyqa como alternativa numérica. No cambia muestras, especificación, REML, remuestreo ni reparto. Los controles de tolerancias estándar frente a estrictas se exportan en optimizer_control_audit.csv. Las diferencias se juzgan por el objetivo, nunca por recuperar porcentajes deseados.

ESTADO DEL PRIMER COMMIT
Preparación, auditoría de procedencia, ocho escenarios, estimaciones puntuales, 999 sorteos de caracterización y comparación emparejada, apéndices y 103 invariantes completados. El bootstrap A tiene199 réplicas; la corrección numérica de B y la finalización C/D continúan. El informe no se etiqueta como completo hasta superar final_deliverable_checks.csv y sus verificaciones de incertidumbre e integridad.
