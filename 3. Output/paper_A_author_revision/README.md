# Revisión autorizada de Paper A — 27/10/2026

Implementa los acuerdos 1–17 de `cambios_pendientes_manuscrito_A_revision_actual.txt`. La comparación parte del contenido aceptado en `8306e22`, inmediatamente anterior a esta ejecución. La comparación conservaba texto y exhibiciones retirados. El 01/11/2026 el autor aceptó todos los cambios: el PDF principal vigente es limpio y toma su contenido de `4. Latex/paper_A/paper_A_content.tex`, con el modo fijado en `manuscript_state.json`. La copia con marcas anterior se conserva en Git.

## Datos y reproducción

Los CSV contienen agregados; `local/` y `logs/` están excluidos de Git. Entradas locales: datos originales, los objetos de reconstrucción `3. Output/paper_A_final_revision/local/{scenario_B,scenario_B_full8,scenario_NoScreen,provenance}.rds`, y dependencias de preparación anteriores. `diagnostics/provenance.json` registra sus huellas. Para reconstruir esas entradas desde los datos, los scripts conservados `paper_A_final_revision/prepare.R` y la reconstrucción previa mantienen su función; esta revisión no modifica los datos ni los objetos originales.

Desde la raíz: `python "2. Code/run_pipeline.py"` reconstruye exhibiciones y ambos PDF canónicos desde agregados. `--analysis` vuelve a estimar A, incluida la predicción, con la biblioteca de R existente. `R_session.txt` documenta versiones. No ejecutar `revise_text.py` sobre futuras ediciones del autor.

`Main/`: 2.520 docentes, ocho tarjetas cada uno. `Common/`: 2.164 casos completos con las mismas referencias y cortes de terciles. `Partial/`, `NoShared/`, `NoScreen/` y `ByID/`: sensibilidades explícitas. La sensibilidad predictiva sin campos compartidos utiliza el criterio original de cuatro o más tarjetas no compartidas; su cohorte difiere de la sensibilidad descriptiva. Está identificada en el texto y la tabla.

## Ampliaciones y comprobaciones

- Descomposición: 199 remuestras aceptadas; 13 soluciones en frontera conservadas y declaradas. Gradiente, Hessiano, optimizadores y errores estándar auditados. Alternativas de covarianza en la misma muestra.
- Predicción: 399 reestimaciones completas por comparación; referencias y parámetros ajustados fuera del profesor evaluado. La ampliación con todas las covariables utiliza 2.164 docentes y exactamente las mismas tarjetas en ambos predictores. Los resultados originales de C se adoptaron una sola vez tras verificar entradas, particiones, puntos y procedencia; `prediction_adoption.json` conserva esa trazabilidad. La opción `prediction.R recompute` reproduce el algoritmo en el código de A sin depender del directorio de C.
- `audit_partitions.R`: 81 reestimaciones de particiones verificadas; sin pérdida de rango ni avisos distintos del aviso esperado por ponderaciones binomiales fraccionarias. `partition_audit.csv` registra los recuentos y errores numéricos.
- Soporte común: 12 combinaciones de otros atributos, 48 perfiles y 1.999 remuestras docentes. Intervalos simultáneos para la familia de tres contrastes.
- `methods_review.txt` y `editorial_review.txt`: revisiones independientes. `final_audit.json`: cierre programático. Las revisiones anteriores incluyen la inspección de las copias aceptada y con marcas. La aceptación oficial limpia incorpora un nuevo control de ausencia de marcas, referencias y exhibiciones vigentes; los informes fechados anteriores conservan su alcance histórico.

La tabla por terciles presenta diferencias de criterio. No se incorporan la tabla nueva de dispersiones, la interpretación sustantiva de correlaciones entre sensibilidades ni el análisis interno del ejemplo desfavorable. Se conserva el modelo conceptual y la descomposición como cierre; la predicción es una comprobación complementaria.
