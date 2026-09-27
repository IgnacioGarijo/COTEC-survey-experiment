# Revisión integrada del paper A

Autorizada el 27/09/2026. Fuente: `Cambios_pendientes_manuscrito_A.txt` (A-001–A-015) y `cambios_pendientes_manuscrito_A_autores.txt` completo. Las decisiones posteriores explícitas sustituyen las anteriores incompatibles. El paper B y los manuscritos históricos se conservan. Entrega nueva en `4. Latex/paper_A_2026-09-27/`.

## Plan de ejecución

1. Actualizar covariables (recursos excesivos 0–10, sin antigüedad en centro), muestras comunes, caracterización, bootstrap con reconstrucción de referencias, alternativas de harshness, terciles y validación anidada/SHAP. Comparación interna de especificaciones sobre muestra anterior.
2. Calcular consistencia en mitades aleatorias con referencias externas a los docentes evaluados. Añadir descomposición multinivel de decisiones con escala, denominador y reparto entre bloques explícitos, incertidumbre y sensibilidades.
3. Reconstruir flujo, exclusiones y disponibilidad de módulos; estudiar orden/tiempos solo cuando los campos lo permitan. Integrar comparación poblacional univariante actualizada en ese mismo apéndice.
4. Reescribir marco de diagnóstico y respuesta educativa y literatura. Resultados en tres bloques: atributos dentro de profesor; harshness, desacuerdo y terciles entre docentes; asociaciones, predicción y descomposición. Dar espacio a los seis atributos y al contraste con desigualdades reales.
5. Integrar anexos por función, eliminar duplicaciones y secciones obsoletas; actualizar resumen, introducción, discusión y conclusión desde los resultados finales. Mantener colores de procedencia y citas verificadas.
6. Auditar cifras, referencias, muestras, tablas/figuras citadas y congruencia entre secciones; compilar y revisar visualmente todas las páginas. Registrar cumplimiento y commits sustantivos.

## Correspondencia de decisiones

- A-001: mitades en anexo, permutación secundaria; A-002/003: literatura y explicación positiva del diseño.
- A-004/009: covariables y recálculos; A-005/006/008: marco y seis atributos.
- A-007/011/012: tres contribuciones, harshness consistente y terciles en cuerpo.
- A-010: simplificar cierre y retirar anexo exclusivo de secuencias.
- A-013/014: un único apéndice de muestra, abandono, agotamiento y composición poblacional.
- A-015: descomposición de variación con componente docente residual, sin usar harshness como predictor.
- Autores 1, 2.3, 3.2, 3.8/9/13/17, 4.1–4.3, 5.1: políticas fuera del argumento principal; sensibilidad de control y transparencia mínima en anexo; B permanece aparte.
- Autores 2.1/2.2: conservar respuestas/soporte existentes sin nuevas exclusiones; 2.4/2.6/3.6/3.7/3.10–16: etiquetas, inferencia, claridad y presentación; 2.5/3.3: limitada explicación/predicción sin exagerar igualdad.
- Autores 3.4/5.2: ventajas del diseño, contexto complejo, edad y experiencia; decisión posterior A-004 sustituye compuesto de recursos.
- Autores 3.5/4.4: terciles descriptivos, sin validar tres tipos; 5.3: muestras y agrupamiento por docente, comprobación de orden existente; 5.4: presentación coherente.

## Estado

Completado el 27/09/2026. Manuscrito nuevo en `4. Latex/paper_A_2026-09-27/paper_A_teacher_decisions.pdf`: 22 páginas, 13 tablas, 7 figuras/diagramas y 18 referencias. Los análisis se finalizaron antes de reescribir el texto; posteriormente se auditó la coherencia y la maquetación.

## Ejecución y resultado

- Pasos 1–2: scripts 27–29 y 36. Muestra común actualizada a 2.157 docentes; recursos excesivos en escala original, edad y experiencia sin antigüedad de centro. Bootstrap de harshness con 999 réplicas, 1.000 particiones de respuestas y 399 réplicas de la descomposición. Mitades: correlación media 0,483; OLS completo: R² predictivo 3,72 %. El efecto de esfuerzo ya no se describe como preciso en la especificación completa.
- Paso 2 / A-015: la partición lineal suma 100 % y utiliza un reparto por proyección Shapley definido explícitamente. Componente docente residual de 8,12 %; sensibilidad logística observada de 12,36 %. Ambos se explican, distinguiendo modelo mixto, efectos fijos, harshness y predicción fuera de muestra.
- Paso 3: scripts 30–32. Flujo, disponibilidad, orden/tiempos y comparación poblacional univariante reunidos en el apéndice A. No se inventan denominadores de invitación ni puntos de abandono irrecuperables. Se mantienen los criterios originales de inclusión.
- Pasos 4–5: marco conceptual nuevo, literatura actualizada y resultados en tres bloques. Terciles con descriptivos y regresiones en el cuerpo; mitades, definiciones alternativas y bootstrap en B; detalles predictivos y partición en C. El resumen, la introducción, la discusión y la conclusión se reescribieron conjuntamente. Las notas de autores se aplicaron conforme a la correspondencia anterior y a las decisiones posteriores.
- Paso 6: scripts 33–35 y revisión visual de las 22 páginas. Cifras y muestras cotejadas con resultados agregados; toda tabla/figura mencionada; bibliografía utilizada y ordenada; sin referencias indefinidas ni desbordamientos de LaTeX. Detalles en `3. Output/paper_A_2026-09-27/reports/final_audit.json` y explicación para los autores en `4. Latex/paper_A_2026-09-27/LEEME.md`.

Commits sustantivos: `a4d6e1d` (plan y ejecución inicial), `da5b5f8` (resultados recalculados y auditoría estadística), más el commit final de manuscrito, maquetación y cierre del registro.
