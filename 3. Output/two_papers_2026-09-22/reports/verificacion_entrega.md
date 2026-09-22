# Verificación de la segunda entrega

22 de septiembre de 2026.

- Se conservaron 2.705 docentes y 22.124 decisiones. El índice se reconstruyó independientemente y se verificó su centrado ponderado. Las categorías académicas reconcilian con el total de respuestas.
- Las 1.999 réplicas de bootstrap son finitas. Los pesos por número de respuestas suman uno dentro de cada docente. Los rankings completos suman uno por brazo y extremo de preferencia.
- La predicción anidada tiene una predicción por docente, modelo y repetición, sin duplicados ni valores ausentes. Se verificaron las medias y los extremos de los cinco R² por modelo. Los objetivos y las transformaciones se reconstruyen tanto en divisiones externas como internas.
- Se verificaron las huellas de todas las tablas del manifiesto congelado antes de la redacción. El pulido posterior de figuras utilizó únicamente esos agregados; no se cambiaron estimaciones para acomodar el texto.
- Paper A: 21 páginas, 12 tablas y 4 figuras. Paper B: 23 páginas, 18 tablas y 6 figuras. Incluyen referencias y anexos, con la misma tipografía y espaciado de la primera versión.
- Ambos PDF se compilaron con pdfLaTeX y referencias resueltas. No hay avisos de cajas desbordadas ni referencias indefinidas. Se renderizaron todas las páginas y se inspeccionaron los mosaicos completos y páginas de tablas y figuras a tamaño ampliado. Los diez pasajes azules del cuerpo se verificaron contra el original.
- Los originales y la versión del 21 de septiembre no se sobrescribieron. No se añadieron datos individuales a Git.

La interpretación principal se revisó para no ocultar el contraste entre la prueba individual de suspensos y su prueba conjunta, la sensibilidad del resultado de equivalencia, ni el carácter aproximado de los intervalos predictivos. La evidencia interna de concentración por docente no se presenta como estabilidad temporal o fiabilidad del índice.

Los detalles verificables por máquina están en `delivery_audit.json`, `validation_deepening.txt` y `results_frozen_manifest.csv`. Los renderizados de control se guardan en `tmp/two_papers_v2_pdf_qa/` y no forman parte de los manuscritos.
