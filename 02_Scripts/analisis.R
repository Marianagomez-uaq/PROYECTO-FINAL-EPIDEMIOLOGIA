# ESTUDIO DE COHORTE RETROSPECTIVO

respuestas <- read.csv("01_RawData/respuestas.csv") # Generar un objeto que contenga las respuestas, para no modificar el archivo original

# Cribado inicial

respuestas <- respuestas [,c(-3,-24,-25)] # Quita la columna puntuación y las últimas 2, ya que están vacías

respuestas <- subset (respuestas, consentimiento == "Sí, entiendo y estoy de acuerdo con participar.") # Quita las respuestas de participantes que no dieron su consentimiento

respuestas <- subset (respuestas, es_estudiante == "Sí (1)") # Filtra los participantes que no son estudiantes universitarios

# Separar el database en expuestos y no expuestos

expuestos <- subset (respuestas, asistencia_campaña == "Sí (1)")
no_expuestos <- subset (respuestas, asistencia_campaña == "No (2)")

# Separar los 2 grupos según diagnóstico

positivo_expuesto <- subset (expuestos, diagnostico_positivo == "Sí  (1)") # aquí en el Sí  (1), se nos fue un espacio de más en los paréntesis
negativo_expuesto <- subset (expuestos, diagnostico_positivo == "No (2)")
positivo_no_expuesto <- subset (no_expuestos, diagnostico_positivo == "Sí  (1)")
negativo_no_expuesto <- subset (no_expuestos, diagnostico_positivo == "No (2)")

# Conteo de individuos por grupo

conteo_p_ex <- length(positivo_expuesto$consentimiento) # Se usó la variable consentimiento para hacer el conteo
conteo_n_ex <- length(negativo_expuesto$consentimiento)
conteo_p_nex <- length(positivo_no_expuesto$consentimiento)
conteo_n_nex <- length(negativo_no_expuesto$consentimiento)

# Dataframe para el análisis

grupos <- data.frame (ITS_pos = c(conteo_p_ex, conteo_p_nex), ITS_neg = c(conteo_n_ex, conteo_n_nex))
row.names(grupos) <- c("expuestos", "no_expuestos")

grupos
