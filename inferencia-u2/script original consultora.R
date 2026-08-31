# =============================================================
# Analisis de imagen publica - Candidato ficticio
# Consultora de Comunicacion Politica "Vector Estrategico"
# Encuesta Nacional (tracking trimestral) y Encuesta Rapida (post-streaming)
# Analista: Unidad de Analisis de Datos
# =============================================================

datos <- read.csv("encuesta imagen publica.csv")

datos$encuesta <- factor(datos$encuesta, levels = c("Nacional", "Rapida"))
datos$grupo_etario <- factor(datos$grupo_etario, levels = c("Adulto", "Joven"))

variables <- c("confianza", "cercania", "honestidad", "liderazgo",
                "gestion", "carisma", "intencion_voto")

# -------------------------------------------------------------
# 1) Diferencias por grupo etario DENTRO de cada encuesta
#    (se hacen todas las pruebas t que hicieron falta, una por
#    variable y por encuesta, sin ajustar el nivel de significancia)
# -------------------------------------------------------------

cat("=== ENCUESTA NACIONAL: diferencias Joven vs Adulto ===\n")
for (v in variables) {
  formula_v <- as.formula(paste(v, "~ grupo_etario"))
  datos_nac <- subset(datos, encuesta == "Nacional")
  resultado <- t.test(formula_v, data = datos_nac)
  cat(sprintf("%-15s t = %.2f, p = %.4f\n", v, resultado$statistic, resultado$p.value))
}

cat("\n=== ENCUESTA RAPIDA: diferencias Joven vs Adulto ===\n")
for (v in variables) {
  formula_v <- as.formula(paste(v, "~ grupo_etario"))
  datos_rap <- subset(datos, encuesta == "Rapida")
  resultado <- t.test(formula_v, data = datos_rap)
  cat(sprintf("%-15s t = %.2f, p = %.4f\n", v, resultado$statistic, resultado$p.value))
}

# Nota del analista: en la Nacional, confianza, liderazgo, gestion e
# intencion_voto dan diferencias significativas (p < 0.05). Esto muestra
# que los jovenes tienen una imagen sistematicamente mas favorable del
# candidato en varias dimensiones clave.
#
# En la Rapida, honestidad da p = 0.0138, significativo: el streaming
# genero un salto notable en la percepcion de honestidad entre los
# jovenes.

# -------------------------------------------------------------
# 2) Efecto del streaming: Encuesta Rapida vs Encuesta Nacional
# -------------------------------------------------------------

cat("\n=== EFECTO DEL STREAMING: Rapida vs Nacional ===\n")
for (v in variables) {
  formula_v <- as.formula(paste(v, "~ encuesta"))
  resultado <- t.test(formula_v, data = datos)
  cat(sprintf("%-15s t = %.2f, p = %.4f\n", v, resultado$statistic, resultado$p.value))
}

# Nota del analista: la intencion de voto no mostro una diferencia
# significativa entre la Rapida y la Nacional (p = 0.3955), por lo que
# se concluye que el momento viral del streaming no tuvo ningun impacto
# sobre la intencion de voto.

# -------------------------------------------------------------
# 3) Modelo lineal: intencion de voto en funcion de encuesta y grupo etario
# -------------------------------------------------------------

modelo <- lm(intencion_voto ~ encuesta + grupo_etario, data = datos)
cat("\n=== MODELO LINEAL: intencion_voto ~ encuesta + grupo_etario ===\n")
print(summary(modelo)$coefficients)

# Nota del analista: el termino de encuesta no es significativo (p = 0.407),
# lo que confirma que el streaming no genero cambios en la intencion de
# voto. El termino de grupo etario si es significativo (p = 0.034): los
# jovenes muestran una intencion de voto significativamente mayor que los
# adultos, un hallazgo relevante para la segmentacion de la campania.

# =============================================================
# CONCLUSION DEL ANALISTA
# =============================================================
# 1. Los jovenes tienen una imagen significativamente mas favorable del
#    candidato en confianza, liderazgo, gestion e intencion de voto.
# 2. El streaming produjo un salto significativo en honestidad percibida
#    entre los jovenes.
# 3. El streaming no tuvo impacto sobre la intencion de voto general.
# 4. La campania deberia orientar sus recursos de comunicacion hacia el
#    segmento joven, que responde mejor al candidato en todas las
#    dimensiones relevantes.
