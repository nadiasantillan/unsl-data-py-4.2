# =============================================================
# Analisis de imagen publica - Candidato ficticio
# Consultora de Comunicacion Politica "Vector Estrategico"
# Encuesta Nacional (tracking trimestral) y Encuesta Rapida (post-streaming)
# Analista: Unidad de Analisis de Datos
# =============================================================

# DescTools provee MeanDiffCI(), que calcula el intervalo de confianza
# para la diferencia de medias entre dos grupos. 
library(DescTools)

datos <- read.csv("encuesta imagen publica.csv")


datos$encuesta <- factor(datos$encuesta, levels = c("Nacional", "Rapida"))
datos$grupo_etario <- factor(datos$grupo_etario, levels = c("Adulto", "Joven"))

# Variables del analisis
variables <- c("confianza", "cercania", "honestidad", "liderazgo",
               "gestion", "carisma", "intencion_voto")

# -------------------------------------------------------------
# 1) Diferencias por grupo etario DENTRO de cada encuesta
#    (se hacen todas las pruebas t que hicieron falta, una por
#    variable y por encuesta, sin ajustar el nivel de significancia)
# -------------------------------------------------------------

cat("=== ENCUESTA NACIONAL: diferencias Joven vs Adulto ===\n")

nacional_df <- list()
i <- 0
for (v in variables) {
  i <- i + 1
  formula_v <- as.formula(paste(v, "~ grupo_etario"))
  datos_nac <- subset(datos, encuesta == "Nacional")
  
  # Test t de diferencia de medias entre Joven y Adulto para la
  # variable v. 
  resultado <- t.test(formula_v, data = datos_nac)
  cat(sprintf("%-15s t = %.2f, p = %.4f\n", v, resultado$statistic, resultado$p.value)) 
  
  # Se extraen los valores individuales de cada grupo (independiente
  # del t.test) para poder pasarselos a MeanDiffCI
  jovenes <- datos[datos$grupo_etario=="Joven"&datos$encuesta=="Nacional", v]
  adultos <- datos[datos$grupo_etario=="Adulto"&datos$encuesta=="Nacional", v]
  
  # Intervalo de confianza al 99% para la diferencia de medias
  # (adultos - jovenes).
  ic299 <- MeanDiffCI(adultos, jovenes, conf.level = 0.99);
  
  # Se arma una fila con: medias de cada grupo, diferencia de medias,
  # limites del IC 95% y limites del IC 99%, los nuevos calculados
  # con MeanDiffCI).
  # resultado$estimate trae las dos medias muestrales,
  # resultado$conf.int trae el IC 95% para la diferencia de medias
  nacional_df[[i]] <- data.frame(variable = v, 
                                 g1 = resultado$estimate[1], 
                                 g2 = resultado$estimate[2],
                                 diff = resultado$estimate[1]-resultado$estimate[2],
                                 l95 = resultado$conf.int[1],
                                 u95 = resultado$conf.int[2],
                                 l99 = ic299["lwr.ci"],
                                 u99 = ic299["upr.ci"])
}

print(do.call(rbind, nacional_df), row.names=F)

cat("\n=== ENCUESTA RAPIDA: diferencias Joven vs Adulto ===\n")
# Mismo procedimiento que el bloque anterior, pero sobre la Encuesta
# Rapida en lugar de la Nacional
rapida_df <- list()
i <- 0
for (v in variables) {
  i <- i + 1
  formula_v <- as.formula(paste(v, "~ grupo_etario"))
  datos_rap <- subset(datos, encuesta == "Rapida")
  resultado <- t.test(formula_v, data = datos_rap)
  jovenes <- datos[datos$grupo_etario=="Joven"&datos$encuesta=="Rapida", v]
  adultos <- datos[datos$grupo_etario=="Adulto"&datos$encuesta=="Rapida", v]
  
  # IC 99% para la diferencia de medias adultos - jovenes, dentro de
  # la Encuesta Rapida
  ic299 <- MeanDiffCI(adultos, jovenes, conf.level = 0.99);
  
  cat(sprintf("%-15s t = %.2f, p = %.4f\n", v, resultado$statistic, resultado$p.value)) 
  rapida_df[[i]] <- data.frame(variable = v, 
                               g1 = resultado$estimate[1], 
                               g2 = resultado$estimate[2],
                               diff = resultado$estimate[1]-resultado$estimate[2],
                               l95 = resultado$conf.int[1],
                               u95 = resultado$conf.int[2],
                               l99 = ic299["lwr.ci"],
                               u99 = ic299["upr.ci"])
}
print(do.call(rbind, rapida_df), row.names=F)

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

# Ahora la comparacion es entre encuestas (Nacional vs Rapida),
# usando a todos los encuestados de cada encuesta para cada variable
ambas_df <- list()
i <- 0
for (v in variables) {
  i <- i + 1
  nacional <- datos[datos$encuesta=="Nacional", v]
  rapida <- datos[datos$encuesta=="Rapida", v]
  formula_v <- as.formula(paste(v, "~ encuesta"))
  resultado <- t.test(formula_v, data = datos)
  
  # IC 99% para la diferencia de medias nacional - rapida
  ic299 <- MeanDiffCI(nacional, rapida, conf.level = 0.99);
  cat(sprintf("%-15s t = %.2f, p = %.4f\n", v, resultado$statistic, resultado$p.value)) 
  ambas_df[[i]] <- data.frame(variable = v, 
                              g1 = resultado$estimate[1], 
                              g2 = resultado$estimate[2],
                              diff = resultado$estimate[1]-resultado$estimate[2],
                              l95 = resultado$conf.int[1],
                              u95 = resultado$conf.int[2],
                              l99 = ic299["lwr.ci"],
                              u99 = ic299["upr.ci"])
}
print(do.call(rbind, ambas_df), row.names=F)

# Nota del analista: la intencion de voto no mostro una diferencia
# significativa entre la Rapida y la Nacional (p = 0.3955), por lo que
# se concluye que el momento viral del streaming no tuvo ningun impacto
# sobre la intencion de voto.

# -------------------------------------------------------------
# 3) Modelo lineal: intencion de voto en funcion de encuesta y grupo etario
# -------------------------------------------------------------

# Modelo de regresion lineal multiple: intencion_voto explicada por
# el tipo de encuesta y el grupo etario
modelo <- lm(intencion_voto ~ encuesta + grupo_etario, data = datos)
cat("\n=== MODELO LINEAL: intencion_voto ~ encuesta + grupo_etario ===\n")
print(summary(modelo)$coefficients)

cat("\nCoeficientes CI 99%")
confint(modelo, level=0.99)
# Resumen modelo: el ajuste explica sólo el 0.05 % de la varianza y el
# valor p del modelo es > 0.05, indicando que no es significativo.
summary(modelo)


# Análisis de residuos: no se cumplen el supuesto de homocedasticidad

# Normalidad
plot(modelo, which=2, main ="QQ Residuos")
# Homocedasticidad: el gráfico de residuos vs intención de voto muestra amplitudes
# mayores de los residuos alrededor del eje x para los distintos valores de 
# intención de voto
plot(modelo, which=1, caption ="Residuos vs Intención de Voto")
# El boxplot de residuos por grupo confirma las varianzas diferentes.
boxplot(residuals(modelo) ~ datos$encuesta + datos$grupo_etario, 
        main = "Residuos por grupo",
        xlab = "Grupo", ylab = "Residuo")
abline(h = 0, col = "red", lty = 2)
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