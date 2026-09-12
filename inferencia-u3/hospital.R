# =============================================================================
# Evaluacion del nuevo protocolo de atencion en guardia
# Hospital Publico Provincial (ficticio) - Area de Estadistica y Calidad
# =============================================================================
library(effectsize)
library(DescTools)

datos <- read.csv("hospital protocolo.csv")
adherencia <- read.csv("hospital adherencia.csv")
piloto <- read.csv("hospital piloto salaA.csv")

datos$protocolo <- factor(datos$protocolo, levels = c("Antes", "Despues"))
piloto$protocolo <- factor(piloto$protocolo, levels = c("Antes", "Despues"))

# -------------------------------------------------------------
# 1) Resultados preliminares: Sala A, primeras dos semanas
# -------------------------------------------------------------

antes_pilot <- piloto$tiempo_espera[piloto$protocolo == "Antes"]
desp_pilot  <- piloto$tiempo_espera[piloto$protocolo == "Despues"]

mean(antes_pilot) #Tiempo de espera promedio (Sala A, pre-implementacion)
mean(desp_pilot) #Tiempo de espera promedio (Sala A, primeras 2 semanas)
d <- cohens_d(antes_pilot, desp_pilot)
# Nota: resultado preliminar muy alentador, d grande. Confirma que el nuevo protocolo funciona.
# ------------------------------------------------------------------------------
# Correcciones comparación Sala A
# ------------------------------------------------------------------------------
# Varianza muestral
cat(sprintf("Varianzas: antes = %.2f, después = %.2f", var(antes_pilot), var(desp_pilot)))
# Test t de Studen
t_res <- t.test(antes_pilot, desp_pilot, var.equal = T)
# Corrección por muestra pequeña (n = 8 por grupo)
g <- hedges_g(antes_pilot, desp_pilot)
# "t(48) = 2,54, p = 0,015, d = 0,65, IC 95% [0,12; 1,17] diferencia entre grupos = 5,2 puntos, g = 0,48, IC 95% [0,08; 0,88]"
cat(sprintf("t(%d) = %.2f, p = %.3f, d = %.2f, IC 95%% [%.2f; %.2f] diferencia entre grupos = %.2f puntos, g = %.2f, IC 95%% [%.2f; %.2f]",
        t_res$parameter, t_res$statistic, t_res$p.value, d$Cohens_d, d$CI_low, 
        d$CI_high, diff(t_res$estimate), g$Hedges_g, g$CI_low, g$CI_high))
# -------------------------------------------------------------
# 2) Tiempo de espera: muestra completa, todas las salas
# -------------------------------------------------------------

t_result <- t.test(tiempo_espera ~ protocolo, data = datos);t_result

# La diferencia es estadisticamente significativa (p < 0.001), lo que confirma la efectividad del nuevo protocolo en toda la red de guardias.

# -------------------------------------------------------------
# 3) Reingreso a 30 dias
# -------------------------------------------------------------

tabla_reingreso <- table(datos$protocolo, datos$reingreso_30d)
dimnames(tabla_reingreso) <- list(Protocolo = c("Antes", "Despues"), Reingreso = c("No", "Si"))
tabla_reingreso

chisq.test(tabla_reingreso)

tabla_reingreso["Antes", "Si"] / sum(tabla_reingreso["Antes", ]) #Tasa de reingreso - Antes
tabla_reingreso["Despues", "Si"] / sum(tabla_reingreso["Despues", ]) #Tasa de reingreso - Despues

oddsratio(tabla_reingreso)

1-oddsratio(tabla_reingreso)$Odds_ratio # % de reducción del reingreso

# -------------------------------------------------------------
# 4) Modelo de regresion logistica para reingreso
# -------------------------------------------------------------

datos$edad_std <- scale(datos$edad)
modelo_logit <- glm(reingreso_30d ~ protocolo + edad_std + comorbilidades,family = binomial, data = datos)
summary(modelo_logit)

r2_nagelkerke <- PseudoR2(modelo_logit, which = "Nagelkerke");r2_nagelkerke #El modelo explica una proporcion razonable de la variabilidad en el reingreso.

# -------------------------------------------------------------
# 5) Adherencia al tratamiento segun turno
# -------------------------------------------------------------

adherencia <- merge(adherencia, datos[, c("id", "turno")], by = "id")
adherencia$turno <- factor(adherencia$turno, levels = c("Manana", "Tarde", "Noche"))

modelo_turno <- lm(adherencia ~ turno, data = adherencia)
anova(modelo_turno)

eta_turno <- eta_squared(modelo_turno, partial = TRUE)
eta_turno # Proporcion de variabilidad explicada por el modelo

# =============================================================================
# CONCLUSIONES
# =============================================================================
# 1. Los resultados preliminares en Sala A mostraron una mejora marcada en los tiempos de espera (d grande), confirmada luego en el analisis de toda la red de guardias (p < 0.001).
# 2. El nuevo protocolo redujo la tasa de reingreso a 30 dias en un 25% (odds ratio).
# 3. El modelo de regresion logistica explica una proporcion razonable de la variabilidad en el reingreso (Pseudo R2 Nagelkerke = 0.063).
# 4. El turno explica una porcion relevante de la variabilidad en la adherencia al tratamiento; se recomienda reforzar al personal del turno noche.
# 5. Se recomienda extender el nuevo protocolo a toda la red hospitalaria provincial de forma inmediata.
