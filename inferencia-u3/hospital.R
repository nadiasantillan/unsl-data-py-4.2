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
# Correcciones comparación - Sala A
# ------------------------------------------------------------------------------
# Varianza muestral
cat(sprintf("Varianzas: antes = %.2f, después = %.2f", var(antes_pilot), var(desp_pilot)))
# Test t de Studen
t_res <- t.test(antes_pilot, desp_pilot, var.equal = T)
# Corrección por muestra pequeña (n = 8 por grupo)
g <- hedges_g(antes_pilot, desp_pilot)
# "t(48) = 2,54, p = 0,015, d = 0,65, IC 95% [0,12; 1,17] diferencia entre grupos = 5,2 puntos, g = 0,48, IC 95% [0,08; 0,88]"
cat(sprintf("t(%d) = %.2f, p = %.3f, diferencia entre grupos = %.2f IC 95%% [%.2f; %.2f] minutos, g = %.2f, IC 95%% [%.2f; %.2f]",
        t_res$parameter, t_res$statistic, t_res$p.value, diff(t_res$estimate), 
        -t_res$conf.int[2], -t_res$conf.int[1], g$Hedges_g, g$CI_low, g$CI_high))
# -------------------------------------------------------------
# 2) Tiempo de espera: muestra completa, todas las salas
# -------------------------------------------------------------

t_result <- t.test(tiempo_espera ~ protocolo, data = datos, var.equal=F);t_result
t_result
# La diferencia es estadisticamente significativa (p < 0.001), lo que confirma la efectividad del nuevo protocolo en toda la red de guardias.
# ------------------------------------------------------------------------------
# Correcciones comparación - Todas las salas
# ------------------------------------------------------------------------------
d_todas_salas <- cohens_d(tiempo_espera ~ protocolo, data = datos)
g_todas_salas <- hedges_g(tiempo_espera ~ protocolo, data = datos)
glass_todas_salas <- glass_delta(tiempo_espera ~ protocolo, data = datos)
# Glass delta, d de Cohen y g de Hedges coinciden
cat(sprintf("t(%.2f) = %.2f, p = %.3f, d = %.2f, IC 95%% [%.2f; %.2f] diferencia entre grupos = %.2f IC 95%% [%.2f; %.2f] minutos\n",
            t_result$parameter, t_result$statistic, t_result$p.value, 
            d_todas_salas$Cohens_d, d_todas_salas$CI_low, d_todas_salas$CI_high, 
            diff(t_result$estimate), -t_result$conf.int[2], -t_result$conf.int[1]))

# -------------------------------------------------------------
# 3) Reingreso a 30 dias
# -------------------------------------------------------------

tabla_reingreso <- table(datos$protocolo, datos$reingreso_30d)
dimnames(tabla_reingreso) <- list(Protocolo = c("Antes", "Despues"), Reingreso = c("No", "Si"))
tabla_reingreso

chisq.test(tabla_reingreso)
tabla_reingreso["Antes", "Si"] / sum(tabla_reingreso["Antes", ]) #Tasa de reingreso - Antes
tabla_reingreso["Despues", "Si"] / sum(tabla_reingreso["Despues", ]) #Tasa de reingreso - Despues

or <- oddsratio(tabla_reingreso)

1-or$Odds_ratio # % de reducción del reingreso
# ------------------------------------------------------------------------------
# Correcciones comparación - Reingreso a 30 días
# ------------------------------------------------------------------------------
# No se debe aplicar la corrección de Yates, ninguna de la frecuencias es menor 
# a 5
chs_t <- chisq.test(tabla_reingreso, correct = F)
cat(sprintf("χ² = %.2f, p = %.3f", chs_t$statistic, chs_t$p.value))
# Intervalo de confianza
m <- matrix(c(70, 130, 310, 430), nrow=2, byrow=T)
oddsratio(m)
cat(sprintf("Odds ratio %.2f IC 95%% [%.2f, %.2f] ", or$Odds_ratio, or$CI_low, or$CI_high))
cat(sprintf("Disminución %.2f IC 95%% [%.2f, %.2f] %%", 1-or$Odds_ratio, 1-or$CI_high, 1-or$CI_low))

# -------------------------------------------------------------
# 4) Modelo de regresion logistica para reingreso
# -------------------------------------------------------------

datos$edad_std <- scale(datos$edad)
modelo_logit <- glm(reingreso_30d ~ protocolo + edad_std + comorbilidades,family = binomial, data = datos)
summary(modelo_logit)

r2_nagelkerke <- PseudoR2(modelo_logit, which = "Nagelkerke");r2_nagelkerke #El modelo explica una proporcion razonable de la variabilidad en el reingreso.


# ------------------------------------------------------------------------------
# Correcciones - Modelo de regresion logistica para reingreso
# ------------------------------------------------------------------------------
confint(modelo_logit)
cat(sprintf("R² Nagelkerke=%.3f, VeallZimmermann=%.3f, McFadden=%.3f, McFaddenAdj=%.3f, Tjur=%.3f", 
        PseudoR2(modelo_logit, which = "Nagelkerke"),
        PseudoR2(modelo_logit, which = "VeallZimmermann"),
        PseudoR2(modelo_logit, which = "McFadden"),
        PseudoR2(modelo_logit, which = "McFaddenAdj"),
        PseudoR2(modelo_logit, which = "Tjur")))
cohens_f(modelo_logit, alternative = "two.sided")
# -------------------------------------------------------------
# 5) Adherencia al tratamiento segun turno
# -------------------------------------------------------------

adherencia <- merge(adherencia, datos[, c("id", "turno")], by = "id")
adherencia$turno <- factor(adherencia$turno, levels = c("Manana", "Tarde", "Noche"))

modelo_turno <- lm(adherencia ~ turno, data = adherencia)
anova(modelo_turno)

eta_turno <- eta_squared(modelo_turno, partial = TRUE)
eta_turno # Proporcion de variabilidad explicada por el modelo
# ------------------------------------------------------------------------------
# Correcciones - Adherencia al tratamiento segun turno
# ------------------------------------------------------------------------------
table(adherencia$turno)
nrow(adherencia)

eta2 <- eta_squared(modelo_turno, partial = F, alternative="two.sided")
omega2 <- omega_squared(modelo_turno, partial = F, alternative="two.sided")
epsilon2 <- epsilon_squared(modelo_turno, partial = F, alternative="two.sided")
standardize_parameters(modelo_turno, method = "refit")

windows()
par(mfrow=c(2,2))
plot(modelo_turno)
# Se calculan los 3 tamaños de efecto de la varianza explicada por el factor turno.
# Se reporta ω² por corregir el sesgo de la varianza presente por azar en ausencia de efecto.
cat(sprintf("η² = %.3f IC 95%% [%.3f, %.3f]", eta2$Eta2, eta2$CI_low, eta2$CI_high))
cat(sprintf("ω² = %.3f IC 95%% [%.3f, %.3f]", omega2$Omega2, omega2$CI_low, omega2$CI_high))
cat(sprintf("ε² = %.3f IC 95%% [%.3f, %.3f]", epsilon2$Epsilon2, epsilon2$CI_low, epsilon2$CI_high))

# =============================================================================
# CONCLUSIONES
# =============================================================================
# 1. Los resultados preliminares en Sala A mostraron una mejora marcada en los tiempos de espera (d grande), confirmada luego en el analisis de toda la red de guardias (p < 0.001).
# 2. El nuevo protocolo redujo la tasa de reingreso a 30 dias en un 25% (odds ratio).
# 3. El modelo de regresion logistica explica una proporcion razonable de la variabilidad en el reingreso (Pseudo R2 Nagelkerke = 0.063).
# 4. El turno explica una porcion relevante de la variabilidad en la adherencia al tratamiento; se recomienda reforzar al personal del turno noche.
# 5. Se recomienda extender el nuevo protocolo a toda la red hospitalaria provincial de forma inmediata.
# =============================================================================
# CONCLUSIONES CORREGIDAS
# =============================================================================
# 1. Los resultados preliminares en Sala A mostraron una mejora marcada en los tiempos de espera (t(14) = 1.59, p = 0.135, diferencia entre grupos = -22.59 IC 95% [-53.15; 7.97] minutos, g = 0.75, IC 95% [-0.23; 1.70]), no coincide con el analisis de toda la red de guardias (t(791.70) = 4.08, p = 0.000, d = 0.27, IC 95% [0.14; 0.40] diferencia entre grupos = -7.83 IC 95% [-11.59; -4.07] minutos).
# 2. El nuevo protocolo redujo la tasa de reingreso a 30 dias en un 25% IC 95% [-0.03%, 0.46%] (odds ratio).
# 3. El modelo de regresion logistica explica una proporcion razonable de la variabilidad en el reingreso (Pseudo R² Nagelkerke=0.063, VeallZimmermann=0.078, McFadden=0.040, McFaddenAdj=0.032, Tjur=0.044).
# 4. El turno explica una porcion relevante de la variabilidad (ω² = 0.026 IC 95% 
#   [0.000, 0.109]) en la adherencia al tratamiento.
#    Si bien el análisis visual de los residuos es satisfactorio los hallazgos no implican
#    causalidad y deberia analizarse si agregar personal cambia los resultados.
# 5. Se recomienda hacer un muestreo representativo de los hospitales de la provincia
# y extender el análisis a los hospitales seleccionados.
