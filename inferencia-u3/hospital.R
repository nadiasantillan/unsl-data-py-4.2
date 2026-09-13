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
cat(sprintf("t(%d) = %.2f, p = %.3f, d = %.2f, IC 95%% [%.2f; %.2f] diferencia entre grupos = %.2f IC 95%% [%.2f; %.2f] minutos, g = %.2f, IC 95%% [%.2f; %.2f]",
        t_res$parameter, t_res$statistic, t_res$p.value, d$Cohens_d, d$CI_low, 
        d$CI_high, diff(t_res$estimate), -t_res$conf.int[2], -t_res$conf.int[1], 
        g$Hedges_g, g$CI_low, g$CI_high))
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
cat(sprintf("t(%.2f) = %.2f, p = %.3f, d = %.2f, IC 95%% [%.2f; %.2f] diferencia entre grupos = %.2f IC 95%% [%.2f; %.2f] minutos, g = %.2f, IC 95%% [%.2f; %.2f]",
            t_result$parameter, t_result$statistic, t_result$p.value, 
            d_todas_salas$Cohens_d, d_todas_salas$CI_low, d_todas_salas$CI_high, 
            diff(t_result$estimate), -t_result$conf.int[2], -t_result$conf.int[1],
            g_todas_salas$Hedges_g, g_todas_salas$CI_low, 
            g_todas_salas$CI_high))

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

eta2 <- eta_squared(modelo_turno, alternative="two.sided")
omega_squared(modelo_turno, alternative="two.sided")
epsilon_squared(modelo_turno, alternative="two.sided")
standardize_parameters(modelo_turno, method = "refit")
standardize_parameters(modelo_turno, method = "smart")

cat(sprintf("η² = %.3f IC 95%% [%.3f, %.3f]", eta2$Eta2, eta2$CI_low, eta2$CI_high))
# =============================================================================
# CONCLUSIONES
# =============================================================================
# 1. Los resultados preliminares en Sala A mostraron una mejora marcada en los tiempos de espera (d grande), confirmada luego en el analisis de toda la red de guardias (p < 0.001).
# 2. El nuevo protocolo redujo la tasa de reingreso a 30 dias en un 25% (odds ratio).
# 3. El modelo de regresion logistica explica una proporcion razonable de la variabilidad en el reingreso (Pseudo R2 Nagelkerke = 0.063).
# 4. El turno explica una porcion relevante de la variabilidad en la adherencia al tratamiento; se recomienda reforzar al personal del turno noche.
# 5. Se recomienda extender el nuevo protocolo a toda la red hospitalaria provincial de forma inmediata.
