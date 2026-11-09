# =====================================================================
# Análisis de SOL_ACT con modelo mixto Tweedie (glmmTMB)
# Respuesta:        SOL_ACT
# Efectos fijos:    Treatment, StudyPeriod (y su interacción)
# Efecto aleatorio: ParticipantID
# Inferencia:       intervalos de confianza (sin p-valores)
# =====================================================================

# ---- 0. Paquetes ----------------------------------------------------
# install.packages(c("readxl", "dplyr", "ggplot2", "glmmTMB", "DHARMa", "emmeans"))
library(readxl)
library(dplyr)
library(ggplot2)
library(glmmTMB)
library(DHARMa)
library(emmeans)

# ---- 1. Importar y preparar los datos -------------------------------
ruta <- "C:/Users/Usuario/Documents/UNSL/Cuarto Año/Segundo cuatrimestre/Optativa II - Inferencia estadistica para la toma de decisiones/trabajo final/pmed.1002587.s005.xlsx"

dat <- as.data.frame(read_excel(ruta, sheet = 3))   # sheet va ADENTRO de read_excel


dat$Treatment     <- factor(dat$Treatment)      # 0, 1, 2
dat$StudyPeriod   <- factor(dat$StudyPeriod)    # factor, salvo que sea numérica a propósito
dat$ParticipantID <- factor(dat$ParticipantID)

str(dat)
colSums(is.na(dat))                                   # datos faltantes

# ---- 2. Descriptivos ------------------------------------------------
summary(dat$SOL_ACT)
mean(dat$SOL_ACT == 0, na.rm = TRUE)                  # proporción de ceros

desc <- dat %>%
  group_by(Treatment, StudyPeriod) %>%
  summarise(n      = n(),
            media  = mean(SOL_ACT, na.rm = TRUE),
            desvio = sd(SOL_ACT, na.rm = TRUE),
            ceros  = mean(SOL_ACT == 0, na.rm = TRUE),
            .groups = "drop")
desc

windows()
ggplot(dat, aes(SOL_ACT)) +
  geom_histogram(bins = 40) +
  facet_grid(Treatment ~ StudyPeriod) +
  theme_minimal() +
  labs(x = "SOL_ACT", y = "Frecuencia")

# ---- 3. Ajuste de modelos -------------------------------------------
# m0: sin efecto aleatorio
m0 <- glmmTMB(SOL_ACT ~ Treatment + StudyPeriod,
              family = tweedie(link = "log"), data = dat)

# m1: con intercepto aleatorio por participante
m1 <- update(m0, . ~ . + (1 | ParticipantID))

# m2: con interacción Treatment x StudyPeriod
m2 <- update(m1, . ~ Treatment * StudyPeriod + (1 | ParticipantID))

# ---- 4. Selección de modelo (AIC / BIC) -----------------------------
AIC(m0, m1, m2)
BIC(m0, m1, m2)
# Se elige el modelo con menor AIC/BIC. Una diferencia de AIC > 10
# se considera evidencia fuerte a favor del modelo con menor valor.

# ---- 5. Diagnóstico del modelo elegido (m2) -------------------------
res2 <- simulateResiduals(m2)
windows()
plot(res2)                       # QQ de residuos y residuos vs. predichos
testDispersion(res2)             # chequeos del modelo, no resultados del estudio
testZeroInflation(res2)          # si da exceso de ceros: ziformula = ~1
testOutliers(res2)

# Autocorrelación temporal (solo si existe una variable de día/fecha):
# testTemporalAutocorrelation(recalculateResiduals(res2, group = dat$Dia),
#                             time = unique(dat$Dia))

# ---- 6. Resultados: coeficientes con IC 95% -------------------------
# Escala log
ic_log <- confint(m2)
ic_log

# Escala original: razón de medias (exp de los coeficientes)
# Columnas: límite inferior, límite superior, estimado
ic_razon <- exp(ic_log[grep("^cond", rownames(ic_log)), ])
ic_razon
# Interpretación: un IC de razón que NO incluye el 1 indica diferencia;
# si lo incluye, los datos son compatibles con ausencia de diferencia.

# ---- 7. Medias estimadas e IC 95% -----------------------------------
em <- emmeans(m2, ~ Treatment | StudyPeriod, type = "response")
em

# Razón de medias entre tratamientos dentro de cada período (con IC)
confint(pairs(em))

# Cambio Baseline -> Treatment dentro de cada grupo (con IC)
em_per <- emmeans(m2, ~ StudyPeriod | Treatment, type = "response")
confint(pairs(em_per))

# Interacción como razón de razones (con IC)
em_int <- emmeans(m2, ~ Treatment * StudyPeriod)
confint(contrast(em_int, interaction = "pairwise"), type = "response")

# ---- 8. Gráfico de medias estimadas con IC 95% ----------------------
em_df <- as.data.frame(em)
windows()
ggplot(em_df, aes(x = StudyPeriod, y = response,
                  colour = Treatment, group = Treatment)) +
  geom_point(position = position_dodge(width = 0.3), size = 3) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL),
                width = 0.15, position = position_dodge(width = 0.3)) +
  geom_line(position = position_dodge(width = 0.3)) +
  theme_minimal() +
  labs(x = "Período de estudio", y = "SOL_ACT (media estimada, IC 95%)",
       colour = "Tratamiento")

# ---- 9. Información del modelo para reportar ------------------------
nobs(m2)                         # observaciones usadas
nlevels(dat$ParticipantID)       # participantes
VarCorr(m2)                      # variabilidad entre participantes
sigma(m2)                        # parámetro de dispersión