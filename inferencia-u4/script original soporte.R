# =============================================================================
# Evaluacion del nuevo protocolo de atencion en el area de soporte
# Area de Datos y Analitica - Sistema de tickets
# =============================================================================
# install.packages("combinat")

library(boot)
library(effectsize)
library(dplyr)
library(combinat)
set.seed(42)
datos <- read.csv("soporte protocolo.csv")
serie <- read.csv("soporte serie diaria.csv")

datos$protocolo <- factor(datos$protocolo, levels = c("Antes", "Despues"))
# Bootstrap estratificado por canal no funciona sin esto
datos$canal <- factor(datos$canal, levels = c("Chat", "Telefono"))
# -------------------------------------------------------------
# 1) Bootstrap jerarquico: tiempo de resolucion segun protocolo
# -------------------------------------------------------------
cat("-------------------------------------------------------------\n")
cat("1) Bootstrap jerarquico: tiempo de resolucion segun protocolo\n")
cat("-------------------------------------------------------------\n")
# El resumen que sigue muestra que hay 1 único protocolo por equipo 
resumen <- datos %>%
  group_by(equipo_id) %>%
  summarise(protocolos_diferentes = n_distinct(protocolo),
            canales_diferentes = n_distinct(canal),
            protocolo = unique(protocolo))
resumen
# Esta información debe ser pasada como parametro a la función boot
# para que no asigne el protocolo equivocado. 
# Por ejemplo:
#   La asignación del protocolo Despues al equipo 1 sería un error
# Sólo 2 equipos de 8, 3 y 6, fueron asignados al protocolo "Después". Se estratifica
# por el factor protocolo para asegurar la representación de ambos niveles.
equipos_unicos <- resumen$equipo_id
estratos <- resumen$protocolo

# Esta función está diseñada para remuestrear equipos
# Tiene que ser llamada con:
#     boot(equipos_unicos, cluster_stat, R = 2000, strata=estratos) 
# En lugar de:
#     boot_equipos <- boot(datos, cluster_stat, R = 2000)
# En este caso indices toma valores 1 <= n <= 80 por lo que cluster_stat devuelve
# NA.
cluster_stat <- function(data, indices) {
  equipos_remuestreados <- equipos_unicos[indices]
  # remuestra <- do.call(rbind, lapply(equipos_remuestreados, function(e) data[data$equipo_id == e, ]))
  # data contine el vector de equipos únicos. La línea anterior fallaba debido a que data no tiene equipo_id
  remuestra <- do.call(rbind, lapply(equipos_remuestreados, function(e) datos[datos$equipo_id == e, ]))
  a <- remuestra$tiempo_resolucion[remuestra$protocolo == "Antes"]
  b <- remuestra$tiempo_resolucion[remuestra$protocolo == "Despues"]
  mean(b) - mean(a)
}

# boot_equipos <- boot(datos, cluster_>stat, R = 2000)
# La llamada anterior no considera la estructura de los equipos y se debe llamar con el vector
# de equipos (equipos_unicos) en lugar del data frame (datos).
boot_equipos <- boot(equipos_unicos, cluster_stat, R = 2000, strata = estratos)
boot_equipos_ci <- boot.ci(boot_equipos, type = "perc")
cat(sprintf("Media tiempo resolución: %.2f IC 95%% [%.2f, %.2f]\n", 
            boot_equipos$t0, 
            boot_equipos_ci$percent[4], 
            boot_equipos_ci$percent[5]))
# -------------------------------------------------------------
# 2) Verificacion con la serie temporal diaria (antes/despues del dia 50)
# -------------------------------------------------------------
cat("-------------------------------------------------------------\n")
cat("2) Verificacion con la serie temporal diaria (antes/despues del dia 50)\n")
cat("-------------------------------------------------------------\n")
# serie_stat <- function(serie_valores) {
#   periodo_reconstruido <- factor(ifelse(serie$dia > 50, "Despues", "Antes"), levels = c("Antes", "Despues"))
#   mean(serie_valores[periodo_reconstruido == "Despues"]) -
#     mean(serie_valores[periodo_reconstruido == "Antes"])
# }
# boot_serie <- tsboot(serie$tiempo_prom_diario, serie_stat, R = 2000, l = 10, sim = "fixed")

# El bloque anterior no considera la autocorrelación temporal de la serie. Cómo está implementada
# la función serie_stat mezcla valores de Antes con Después.
serie$periodo <- factor(ifelse(serie$dia > 50, "Despues", "Antes"), levels = c("Antes", "Despues"))
serie_antes <- serie[serie$periodo == "Antes", "tiempo_prom_diario"]
serie_despues <- serie[serie$periodo == "Despues", "tiempo_prom_diario"]
# Efecto observado (diferencia de medias) y tamaño de efecto (d de Cohen) - esto bootstrapearemos!
diferencia_obs <- mean(serie_despues) - mean(serie_antes);diferencia_obs 
d_obs <- cohens_d(serie_despues, serie_antes);d_obs

bloque_stat <- function(serie_valores) mean(serie_valores)
# La diferencia entre las dos distribuciones de bloques (cada una con su propia 
# autocorrelacion interna respetada) da el IC de la diferencia
boot_serie_antes <- tsboot(serie_antes, bloque_stat, R = 2000, l = 10, sim = "fixed")
boot_serie_despues <- tsboot(serie_despues, bloque_stat, R = 2000, l = 10, sim = "fixed")

diferencias_despues_antes <- boot_serie_despues$t - boot_serie_antes$t
ic_diff <- quantile(diferencias_despues_antes, c(0.025, 0.975))

cat(sprintf("Media tiempo promedio diario: %.2f IC 95%% [%.2f, %.2f]\n", 
            mean(diferencias_despues_antes), ic_diff[1], ic_diff[2]))

# Tamaño de efecto con IC vía block bootstrap: mismo arreglo que arriba, 
# bootstrapeando cada periodo por separado y calculando d en cada par de remuestras 
# (una de Antes, una de Despues)
bloque_stat_sd <- function(serie_valores) sd(serie_valores)
boot_serie_antes_sd <- tsboot(serie_antes, bloque_stat_sd, R = 2000, l = 10, sim = "fixed")
boot_serie_despues_sd <- tsboot(serie_despues, bloque_stat_sd, R = 2000, l = 10, sim = "fixed")

n_antes <- length(serie_antes)
n_despues <- length(serie_despues)
sp <- sqrt(
  ((n_antes-1)*boot_serie_antes_sd$t^2 + (n_despues-1)*boot_serie_despues_sd$t^2) / 
    (n_antes+n_despues-2)
  )
cohens_d_bloque <- diferencias_despues_antes / sp   # reutiliza las 2000 diferencias de medias ya calculadas arriba
icd_bloque <- quantile(cohens_d_bloque, c(0.025, 0.975))
cat(sprintf("d de Cohen serie temporal: %.2f IC 95%% [%.2f, %.2f]\n", 
            mean(cohens_d_bloque), icd_bloque[1], icd_bloque[2]))

# -------------------------------------------------------------
# 3) Test de permutacion para la diferencia de medianas
# -------------------------------------------------------------
cat("-------------------------------------------------------------\n")
cat("3) Test de permutacion para la diferencia de medianas\n")
cat("-------------------------------------------------------------\n")
diferencia_mediana_obs <- median(datos$tiempo_resolucion[datos$protocolo == "Despues"]) -
  median(datos$tiempo_resolucion[datos$protocolo == "Antes"])
# diferencia_mediana_obs
# 
# permutaciones <- replicate(3000, {
#   protocolo_barajado <- sample(datos$protocolo)
#   median(datos$tiempo_resolucion[protocolo_barajado == "Despues"]) -
#     median(datos$tiempo_resolucion[protocolo_barajado == "Antes"])
# })
# 
# p_valor <- mean(abs(permutaciones) >= abs(diferencia_mediana_obs))
# p_valor

# El bloque de código anterior no considera la estructura de equipo y la asignación 
# por equipos del protocolo. Al ser observaciones anidadas en grupos no se cumple el 
# supuesto de intercambiabilidad, ya que dentro de un mismo grupo no son observaciones 
# independientes. Se implementa permutaciones respetando la estructura de equipos.
permutaciones_estratos <- unique(permn(estratos))
permutaciones <- sapply(permutaciones_estratos, function(protocolos_mezclados) {
  protocolos_mezclados <- unlist(protocolos_mezclados)
  antes <- datos$tiempo_resolucion[datos$equipo_id %in% which(protocolos_mezclados=="Antes")]
  despues <- datos$tiempo_resolucion[datos$equipo_id %in% which(protocolos_mezclados=="Despues")]
  median(despues) - median(antes) 
})

p_valor <- mean(abs(permutaciones) >= abs(diferencia_mediana_obs))
cat(sprintf("Mediana observada: %.2f (p-valor: %.3f)\n", diferencia_mediana_obs, p_valor))

# ---- IC de la diferencia de medianas por inversion del test
# Prueba diferencias Despues - Antes entre -15 y 15
grilla_delta <- seq(-15, 20, by = 0.25)
datos_antes <- datos[datos$protocolo == "Antes", "tiempo_resolucion"]

# p_valor_para_delta <- sapply(grilla_delta, function(delta) {
#   despues_ajustado <- datos$tiempo_resolucion[datos$protocolo == "Despues"]-delta
#   dif_ajustada_obs <- median(despues_ajustado) - median(datos_antes)
#   
#   perm_ajustada <- sapply(permutaciones_estratos, function(protocolos_mezclados) {
#     protocolos_mezclados <- unlist(protocolos_mezclados)
#     print(which(protocolos_mezclados=="Antes"))
#     print(which(protocolos_mezclados=="Despues"))
#     antes <- datos$tiempo_resolucion[datos$equipo_id %in% which(protocolos_mezclados=="Antes")]
#     despues <- datos$tiempo_resolucion[datos$equipo_id %in% which(protocolos_mezclados=="Despues")]-delta
#     median(despues) - median(antes) 
#   })
#   mean(abs(perm_ajustada) >= abs(dif_ajustada_obs))
# })

p_valor_para_delta <- sapply(grilla_delta, function(delta) {
  # ajusto los datos una sola vez, antes de permutar
  y_adj <- datos$tiempo_resolucion - delta * (datos$protocolo == "Despues")
  
  dif_obs <- median(y_adj[datos$protocolo == "Despues"]) -
    median(y_adj[datos$protocolo == "Antes"])
  
  dif_perm <- sapply(permutaciones_estratos, function(p) {
    p <- unlist(p)
    median(y_adj[p == "Despues"]) - median(y_adj[p == "Antes"])
  })
  
  mean(abs(dif_perm) >= abs(dif_obs))
})

# el IC son los valores de la grilla donde el test NO se rechaza (p > 0.05)
ic_permutacional <- range(grilla_delta[p_valor_para_delta > 0.05])
cat(sprintf("Diferencia mediana tiempo de atención (Después - Antes): %.2f IC 95%% [%.2f, %.2f]\n", 
            diferencia_mediana_obs, ic_permutacional[1], ic_permutacional[2]))
# -------------------------------------------------------------
# 4) Bootstrap estratificado por canal de atencion
# -------------------------------------------------------------
cat("-------------------------------------------------------------\n")
cat("4) Bootstrap estratificado por canal de atencion\n")
cat("-------------------------------------------------------------\n")

media_stat <- function(data, indices) {
  mean(data[indices])
}
# Hay que convertir canal en factor, de lo contrario bootstrap ignora el 
# parámetro strata
boot_canal <- boot(datos$tiempo_resolucion, media_stat, R = 2000, strata = datos$canal)
boot_canal_ci <- boot.ci(boot_canal, type = "perc")

cat(sprintf("Media tiempo de atención: %.2f IC 95%% [%.2f, %.2f]\n", 
            boot_canal$t0, boot_canal_ci$percent[4], boot_canal_ci$percent[5]))

# -------------------------------------------------------------
# 5) Verificacion de cobertura del metodo bootstrap usado
# -------------------------------------------------------------
# No tiene conexión con el resto
# wald_ci <- function(x, n, z = 1.96) {
#   p <- x / n
#   se <- sqrt(p * (1 - p) / n)
#   c(p - z * se, p + z * se)
# }
# 
# n_check <- 25
# p_check <- 0.12
# 
# x_sim <- rbinom(20, n_check, p_check)
# coberturas <- sapply(x_sim, function(x) {
#   ci <- wald_ci(x, n_check)
#   ci[1] <= p_check & p_check <= ci[2]
# })
# cobertura_estimada <- mean(coberturas)
# cobertura_estimada
# =============================================================================
# CONCLUSIONES
# =============================================================================
# 1. El bootstrap jerarquico confirma una reduccion en el tiempo de resolucion tras el nuevo protocolo.
# -- -2.00 IC 95% [-10.45, 6.76] No permite afirmar 1, el IC indica que el tiempo de resolución de protocolo nuevo puede ser casi 7 minutos mayor al anterior
# 2. La verificacion con la serie temporal no muestra evidencia de cambio (el IC bootstrap incluye holgadamente al cero), lo que es consistente con que el efecto observado podria deberse a variacion normal.
# -- -2.94 IC 95% [-5.42, -0.31] El análisis de la serie de tiempo indica que a partir del día 50 el promedio de tiempo diario baja entre .3 y 5.4 minutos
# 3. El test de permutacion no confirma una diferencia significativa entre protocolos (p = 0.143).
# -- No es correcto afirmar que la diferencia es significativa basandonos en el p-valor.
# -- La mediana de la diferencia de antención es de -5.50 IC 95% [-15, 15]. El IC hace evidente que no hay una reducción en las medianas
# 4. El bootstrap estratificado por canal no mostro anomalias.
# -- Cómo estaba planteado se ignoraban los canales de atención ya que no estaban representados como niveles de un factor.
# 5. Se verifico la cobertura del metodo utilizado mediante una simulacion de Monte Carlo, confirmando que el procedimiento es confiable.
# -- No aplica
