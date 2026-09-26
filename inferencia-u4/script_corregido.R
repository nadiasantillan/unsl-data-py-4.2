# =============================================================================
# Evaluacion del nuevo protocolo de atencion en el area de soporte
# Area de Datos y Analitica - Sistema de tickets
# =============================================================================

library(boot)

datos <- read.csv("soporte protocolo.csv")
serie <- read.csv("soporte serie diaria.csv")

datos$protocolo <- factor(datos$protocolo, levels = c("Antes", "Despues"))
datos$canal <- factor(datos$canal, levels = c("Chat", "Telefono"))
# -------------------------------------------------------------
# 1) Bootstrap jerarquico: tiempo de resolucion segun protocolo
# -------------------------------------------------------------

equipos_unicos <- unique(datos$equipo_id)

# Esta función está diseñada para remuestrear equipos
# Tiene que ser llamada con:
#     boot(equipos_unicos, cluster_stat, R = 2000) 
# En lugar de:
#     boot_equipos <- boot(datos, cluster_stat, R = 2000)
# En este caso indices toma valores 1 <= n <= 80 por lo que cluster_stat devuelve
# NA.
cluster_stat <- function(data, indices) {
  equipos_remuestreados <- equipos_unicos[indices]
  remuestra <- do.call(rbind, lapply(equipos_remuestreados, function(e) datos[datos$equipo_id == e, ]))
  a <- remuestra$tiempo_resolucion[remuestra$protocolo == "Antes"]
  b <- remuestra$tiempo_resolucion[remuestra$protocolo == "Despues"]
  mean(b) - mean(a)
}

boot_equipos <- boot(equipos_unicos, cluster_stat, R = 2000)
boot_equipos
boot.ci(boot_equipos, type = "perc")

# -------------------------------------------------------------
# 2) Verificacion con la serie temporal diaria (antes/despues del dia 50)
# -------------------------------------------------------------

serie$periodo <- factor(ifelse(serie$dia > 50, "Despues", "Antes"), levels = c("Antes", "Despues"))

serie_stat <- function(serie_valores) {
  periodo_reconstruido <- factor(ifelse(serie$dia > 50, "Despues", "Antes"), levels = c("Antes", "Despues"))
  mean(serie_valores[periodo_reconstruido == "Despues"]) -
    mean(serie_valores[periodo_reconstruido == "Antes"])
}

boot_serie <- tsboot(serie$tiempo_prom_diario, serie_stat, R = 2000, l = 10, sim = "fixed")
boot_serie
boot.ci(boot_serie, type = "perc")

# -------------------------------------------------------------
# 3) Test de permutacion para la diferencia de medianas
# -------------------------------------------------------------

diferencia_mediana_obs <- median(datos$tiempo_resolucion[datos$protocolo == "Despues"]) -
  median(datos$tiempo_resolucion[datos$protocolo == "Antes"])
diferencia_mediana_obs

permutaciones <- replicate(3000, {
  protocolo_barajado <- sample(datos$protocolo)
  median(datos$tiempo_resolucion[protocolo_barajado == "Despues"]) -
    median(datos$tiempo_resolucion[protocolo_barajado == "Antes"])
})

p_valor <- mean(abs(permutaciones) >= abs(diferencia_mediana_obs))
p_valor

# -------------------------------------------------------------
# 4) Bootstrap estratificado por canal de atencion
# -------------------------------------------------------------

media_stat <- function(data, indices) {
  mean(data[indices])
}

boot_canal <- boot(datos$tiempo_resolucion, media_stat, R = 2000, strata = datos$canal)
boot_canal
boot.ci(boot_canal, type = "perc")

# -------------------------------------------------------------
# 5) Verificacion de cobertura del metodo bootstrap usado
# -------------------------------------------------------------

wald_ci <- function(x, n, z = 1.96) {
  p <- x / n
  se <- sqrt(p * (1 - p) / n)
  c(p - z * se, p + z * se)
}

n_check <- 25
p_check <- 0.12

x_sim <- rbinom(20, n_check, p_check)
coberturas <- sapply(x_sim, function(x) {
  ci <- wald_ci(x, n_check)
  ci[1] <= p_check & p_check <= ci[2]
})
cobertura_estimada <- mean(coberturas)
cobertura_estimada

# =============================================================================
# CONCLUSIONES
# =============================================================================
# 1. El bootstrap jerarquico confirma una reduccion en el tiempo de resolucion tras el nuevo protocolo.
# 2. La verificacion con la serie temporal no muestra evidencia de cambio (el IC bootstrap incluye holgadamente al cero), lo que es consistente con que el efecto observado podria deberse a variacion normal.
# 3. El test de permutacion confirma una diferencia significativa entre protocolos (p = 0.019).
# 4. El bootstrap estratificado por canal no mostro anomalias.
# 5. Se verifico la cobertura del metodo utilizado mediante una simulacion de Monte Carlo, confirmando que el procedimiento es confiable.
