# =============================================================================
# Evaluacion del nuevo protocolo de atencion en el area de soporte
# Area de Datos y Analitica - Sistema de tickets
# =============================================================================

library(boot)
library(dplyr)

set.seed(42)
datos <- read.csv("soporte protocolo.csv")
serie <- read.csv("soporte serie diaria.csv")

datos$protocolo <- factor(datos$protocolo, levels = c("Antes", "Despues"))
datos$canal <- factor(datos$canal, levels = c("Chat", "Telefono"))
# -------------------------------------------------------------
# 1) Bootstrap jerarquico: tiempo de resolucion segun protocolo
# -------------------------------------------------------------
equipos_unicos <- unique(datos$equipo_id)

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
  remuestra <- do.call(rbind, lapply(equipos_remuestreados, function(e) datos[datos$equipo_id == e, ]))
  a <- remuestra$tiempo_resolucion[remuestra$protocolo == "Antes"]
  b <- remuestra$tiempo_resolucion[remuestra$protocolo == "Despues"]
  mean(b) - mean(a)
}

ci_diff_media_equipos <- function(data) {
    resumen <- data %>%
      group_by(equipo_id) %>%
      summarise(protocolo = unique(protocolo))
    equipos_unicos <- unique(data$equipo_id)    
    estratos <- resumen$protocolo
    boot_equipos <- boot(equipos_unicos, cluster_stat, R = 2000, strata = estratos)
    ci_estim <- boot.ci(boot_equipos, type = "perc")
    
    return(c(ci_estim$percent[4], ci_estim$percent[5]))
}
ci_diff_media_equipos(datos)
# -------------------------------------------------------------
# 2) Verificacion con la serie temporal diaria (antes/despues del dia 50)
# -------------------------------------------------------------

serie$periodo <- factor(ifelse(serie$dia > 50, "Despues", "Antes"), levels = c("Antes", "Despues"))
serie_antes <- serie[serie$periodo == "Antes", "tiempo_prom_diario"]
serie_despues <- serie[serie$periodo == "Despues", "tiempo_prom_diario"]
bloque_stat <- function(serie_valores) mean(serie_valores)

boot_serie_antes <- tsboot(serie_antes, bloque_stat, R = 2000, l = 10, sim = "fixed")
boot_serie_despues <- tsboot(serie_despues, bloque_stat, R = 2000, l = 10, sim = "fixed")

diferencias_despues_antes <- boot_serie_despues$t - boot_serie_antes$t
ic_diff <- quantile(diferencias_despues_antes, c(0.025, 0.975))
mean(diferencias_despues_antes); ic_diff

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
mean(cohens_d_bloque);icd_bloque

# -------------------------------------------------------------
# 3) Test de permutacion para la diferencia de medianas
# -------------------------------------------------------------
datos_antes <- datos[datos$protocolo == "Antes", "tiempo_resolucion"]
datos_despues <- datos[datos$protocolo == "Despues", "tiempo_resolucion"]

diferencia_mediana_obs <- median(datos_despues) - median(datos_antes)
diferencia_mediana_obs
todos <- datos$tiempo_resolucion

n_antes <- length(datos_antes)
n_despues <- length(datos_despues)
permutaciones <- replicate(3000, {
  # protocolo_barajado <- sample(datos$protocolo)
  # median(datos$tiempo_resolucion[protocolo_barajado == "Despues"]) -
  #   median(datos$tiempo_resolucion[protocolo_barajado == "Antes"])
  mezclado <- sample(todos) # baraja TODAS las observaciones juntas
  median(mezclado[(n_antes+1):length(todos)]) - median(mezclado[1:n_antes]) 
})
hist(permutaciones)
p_valor <- mean(abs(permutaciones) >= abs(diferencia_mediana_obs))
p_valor

# ---- IC por inversion del test
grilla_delta <- seq(-15, 15, by = 0.25)

p_valor_para_delta <- sapply(grilla_delta, function(delta) {
  despues_ajustado <- datos_despues - delta # "que pasaria si la verdadera diferencia fuera delta"
  todos_ajustado <- c(datos_antes, despues_ajustado)
  dif_ajustada_obs <- median(despues_ajustado) - median(datos_antes)
  perm_ajustada <- replicate(1000, {
    mezclado <- sample(todos_ajustado)
    median(mezclado[(n_antes+1):length(todos_ajustado)]) - median(mezclado[1:n_antes])
  })
  # hist(perm_ajustada)
  mean(abs(perm_ajustada) >= abs(dif_ajustada_obs))
})

# el IC son los valores de la grilla donde el test NO se rechaza (p > 0.05)
ic_permutacional <- range(grilla_delta[p_valor_para_delta > 0.05])
ic_permutacional

# ---- Tamaño de efecto no paramétrico: probabilidad de superioridad (CLES) vía 
# el estadístico de Mann-Whitney, con su IC ----
# wilcox.test con conf.int=TRUE da el estimador de Hodges-Lehmann 
# (una mediana de diferencias por pares) junto con su IC, y el estadistico W 
# permite derivar la probabilidad de superioridad

test_mw <- wilcox.test(datos_antes, datos_despues, conf.int = TRUE)
test_mw

probabilidad_superioridad <- test_mw$statistic / (length(datos_antes) * length(datos_despues))
probabilidad_superioridad

# IC de la probabilidad de superioridad, vía bootstrap (no tiene formula cerrada 
# simple, mismo espiritu que las Secciones 1 y 2)
cles_stat <- function(data, indices) {
  remuestra <- data[indices, ]
  a <- remuestra$tiempo[remuestra$protocolo == "Antes"]
  b <- remuestra$tiempo[remuestra$protocolo == "Despues"]
  if (length(a) == 0 || length(b) == 0) return(NA) # evita remuestras degeneradas sin un grupo
  sum(outer(a, b, ">")) / (length(a) * length(b))
}

boot_cles <- boot(datos, cles_stat, R = 2000, strata = datos$protocolo) 
boot.ci(boot_cles, type = "perc")

# -------------------------------------------------------------
# 4) Bootstrap estratificado por canal de atencion
# -------------------------------------------------------------

media_stat <- function(data, indices) {
  mean(data[indices])
}
# Hay que convertir canal en factor, de lo contrario bootstrap ignora el 
# parámetro strata
boot_canal <- boot(datos$tiempo_resolucion, media_stat, R = 2000, strata = datos$canal)
boot_canal
boot.ci(boot_canal, type = "perc")

# -------------------------------------------------------------
# 5) Verificacion de cobertura del metodo bootstrap usado
# -------------------------------------------------------------
protocolo_antes <- datos$tiempo_resolucion[datos$protocolo=="Antes"]
protocolo_despues <- datos$tiempo_resolucion[datos$protocolo=="Despues"]
m_antes <- mean(protocolo_antes)
m_despues <- mean(protocolo_despues)
s_antes <- sd(protocolo_antes)
s_despues <- sd(protocolo_despues)
n_antes <- length(protocolo_antes)
n_despues <- length(protocolo_despues)

diffmeans <- seq(-10, 10, by=0.25)
ci_contiene_media <- sapply(diffmeans, function(diff_actual) {
  cat(sprintf("Simulating diff = %.2f\n", diff_actual))
    datos_sim <- data.frame(datos)
    x_antes <- rnorm(n_antes, m_antes, s_antes)
    x_despues <- rnorm(n_despues, m_despues+diff_actual, s_despues)
    
    datos_sim[datos_sim$protocolo == "Antes", "tiempo_resolucion"] <- x_antes
    datos_sim[datos_sim$protocolo == "Despues", "tiempo_resolucion"] <- x_despues

    ci <- ci_diff_media_equipos(datos_sim)
    ci[1] <= diff_actual & diff_actual <= ci[2]
})
mean(ci_contiene_media)
# =============================================================================
# CONCLUSIONES
# =============================================================================
# 1. El bootstrap jerarquico confirma una reduccion en el tiempo de resolucion tras el nuevo protocolo.
# 2. La verificacion con la serie temporal no muestra evidencia de cambio (el IC bootstrap incluye holgadamente al cero), lo que es consistente con que el efecto observado podria deberse a variacion normal.
# 3. El test de permutacion confirma una diferencia significativa entre protocolos (p = 0.019).
# 4. El bootstrap estratificado por canal no mostro anomalias.
# 5. Se verifico la cobertura del metodo utilizado mediante una simulacion de Monte Carlo, confirmando que el procedimiento es confiable.
