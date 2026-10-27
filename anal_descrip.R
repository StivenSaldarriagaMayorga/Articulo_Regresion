# =============================================================================
#  Análisis descriptivo - Medidas dasométricas de 137 parcelas forestales
# =============================================================================


# 0. Paquetes -----------------------------------------------------------------

paquetes  <- c("readxl", "ggplot2", "tidyr", "GGally")
faltantes <- paquetes[!paquetes %in% rownames(installed.packages())]
if (length(faltantes) > 0) install.packages(faltantes)

library(readxl)
library(ggplot2)
library(tidyr)
library(GGally)

theme_set(theme_minimal(base_size = 12))


# 1. Lectura de los datos desde el repositorio --------------------------------

url_datos <- paste0("https://raw.githubusercontent.com/",
                    "StivenSaldarriagaMayorga/Articulo_Regresion/main/datos.xlsx")

archivo_temporal <- tempfile(fileext = ".xlsx")
download.file(url_datos, archivo_temporal, mode = "wb", quiet = TRUE)
datos <- as.data.frame(read_excel(archivo_temporal, sheet = 1))


# 2. Preparación ---------------------------------------------------------------

names(datos) <- c("parcela", "x", "y", "area_basal",
                  "dap", "altura", "volumen", "biomasa")

etiquetas <- c(area_basal = "Área basal (m²)",
               dap        = "DAP (cm)",
               altura     = "Altura (m)",
               volumen    = "Volumen (m³)",
               biomasa    = "Biomasa (ton/ha)")
variables <- names(etiquetas)

datos_largo <- pivot_longer(datos, cols = all_of(variables),
                            names_to = "variable", values_to = "valor")
datos_largo$variable <- factor(datos_largo$variable,
                               levels = variables, labels = etiquetas)


# 3. Calidad de los datos -------------------------------------------------------

cat("\n==== 3. CALIDAD DE LOS DATOS ====\n")
cat("Parcelas (filas):", nrow(datos), "| Variables (columnas):", ncol(datos), "\n")
cat("Datos faltantes por variable:\n")
print(colSums(is.na(datos)))
cat("Parcelas repetidas:", sum(duplicated(datos$parcela)), "\n")
cat("Numeración de parcelas: de", min(datos$parcela), "a", max(datos$parcela),
    "->", max(datos$parcela) - nrow(datos), "números sin parcela en la base\n")


# 4. Resumen numérico -----------------------------------------------------------

asimetria <- function(v) mean((v - mean(v))^3) / mean((v - mean(v))^2)^1.5
curtosis  <- function(v) mean((v - mean(v))^4) / mean((v - mean(v))^2)^2 - 3

resumir <- function(v) {
  c(n         = length(v),
    media     = mean(v),
    mediana   = median(v),
    sd        = sd(v),
    cv        = 100 * sd(v) / mean(v),
    minimo    = min(v),
    q1        = unname(quantile(v, 0.25)),
    q3        = unname(quantile(v, 0.75)),
    maximo    = max(v),
    asimetria = asimetria(v),
    curtosis  = curtosis(v))
}

resumen <- t(sapply(datos[variables], resumir))
cat("\n==== 4. RESUMEN NUMÉRICO ====\n")
print(round(resumen, 2))


# 5. Forma de las distribuciones ----------------------------------------------

# 5.1 Histogramas
grafico_histogramas <- ggplot(datos_largo, aes(x = valor)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20,
                 fill = "#7FA7C9", colour = "white") +
  geom_density(colour = "#1F4E79", linewidth = 0.8) +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  labs(title = "Distribución de cada variable",
       subtitle = "Histograma y curva de densidad",
       x = NULL, y = "Densidad")
print(grafico_histogramas)

# 5.2 Diagramas de caja
grafico_cajas <- ggplot(datos_largo, aes(x = "", y = valor)) +
  geom_boxplot(fill = "#7FA7C9", outlier.colour = "#C0392B", width = 0.5) +
  facet_wrap(~ variable, scales = "free", ncol = 5) +
  labs(title = "Diagramas de caja",
       subtitle = "Los puntos rojos son atípicos según la regla de 1.5 × RIC",
       x = NULL, y = NULL)
print(grafico_cajas)

# 5.3 Gráficos Q-Q
grafico_qq <- ggplot(datos_largo, aes(sample = valor)) +
  stat_qq(colour = "#1F4E79", size = 1.5) +
  stat_qq_line(colour = "#C0392B") +
  facet_wrap(~ variable, scales = "free", ncol = 3) +
  labs(title = "Gráficos Q-Q contra la distribución normal",
       x = "Cuantiles teóricos", y = "Cuantiles observados")
print(grafico_qq)


# 6. Valores atípicos -----------------------------------------------------------

# 6.1 Regla de Tukey (1.5 × RIC)
es_atipico <- function(v) {
  ric <- IQR(v)
  v < quantile(v, 0.25) - 1.5 * ric | v > quantile(v, 0.75) + 1.5 * ric
}

marcas_atipicos <- sapply(datos[variables], es_atipico)
cat("\n==== 6. VALORES ATÍPICOS (regla 1.5 × RIC) ====\n")
cat("Cantidad de atípicos por variable:\n")
print(colSums(marcas_atipicos))

cat("\nParcelas atípicas en al menos una variable:\n")
print(datos[rowSums(marcas_atipicos) > 0, c("parcela", variables)], row.names = FALSE)

# 6.2 Coeficiente de esbeltez
datos$esbeltez <- round(datos$altura / (datos$dap / 100), 1)

cat("\nParcelas con mayor coeficiente de esbeltez:\n")
print(head(datos[order(-datos$esbeltez), c("parcela", "dap", "altura", "esbeltez")], 5),
      row.names = FALSE)

parcela_sospechosa <- datos$parcela[which.max(datos$esbeltez)]
cat("\n-> La parcela ", parcela_sospechosa, " tiene esbeltez de ",
    round(max(datos$esbeltez)), " (la mediana es ", round(median(datos$esbeltez)),
    "): su altura es imposible para su diámetro,\n",
    "   muy probablemente un error de digitación.\n", sep = "")


# 7. Relaciones entre variables -------------------------------------------------

# 7.1 Matriz de dispersión
grafico_pares <- ggpairs(datos[variables],
                         columnLabels = unname(etiquetas),
                         lower = list(continuous = wrap("points", alpha = 0.6,
                                                        colour = "#1F4E79")),
                         diag  = list(continuous = wrap("densityDiag",
                                                        fill = "#7FA7C9")),
                         progress = FALSE) +
  labs(title = "Relaciones entre las variables dasométricas")
print(grafico_pares)

# 7.2 Correlaciones de Pearson y Spearman
cor_pearson  <- cor(datos[variables], method = "pearson")
cor_spearman <- cor(datos[variables], method = "spearman")

cat("\n==== 7. CORRELACIONES ====\n")
cat("Pearson:\n");  print(round(cor_pearson, 2))
cat("\nSpearman:\n"); print(round(cor_spearman, 2))

# 7.3 Correlaciones de la altura con y sin la parcela sospechosa
sin_sospechosa <- datos[datos$parcela != parcela_sospechosa, variables]
comparacion_altura <- data.frame(
  con_parcela = cor_pearson["altura", ],
  sin_parcela = cor(sin_sospechosa)["altura", ]
)
cat("\nCorrelación de Pearson de la altura con y sin la parcela",
    parcela_sospechosa, ":\n")
print(round(comparacion_altura, 2))

# 7.4 Biomasa y volumen frente a DAP y altura
modelo_biomasa <- lm(log(biomasa) ~ log(dap), data = datos)
modelo_volumen <- lm(log(volumen) ~ log(dap) + log(altura),
                     data = datos[datos$parcela != parcela_sospechosa, ])

a <- exp(coef(modelo_biomasa)[1])
b <- coef(modelo_biomasa)[2]
cat("\nR² de log(biomasa) ~ log(DAP):", round(summary(modelo_biomasa)$r.squared, 5), "\n")
cat("   -> biomasa =", round(a, 4), "× DAP ^", round(b, 3), "\n")
cat("R² de log(volumen) ~ log(DAP) + log(altura):",
    round(summary(modelo_volumen)$r.squared, 5), "\n")

grafico_biomasa <- ggplot(datos, aes(x = dap, y = biomasa)) +
  geom_point(colour = "#1F4E79", size = 2) +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Biomasa frente a DAP (ambos ejes en escala logarítmica)",
       subtitle = "Los puntos forman una recta perfecta: la biomasa se calculó a partir del DAP",
       x = etiquetas["dap"], y = etiquetas["biomasa"])
print(grafico_biomasa)

grafico_altura_dap <- ggplot(datos, aes(x = dap, y = altura)) +
  geom_point(colour = "#1F4E79", size = 2) +
  geom_point(data = datos[datos$parcela == parcela_sospechosa, ],
             colour = "#C0392B", size = 3) +
  annotate("text", x = datos$dap[datos$parcela == parcela_sospechosa],
           y = max(datos$altura), hjust = -0.15,
           label = paste("Parcela", parcela_sospechosa), colour = "#C0392B") +
  labs(title = "Altura frente a DAP",
       subtitle = "Un solo punto arrastra la escala y oculta la relación del resto",
       x = etiquetas["dap"], y = etiquetas["altura"])
print(grafico_altura_dap)
