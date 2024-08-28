# Cargar las librerías necesarias
library(ggplot2)

# Definir rutas relativas
archive <- "data/Sleep_health_and_lifestyle_dataset.csv"
saved <- "data/Sleep_health_and_lifestyle_dataset_clean.csv"

# Leer y limpiar los datos
data <- read.csv(archive, na.strings = c("", "NA", "NaN", "None"))
data_clean <- na.omit(data)

# Guardar el archivo limpio
write.csv(data_clean, saved, row.names = FALSE)
cat("Archivo limpio guardado con éxito.\n")

# Función para calcular métricas descriptivas para columnas numéricas
calc_metrics <- function(column) {
  list(
    Media = mean(column, na.rm = TRUE),
    Mediana = median(column, na.rm = TRUE),
    Minimo = min(column, na.rm = TRUE),
    Maximo = max(column, na.rm = TRUE),
    Rango = range(column, na.rm = TRUE)
  )
}

# Aplicar la función a cada columna numérica
metrics_Age <- calc_metrics(data_clean$Age)
metrics_Sleep_Duration <- calc_metrics(data_clean$Sleep.Duration)
metrics_Stress_Level <- calc_metrics(data_clean$Stress.Level)
metrics_Heart_Rate <- calc_metrics(data_clean$Heart.Rate)
metrics_Daily_Steps <- calc_metrics(data_clean$Daily.Steps)

# Imprimir las métricas
cat("\nMétricas de Age:\n")
print(metrics_Age)

cat("\nMétricas de Sleep Duration:\n")
print(metrics_Sleep_Duration)

cat("\nMétricas de Stress Level:\n")
print(metrics_Stress_Level)

cat("\nMétricas de Heart Rate:\n")
print(metrics_Heart_Rate)

cat("\nMétricas de Daily Steps:\n")
print(metrics_Daily_Steps)

# Calcular frecuencias absolutas y relativas para una columna categórica, por ejemplo, Gender
# Frecuencia absoluta
abs_freq_gender <- table(data_clean$Gender)
cat("\nFrecuencia absoluta de Gender:\n")
print(abs_freq_gender)

# Frecuencia relativa
rel_freq_gender <- abs_freq_gender / length(data_clean$Gender)
cat("\nFrecuencia relativa de Gender:\n")
print(rel_freq_gender)

# Ejemplo adicional para Sleep Disorder
# Frecuencia absoluta
abs_freq_sleep_disorder <- table(data_clean$Sleep.Disorder)
cat("\nFrecuencia absoluta de Sleep Disorder:\n")
print(abs_freq_sleep_disorder)

# Frecuencia relativa
rel_freq_sleep_disorder <- abs_freq_sleep_disorder / length(data_clean$Sleep.Disorder)
cat("\nFrecuencia relativa de Sleep Disorder:\n")
print(rel_freq_sleep_disorder)

# Verifica los valores únicos y el tipo de datos en la columna original
cat("\nValores únicos de Quality.of.Sleep original:\n")
unique(data$Quality.of.Sleep)

cat("\nResumen de Quality.of.Sleep original:\n")
summary(data$Quality.of.Sleep)

cat("\nEstructura de Quality.of.Sleep original:\n")
str(data$Quality.of.Sleep)

# Reemplaza valores inesperados por NA antes de convertir
valid_values <- c(4, 5, 6, 7, 8, 9)
data_clean$Quality.of.Sleep <- ifelse(data_clean$Quality.of.Sleep %in% valid_values, 
                                      data_clean$Quality.of.Sleep, 
                                      NA)

# Convertir Quality.of.Sleep a una variable ordinal con etiquetas
data_clean$Quality.of.Sleep <- factor(data_clean$Quality.of.Sleep, 
                                      levels = valid_values,
                                      labels = c("Poor", "Fair", "Good", "Very Good", "Excellent", NA))

# Convertir a numérico
data_clean$Quality.of.Sleep <- as.numeric(data_clean$Quality.of.Sleep)

# Verifica los valores únicos y el tipo de datos después de la conversión
cat("\nValores únicos de Quality.of.Sleep después de conversión:\n")
unique(data_clean$Quality.of.Sleep)

cat("\nEstructura de Quality.of.Sleep después de conversión
