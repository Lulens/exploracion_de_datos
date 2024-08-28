# Definir el directorio raíz
root_dir <- "data"  

# Crear las rutas relativas para los archivos
archive <- file.path(root_dir, "Sleep_health_and_lifestyle_dataset.csv")
cleaned_file <- file.path(root_dir, "Sleep_health_and_lifestyle_dataset_clean.csv")
ecdf_plots_dir <- file.path(root_dir, "ECDF_Plots")
bar_plots_dir <- file.path(root_dir, "Bar_Plots")
pie_plots_dir <- file.path(root_dir, "Pie_Plots")
histogram_plots_dir <- file.path(root_dir, "Histogram_Plots")
kernel_density_plots_dir <- file.path(root_dir, "Kernel_Density_Plots")

# Crear los directorios si no existen
dir.create(ecdf_plots_dir, showWarnings = FALSE)
dir.create(bar_plots_dir, showWarnings = FALSE)
dir.create(pie_plots_dir, showWarnings = FALSE)
dir.create(histogram_plots_dir, showWarnings = FALSE)
dir.create(kernel_density_plots_dir, showWarnings = FALSE)

# Leer y limpiar los datos
data <- read.csv(archive, na.strings = c("", "NA", "NaN", "None"))
data_clean <- na.omit(data)

# Guardar el archivo limpio
write.csv(data_clean, cleaned_file, row.names = FALSE)
cat("Archivo limpio guardado con éxito.\n")

# Función para calcular métricas descriptivas para columnas numéricas
calc_metrics <- function(column) {
  list(
    Media = mean(column, na.rm = TRUE), # "promedio"
    Mediana = median(column, na.rm = TRUE), # valor que se encuentra en el medio 
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
# Frecuencia absoluta -  el número de veces que se repite algo
abs_freq_gender <- table(data_clean$Gender)
cat("\nFrecuencia absoluta de Gender:\n")
print(abs_freq_gender)

# Frecuencia relativa - el número de veces que se produce un resultado dividido por el número total de datos observados
rel_freq_gender <- abs_freq_gender / length(data_clean$Gender)
cat("\nFrecuencia relativa de Gender:\n")
print(rel_freq_gender)

# Ejemplo adicional para Sleep Disorder
# Frecuencia absoluta - el número de veces que se repite algo
abs_freq_sleep_disorder <- table(data_clean$Sleep.Disorder)
cat("\nFrecuencia absoluta de Sleep Disorder:\n")
print(abs_freq_sleep_disorder)

# Frecuencia relativa - el número de veces que se produce un resultado dividido por el número total de datos observados
rel_freq_sleep_disorder <- abs_freq_sleep_disorder / length(data_clean$Sleep.Disorder)
cat("\nFrecuencia relativa de Sleep Disorder:\n")
print(rel_freq_sleep_disorder)

# Verifica los valores únicos y el tipo de datos en la columna original
cat("\nValores únicos de Quality.of.Sleep original:\n")
unique(data$Quality.of.Sleep) # un resumen estadístico de la columna Quality.of.Sleep

cat("\nResumen de Quality.of.Sleep original:\n")
summary(data$Quality.of.Sleep) # mínimo, primer cuartil, mediana, media, tercer cuartil y máximo

cat("\nEstructura de Quality.of.Sleep original:\n")
str(data$Quality.of.Sleep) # entender cómo están almacenados los datos y para diagnosticar problemas con los datos

# Reemplaza valores inesperados por NA antes de convertir
valid_values <- c(4, 5, 6, 7, 8, 9)
# Reemplaza los valores inesperados en Quality.of.Sleep por NA, manteniendo solo los valores válidos especificados en valid_values.
data_clean$Quality.of.Sleep <- ifelse(data_clean$Quality.of.Sleep %in% valid_values, 
                                      data_clean$Quality.of.Sleep, 
                                      NA)

# Convertir Quality.of.Sleep a una variable ordinal con etiquetas para interpretar los datos de forma mas clara 
data_clean$Quality.of.Sleep <- factor(data_clean$Quality.of.Sleep, 
                                      levels = valid_values,
                                      labels = c("Poor", "Fair", "Good", "Very Good", "Excellent", NA))

# Convertir a numérico - útil para realizar análisis cuantitativos o para generar gráficos que requieren datos numéricos
data_clean$Quality.of.Sleep <- as.numeric(data_clean$Quality.of.Sleep)

# Verifica los valores únicos y el tipo de datos después de la conversión
cat("\nValores únicos de Quality.of.Sleep después de conversión:\n")
unique(data_clean$Quality.of.Sleep)

cat("\nEstructura de Quality.of.Sleep después de conversión:\n")
str(data_clean$Quality.of.Sleep)

# Verifica si hay valores NA
cat("\nNúmero de valores NA en Quality.of.Sleep:\n")
sum(is.na(data_clean$Quality.of.Sleep))

# Calcular y graficar la ECDF para Quality of Sleep
if (sum(!is.na(data_clean$Quality.of.Sleep)) > 0) {
  png(file.path(ecdf_plots_dir, "ECDF_Quality_of_Sleep.png"))
  plot(ecdf(data_clean$Quality.of.Sleep), main="ECDF de Quality of Sleep",
       xlab="Quality of Sleep", ylab="ECDF")
  dev.off()
  cat("Gráfico ECDF de Quality of Sleep guardado como imagen.\n")
} else {
  cat("Error: No hay valores válidos en Quality.of.Sleep para ECDF.\n")
}

# Calcular y graficar la ECDF para Sleep Duration
if (length(data_clean$Sleep.Duration[!is.na(data_clean$Sleep.Duration)]) > 0) {
  png(file.path(ecdf_plots_dir, "ECDF_Sleep_Duration.png"))
  plot(ecdf(data_clean$Sleep.Duration), main="ECDF de Sleep Duration",
       xlab="Sleep Duration", ylab="ECDF")
  dev.off()
  cat("Gráfico ECDF de Sleep Duration guardado como imagen.\n")
} else {
  cat("Error: No hay valores válidos en Sleep.Duration para ECDF.\n")
}

# Crear un dataframe con los intervalos y las frecuencias absolutas
grouped_data <- data.frame(
  Intervalo = c("(0, 10]", "(10, 15]", "(15, 20]", "(20, 25]", "(25, 30]", "(30, 35]", "(35, 40]", "(40, 45]", "(45, 50]", "(50, 55]"),
  Frecuencia = c(10, 3, 21, 75, 215, 373, 350, 171, 52, 6)
)

# Calcular el número total de observaciones
total_observaciones <- sum(grouped_data$Frecuencia)

# Calcular las frecuencias relativas
grouped_data$Frecuencia.Relativa <- grouped_data$Frecuencia / total_observaciones

# Calcular las frecuencias acumuladas
grouped_data$Frecuencia.Acumulada <- cumsum(grouped_data$Frecuencia.Relativa)

# Extraer los límites de los intervalos
límite_inferior <- c(0, as.numeric(gsub("\\(.*,(.*)\\]", "\\1", grouped_data$Intervalo)))
límite_superior <- as.numeric(gsub("\\((.*),.*\\]", "\\1", grouped_data$Intervalo))

# Crear un dataframe para la gráfica
ecdf_data <- data.frame(
  Límite.Superior = límite_superior,
  Frecuencia.Acumulada = grouped_data$Frecuencia.Acumulada
)

# Graficar la ECDF usando funciones base
png(file.path(ecdf_plots_dir, "ECDF_Datos_Agrupados.png"))
plot(ecdf_data$Límite.Superior, ecdf_data$Frecuencia.Acumulada, type = "s",
     main = "ECDF para Datos Agrupados",
     xlab = "Intervalos",
     ylab = "Frecuencia Acumulada")
dev.off()

# Crear gráficos de barras para variables categóricas
# Gráfico de barras de Frecuencia Relativa de Gender
png(file.path(bar_plots_dir, "Bar_Relative_Frequency_Gender.png"))
barplot(gender_freq / sum(gender_freq), 
        main = "Frecuencia Relativa de Gender",
        xlab = "Gender", 
        ylab = "Frecuencia Relativa", 
        col = "lightgreen")
dev.off()

# Gráfico de barras de Frecuencia Absoluta de Gender
png(file.path(bar_plots_dir, "Bar_Absolute_Frequency_Gender.png"))
barplot(gender_freq, 
        main = "Frecuencia Absoluta de Gender",
        xlab = "Gender", 
        ylab = "Frecuencia Absoluta", 
        col = "lightblue")
dev.off()

# Gráfico de barras de Sleep Duration
png(file.path(bar_plots_dir, "Bar_Sleep_Duration.png"))
barplot(table(data_clean$Sleep.Duration), 
        main = "Duración del Sueño",
        xlab = "Horas de Sueño", 
        ylab = "Frecuencia", 
        col = "orange")
dev.off()

# Gráfico de barras de Stress Level
png(file.path(bar_plots_dir, "Bar_Stress_Level.png"))
barplot(table(data_clean$Stress.Level), 
        main = "Nivel de Estrés",
        xlab = "Estrés", 
        ylab = "Frecuencia", 
        col = "red")
dev.off()

# Gráfico de barras de Heart Rate
png(file.path(bar_plots_dir, "Bar_Heart_Rate.png"))
barplot(table(data_clean$Heart.Rate), 
        main = "Ritmo Cardíaco",
        xlab = "Ritmo", 
        ylab = "Frecuencia", 
        col = "purple")
dev.off()

# Gráfico de barras de Daily Steps
png(file.path(bar_plots_dir, "Bar_Daily_Steps.png"))
barplot(table(data_clean$Daily.Steps), 
        main = "Cantidad de Pasos Diarios",
        xlab = "Pasos Diarios", 
        ylab = "Frecuencia", 
        col = "blue")
dev.off()


# Gráficos de torta para variables categóricas
# Frecuencia absoluta de Sleep Disorder
sleep_disorder_freq <- table(data_clean$Sleep.Disorder)
png(file.path(pie_plots_dir, "Distribucion_Sleep_Disorder.png"))
pie(sleep_disorder_freq, main = "Distribución de Sleep Disorder",
    col = rainbow(length(sleep_disorder_freq)))
dev.off()

# Pie Chart para Gender
etiquetas <- paste(names(abs_freq_gender), rel_freq_gender, "%")
png(file.path(pie_plots_dir, "Pie_Chart_Gender.png"))
pie(abs_freq_gender, labels = etiquetas, main = "Distribución de Género en el Dataset")
dev.off()

# Pie Chart para Sleep Disorder
etiqueta_sleep_disorder  <- paste(names(abs_freq_sleep_disorder), (round(rel_freq_sleep_disorder,1)), "%")
png(file.path(pie_plots_dir, "Pie_Chart_Sleep_Disorder.png"))
pie(abs_freq_sleep_disorder, labels = etiqueta_sleep_disorder, main = "Distribución de Sleep Disorder")
dev.off()


# Gráficos de histogramas para variables numéricas
# Histograma de Sleep Duration
png(file.path(histogram_plots_dir, "Histograma_Sleep_Duration.png"))
hist(data_clean$Sleep.Duration,
     main = "Histograma de la Duración del Sueño",
     xlab = "Duración del Sueño (horas)",
     ylab = "Frecuencia",
     col = "blue",
     border = "black")
dev.off()

# Histograma de Stress Level
png(file.path(histogram_plots_dir, "Histograma_Stress_Level.png"))
hist(data_clean$Stress.Level,
     main = "Histograma del Nivel de Estrés",
     xlab = "Nivel de Estrés",
     ylab = "Frecuencia",
     col = "red",
     border = "black")
dev.off()

# Histograma de Heart Rate
png(file.path(histogram_plots_dir, "Histograma_Heart_Rate.png"))
hist(data_clean$Heart.Rate,
     main = "Histograma del Ritmo Cardíaco",
     xlab = "Ritmo",
     ylab = "Frecuencia",
     col = "green",
     border = "black")
dev.off()

# Histograma de Daily Steps
png(file.path(histogram_plots_dir, "Histograma_Daily_Steps.png"))
hist(data_clean$Daily.Steps,
     main = "Histograma de Conteo de Pasos Diarios",
     xlab = "Cantidad de Pasos",
     ylab = "Frecuencia",
     col = "purple",
     border = "black")
dev.off()

# Histograma de Age
png(file.path(histogram_plots_dir, "Histograma_Age.png"))
hist(data_clean$Age, breaks = 10, col = "green", 
     main = "Histograma de Age",
     xlab = "Age", ylab = "Frecuencia")
dev.off()

# Kernel Sleep Duration
png(file.path(kernel_density_plots_dir, "Densidad_Kernel_Sleep_Duration.png"))
plot(density(data_clean$Sleep.Duration),
     main = "Densidad Kernel de la Duración del Sueño",
     xlab = "Duración del Sueño (horas)",
     ylab = "Densidad",
     col = "blue",
     lwd = 3)
dev.off()

# Kernel Stress Level
png(file.path(kernel_density_plots_dir, "Densidad_Kernel_Stress_Level.png"))
plot(density(data_clean$Stress.Level),
     main = "Densidad Kernel del Nivel de Estrés",
     xlab = "Nivel de Estrés",
     ylab = "Densidad",
     col = "red",
     lwd = 3)
dev.off()

# Kernel Ritmo Cardiaco
png(file.path(kernel_density_plots_dir, "Densidad_Kernel_Heart_Rate.png"))
plot(density(data_clean$Heart.Rate),
     main = "Densidad Kernel del Ritmo Cardíaco",
     xlab = "Ritmo",
     ylab = "Densidad",
     col = "green",
     lwd = 3)
dev.off()

# Kernel Pasos Diarios
png(file.path(kernel_density_plots_dir, "Densidad_Kernel_Daily_Steps.png"))
plot(density(data_clean$Daily.Steps),
     main = "Densidad Kernel del Conteo de Pasos Diarios",
     xlab = "Cantidad de Pasos",
     ylab = "Densidad",
     col = "purple",
     lwd = 3)
dev.off()

# Graficar densidades kernel para Age
png(file.path(kernel_density_plots_dir, "Densidad_Kernel_Age.png"))
plot(density(data_clean$Age), main = "Densidad Kernel de Age",
     xlab = "Age", ylab = "Densidad", col = "green")
dev.off()

# Mensaje de finalización
cat("\nTodas las gráficas han sido generadas y guardadas exitosamente.\n")

