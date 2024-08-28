# Cargar las librerías necesarias
library(ggplot2)

# Leer y limpiar los datos
archive <- "C:/Users/Lucia Baez/Documents/UCA/Exploracion de Datos/Sleep Health/Sleep_health_and_lifestyle_dataset.csv"
data <- read.csv(archive, na.strings = c("", "NA", "NaN", "None"))
data_clean <- na.omit(data)

# Guardar el archivo limpio
saved <- "C:/Users/Lucia Baez/Documents/UCA/Exploracion de Datos/Sleep Health/Sleep_health_and_lifestyle_dataset_clean.csv"
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

cat("\nEstructura de Quality.of.Sleep después de conversión:\n")
str(data_clean$Quality.of.Sleep)

# Verifica si hay valores NA
cat("\nNúmero de valores NA en Quality.of.Sleep:\n")
sum(is.na(data_clean$Quality.of.Sleep))

# Calcular y graficar la ECDF para Quality of Sleep
if (sum(!is.na(data_clean$Quality.of.Sleep)) > 0) {
  png("C:/Users/Lucia Baez/Documents/UCA/Exploracion de Datos/Sleep Health/ECDF_Quality_of_Sleep.png")
  plot.ecdf(data_clean$Quality.of.Sleep, main="ECDF de Quality of Sleep",
            xlab="Quality of Sleep", ylab="ECDF")
  dev.off()
  cat("Gráfico ECDF de Quality of Sleep guardado como imagen.\n")
} else {
  cat("Error: No hay valores válidos en Quality.of.Sleep para ECDF.\n")
}

# Calcular y graficar la ECDF para Sleep Duration
if (length(data_clean$Sleep.Duration[!is.na(data_clean$Sleep.Duration)]) > 0) {
  png("C:/Users/Lucia Baez/Documents/UCA/Exploracion de Datos/Sleep Health/ECDF_Sleep_Duration.png")
  plot.ecdf(data_clean$Sleep.Duration, main="ECDF de Sleep Duration",
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

# Graficar la ECDF usando ggplot2
ecdf_plot <- ggplot(ecdf_data, aes(x = Límite.Superior, y = Frecuencia.Acumulada)) +
  geom_step() +
  labs(title = "ECDF para Datos Agrupados",
       x = "Intervalos",
       y = "Frecuencia Acumulada") +
  theme_minimal()
ggsave("C:/Users/Lucia Baez/Documents/UCA/Exploracion de Datos/Sleep Health/ECDF_Datos_Agrupados.png", plot = ecdf_plot)

# Crear gráficos de barras para variables categóricas
# Frecuencia absoluta y relativa de Gender
gender_freq <- table(data_clean$Gender)
gender_freq_df <- as.data.frame(gender_freq)

# Graficar frecuencia absoluta de Gender
gender_bar_plot <- ggplot(gender_freq_df, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  labs(title = "Frecuencia Absoluta de Gender",
       x = "Gender",
       y = "Frecuencia Absoluta") +
  theme_minimal()
ggsave("C:/Users/Lucia Baez/Documents/UCA/Exploracion de Datos/Sleep Health/Frecuencia_Absoluta_Gender.png", plot = gender_bar_plot)

# Graficar frecuencia relativa de Gender
gender_bar_plot_rel <- ggplot(gender_freq_df, aes(x = Var1, y = Freq / sum(Freq))) +
  geom_bar(stat = "identity") +
  labs(title = "Frecuencia Relativa de Gender",
       x = "Gender",
       y = "Frecuencia Relativa") +
  theme_minimal()
ggsave("C:/Users/Lucia Baez/Documents/UCA/Exploracion de Datos/Sleep Health/Frecuencia_Relativa_Gender.png", plot = gender_bar_plot_rel)

# Frecuencia absoluta y relativa de Sleep Disorder
sleep_disorder_freq <- table(data_clean$Sleep.Disorder)
sleep_disorder_freq_df <- as.data.frame(sleep_disorder_freq)

# Graficar frecuencia absoluta de Sleep Disorder
sleep_disorder_bar_plot <- ggplot(sleep_disorder_freq_df, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity") +
  labs(title = "Frecuencia Absoluta de Sleep Disorder",
       x = "Sleep Disorder",
       y = "Frecuencia Absoluta") +
  theme_minimal()
ggsave("C:/Users/Lucia Baez/Documents/UCA/Exploracion de Datos/Sleep Health/Frecuencia_Absoluta_Sleep_Disorder.png", plot = sleep_disorder_bar_plot)

# Graficar frecuencia relativa de Sleep Disorder
sleep_disorder_bar_plot_rel <- ggplot(sleep_disorder_freq_df, aes(x = Var1, y = Freq / sum(Freq))) +
  geom_bar(stat = "identity") +
  labs(title = "Frecuencia Relativa de Sleep Disorder",
       x = "Sleep Disorder",
       y = "Frecuencia Relativa") +
  theme_minimal()
ggsave("C:/Users/Lucia Baez/Documents/UCA/Exploracion de Datos/Sleep Health/Frecuencia_Relativa_Sleep_Disorder.png", plot = sleep_disorder_bar_plot_rel)

# Pie Chart para Gender
gender_freq <- table(data_clean$Gender)
gender_freq_df <- as.data.frame(gender_freq)
colnames(gender_freq_df) <- c("Gender", "Frequency")
gender_freq_df$Percentage <- (gender_freq_df$Frequency / sum(gender_freq_df$Frequency)) * 100

# Pie chart for Gender
gender_pie_plot <- ggplot(gender_freq_df, aes(x = "", y = Percentage, fill = Gender)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Distribución de Gender") +
  theme_void()
ggsave("C:/Users/Lucia Baez/Documents/UCA/Exploracion de Datos/Sleep Health/Pie_Chart_Gender.png", plot = gender_pie_plot)

# Pie Chart para Sleep Disorder
sleep_disorder_freq <- table(data_clean$Sleep.Disorder)
sleep_disorder_freq_df <- as.data.frame(sleep_disorder_freq)
colnames(sleep_disorder_freq_df) <- c("Sleep_Disorder", "Frequency")
sleep_disorder_freq_df$Percentage <- (sleep_disorder_freq_df$Frequency / sum(sleep_disorder_freq_df$Frequency)) * 100

# Pie chart for Sleep Disorder
sleep_disorder_pie_plot <- ggplot(sleep_disorder_freq_df, aes(x = "", y = Percentage, fill = Sleep_Disorder)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Distribución de Sleep Disorder") +
  theme_void()
ggsave("C:/Users/Lucia Baez/Documents/UCA/Exploracion de Datos/Sleep Health/Pie_Chart_Sleep_Disorder.png", plot = sleep_disorder_pie_plot)


# Histograma de Sleep Duration
# Graficar el histograma con ggplot2
histogram_sleep_duration <- ggplot(data_clean, aes(x = Sleep.Duration)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Histograma de Sleep Duration",
       x = "Sleep Duration (horas)",
       y = "Frecuencia") +
  theme_minimal()
ggsave("C:/Users/Lucia Baez/Documents/UCA/Exploracion de Datos/Sleep Health/Histograma_Sleep_Duration.png", plot = histogram_sleep_duration)

histogram_stress_level <- ggplot(data_clean, aes(x = Stress.Level)) +
  geom_histogram(bins = 30, fill = "red", color = "black") +
  labs(title = "Histograma de Stress Level",
       x = "Stress Level",
       y = "Frecuencia") +
  theme_minimal()
ggsave("C:/Users/Lucia Baez/Documents/UCA/Exploracion de Datos/Sleep Health/Histograma_Stress_Level.png", plot = histogram_stress_level)

histogram_heart_rate <- ggplot(data_clean, aes(x = Heart.Rate)) +
  geom_histogram(bins = 30, fill = "green", color = "black") +
  labs(title = "Histograma de Heart Rate",
       x = "Heart Rate (lpm)",
       y = "Frecuencia") +
  theme_minimal()
ggsave("C:/Users/Lucia Baez/Documents/UCA/Exploracion de Datos/Sleep Health/Histograma_Heart_Rate.png", plot = histogram_heart_rate)

histogram_daily_steps <- ggplot(data_clean, aes(x = Daily.Steps)) +
  geom_histogram(bins = 30, fill = "purple", color = "black") +
  labs(title = "Histograma de Daily Steps",
       x = "Daily Steps",
       y = "Frecuencia") +
  theme_minimal()
ggsave("C:/Users/Lucia Baez/Documents/UCA/Exploracion de Datos/Sleep Health/Histograma_Daily_Steps.png", plot = histogram_daily_steps)


# Crear y guardar gráficos de densidad kernel
# Kernel Density Plot para Sleep Duration
kernel_density_sleep_duration <- ggplot(data_clean, aes(x = Sleep.Duration)) +
  geom_density(fill = "blue", color = "black") +
  labs(title = "Kernel Density Plot de Sleep Duration",
       x = "Sleep Duration (horas)",
       y = "Densidad") +
  theme_minimal()
ggsave("C:/Users/Lucia Baez/Documents/UCA/Exploracion de Datos/Sleep Health/Kernel_Density_Sleep_Duration.png", plot = kernel_density_sleep_duration)

# Kernel Density Plot para Stress Level
kernel_density_stress_level <- ggplot(data_clean, aes(x = Stress.Level)) +
  geom_density(fill = "red", color = "black") +
  labs(title = "Kernel Density Plot de Stress Level",
       x = "Stress Level",
       y = "Densidad") +
  theme_minimal()
ggsave("C:/Users/Lucia Baez/Documents/UCA/Exploracion de Datos/Sleep Health/Kernel_Density_Stress_Level.png", plot = kernel_density_stress_level)

# Kernel Density Plot para Heart Rate
kernel_density_heart_rate <- ggplot(data_clean, aes(x = Heart.Rate)) +
  geom_density(fill = "green", color = "black") +
  labs(title = "Kernel Density Plot de Heart Rate",
       x = "Heart Rate (lpm)",
       y = "Densidad") +
  theme_minimal()
ggsave("C:/Users/Lucia Baez/Documents/UCA/Exploracion de Datos/Sleep Health/Kernel_Density_Heart_Rate.png", plot = kernel_density_heart_rate)

# Kernel Density Plot para Daily Steps
kernel_density_daily_steps <- ggplot(data_clean, aes(x = Daily.Steps)) +
  geom_density(fill = "purple", color = "black") +
  labs(title = "Kernel Density Plot de Daily Steps",
       x = "Daily Steps",
       y = "Densidad") +
  theme_minimal()
ggsave("C:/Users/Lucia Baez/Documents/UCA/Exploracion de Datos/Sleep Health/Kernel_Density_Daily_Steps.png", plot = kernel_density_daily_steps)

