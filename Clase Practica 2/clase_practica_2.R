datos <- read.table("ENNyS_menorA2.txt", header = TRUE, sep = "")


datos_Sexo <- as.factor(datos$Sexo)

# Calcular las frecuencias relativas
tabla_frecuencia_sexo <- prop.table(table(datos_Sexo))

# Imprimir la tabla de frecuencias
print("Tabla de Frecuencias Relativas para Sexo:")
print(tabla_frecuencia_sexo)

# 2. Tabla de frecuencias relativas para la variable Tipo de embarazo
# Asegurarse de que Tipo de embarazo sea un factor
datos_Tipo_embarazo <- as.factor(datos$Tipo_embarazo)

# Calcular las frecuencias relativas
tabla_frecuencia_embarazo <- prop.table(table(datos_Tipo_embarazo))

# Imprimir la tabla de frecuencias
print("Tabla de Frecuencias Relativas para Tipo de embarazo:")
print(tabla_frecuencia_embarazo)


# Calcular las frecuencias relativas del sexo
frecuencia_sexo <- prop.table(table(datos$Sexo))

# Crear el gráfico de barras con la función base de R
barplot(tabla_frecuencia_sexo,
        main = "Distribución por Sexo",
        xlab = "Sexo del Bebé",
        ylab = "Frecuencia Relativa",
        col = c("turquoise2", "olivedrab4"),
        names.arg = c("Femenino", "Masculino"))

pie(tabla_frecuencia_sexo,
    main = "Distribución por Sexo",
    col = c("turquoise2", "olivedrab4"),
    labels = paste(names(tabla_frecuencia_sexo)))


# Calcular la tabla de contingencia
tabla_contingencia <- table(datos_Sexo, datos_Tipo_embarazo)
print("Tabla de Contingencia (Sexo vs. Tipo de embarazo):")
print(tabla_contingencia)

# Opcional: Calcular la tabla de contingencia con porcentajes
# Por fila
tabla_porcentual_fila <- prop.table(tabla_contingencia, margin = 1)
print("Tabla de Contingencia (porcentajes por fila):")
print(tabla_porcentual_fila)

# Por columna
tabla_porcentual_columna <- prop.table(tabla_contingencia, margin = 2)
print("Tabla de Contingencia (porcentajes por columna):")
print(tabla_porcentual_columna)

barplot(tabla_contingencia,
        beside = TRUE,
        legend.text = TRUE,
        col = c("turquoise2", "olivedrab4"),
        main = "Distribución de Sexo por Tipo de Embarazo",
        xlab = "Tipo de Embarazo",
        ylab = "Frecuencia",
        names.arg = c("Múltiple", "Simple"))


# Cargar los datos (si no lo has hecho ya)
# datos <- read.table("ENNyS menorA2.txt", header = TRUE, sep = "\t")


# Crear el histograma para Edad
hist(datos$Edad,
     main = "Distribución de Edad de los Bebés",
     xlab = "Edad (años)",
     ylab = "Frecuencia",
     col = "rosybrown",
     border = "white",
     breaks = 20) # Define el número de barras


# Crear el histograma para Peso
hist(datos$Peso,
     main = "Distribución de Peso de los Bebés",
     xlab = "Peso (kg)",
     ylab = "Frecuencia",
     col = "lightblue",
     border = "white",
     breaks = 20) 


# Crear el histograma para Perímetro Cefálico
hist(datos$Perim_encef,
     main = "Distribución de Perímetro Cefálico",
     xlab = "Perímetro Cefálico (cm)",
     ylab = "Frecuencia",
     col = "lightgreen",
     border = "white",
     breaks = 20)


# Crear el histograma para Talla
hist(datos$Talla,
     main = "Distribución de Talla",
     xlab = "Talla (cm)",
     ylab = "Frecuencia",
     col = "palevioletred2",
     border = "white",
     breaks = 20)


# Crear el boxplot para Edad
boxplot(Edad ~ Sexo,
        data = datos,
        main = "Boxplot de Edad",
        xlab = "Sexo",
        ylab = "Edad (años)",
        col = c("turquoise2", "olivedrab4"))

# Crear el boxplot para Peso
boxplot(Peso ~ Sexo,
        data = datos,
        main = "Boxplot de Peso",
        xlab = "Sexo",
        ylab = "Peso (kg)",
        col = c("turquoise2", "olivedrab4"))

# Crear el boxplot para Perímetro Cefálico
boxplot(Perim_encef ~ Sexo,
        data = datos,
        main = "Boxplot de Perímetro Cefálico",
        xlab = "Sexo",
        ylab = "Perímetro Cefálico (cm)",
        col = c("turquoise2", "olivedrab4"))

# Crear el boxplot para Talla
boxplot(Talla ~ Sexo,
        data = datos,
        main = "Boxplot de Talla",
        xlab = "Sexo",
        ylab = "Talla (cm)",
        col = c("turquoise2", "olivedrab4"))




# Nombres de las variables continuas
variables_continuas <- c("Peso", "Perim_encef", "Talla")

# Bucle para cada variable
for (variable in variables_continuas) {
  
  # Extraer la columna de datos
  x <- datos[[variable]]
  
  # Calcular la media muestral
  media <- mean(x, na.rm = TRUE)
  
  # Calcular la mediana muestral
  mediana <- median(x, na.rm = TRUE)
  
  # Calcular la media α-podada para α = 0.1
  media_podada_0.1 <- mean(x, trim = 0.1, na.rm = TRUE)
  
  # Calcular la media α-podada para α = 0.2
  media_podada_0.2 <- mean(x, trim = 0.2, na.rm = TRUE)
  
  # Imprimir los resultados
  cat("--- Medidas de Posición para la variable:", variable, "---\n")
  cat("Media Muestral: ", round(media, 2), "\n")
  cat("Mediana Muestral: ", round(mediana, 2), "\n")
  cat("Media Podada (α=0.1): ", round(media_podada_0.1, 2), "\n")
  cat("Media Podada (α=0.2): ", round(media_podada_0.2, 2), "\n")
  cat("\n")
}

# Se usa cat() porque en R print() no puede hacer concatenaciones



# Nombres de las variables continuas
variables_continuas <- c("Peso", "Perim_encef", "Talla")

# Bucle para cada variable
for (variable in variables_continuas) {
  
  # Extraer la columna de datos
  x <- datos[[variable]]
  
  # Calcular el desvío estándar muestral
  desvio_estandar <- sd(x, na.rm = TRUE)
  
  # Calcular la distancia intercuartil (IQR)
  iqr <- IQR(x, na.rm = TRUE)
  
  # Calcular la MAD (Mediana de las Desviaciones Absolutas)
  mad <- mad(x, na.rm = TRUE)
  
  # Imprimir los resultados
  cat("--- Medidas de Dispersión para la variable:", variable, "---\n")
  cat("Desvío Estándar: ", round(desvio_estandar, 2), "\n")
  cat("Distancia Intercuartil (IQR): ", round(iqr, 2), "\n")
  cat("MAD Muestral: ", round(mad, 2), "\n")
  cat("\n")
}


# Definir los percentiles que queremos calcular
percentiles_deseados <- c(0.1, 0.25, 0.5, 0.75, 0.9)

# Bucle para cada variable
for (variable in variables_continuas) {
  
  # Extraer la columna de datos
  x <- datos[[variable]]
  
  # Calcular los percentiles
  percentiles <- quantile(x, probs = percentiles_deseados, na.rm = TRUE)
  
  # Imprimir los resultados
  cat("--- Percentiles para la variable:", variable, "---\n")
  print(percentiles)
  cat("\n")
}

#1. Gráfico Q-Q Normal para Peso
qqnorm(datos$Peso, main = "Q-Q Plot Normal para Peso", col = "seashell3")
qqline(datos$Peso, col = "sienna1", lwd = 2)

# 2. Gráfico Q-Q Normal para Perímetro Cefálico
qqnorm(datos$Perim_encef, main = "Q-Q Plot Normal para Perímetro Cefálico", col = "seashell3")
qqline(datos$Perim_encef, col = "sienna1", lwd = 2)

# 3. Gráfico Q-Q Normal para Talla
qqnorm(datos$Talla, main = "Q-Q Plot Normal para Talla", col = "seashell3")
qqline(datos$Talla, col = "sienna1", lwd = 2)















