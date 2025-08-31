
###################
### Ejercicio 1 ###
###################

# Leer los datos
datos <- read.csv("Debernardi.csv", stringsAsFactors = FALSE)

# a) Tabla de frecuencias absolutas y relativas
tabla_diag <- table(datos$diagnosis)
tabla_diag_rel <- prop.table(tabla_diag)

# Mostrar ambas tablas
print(tabla_diag)       # Frecuencia absoluta
print(tabla_diag_rel)   # Frecuencia relativa

# b) Gráfico de barras con base R
barplot(tabla_diag_rel,
        main = "Frecuencia de los diagnósticos",
        xlab = "Diagnóstico",
        ylab = "Frecuencia",
        col = c("lightblue", "violetred1", "springgreen2"))


###################
### Ejercicio 2 ###
###################

# Leer los datos
titanic <- read.csv("datos_titanic.csv", stringsAsFactors = FALSE)

# a) 

# Probabilidad de ser mujer dado que sobrevivió
mujeres_sobrevivientes <- sum(titanic$Sex == "female" & titanic$Survived == 1)
total_sobrevivientes   <- sum(titanic$Survived == 1)

prob_mujer_dado_sobrevivio <- mujeres_sobrevivientes / total_sobrevivientes

# Probabilidad de ser mujer en toda la muestra
total_mujeres <- sum(titanic$Sex == "female")
total_pasajeros <- nrow(titanic)

prob_mujer <- total_mujeres / total_pasajeros

# Mostrar resultados
cat("P(Mujer | Sobrevivió) =", round(prob_mujer_dado_sobrevivio, 3), "\n")
cat("P(Mujer) =", round(prob_mujer, 3), "\n")


# b)

# Tabla de contingencia entre Survived y Pclass
tabla_SP <- table(titanic$Survived, titanic$Pclass)
print(tabla_SP)

# Probabilidad de sobrevivir dado cada valor de Pclass
# (dividimos los sobrevivientes entre el total de esa clase)
prob_sobrevivir_por_clase <- prop.table(tabla_SP, margin = 2)  # normaliza por columna

print(prob_sobrevivir_por_clase)

# Extraer solo la fila de "Sobrevivió = 1"
prob_sobrevivir_dado_clase <- prob_sobrevivir_por_clase["1", ]
print(prob_sobrevivir_dado_clase)

# c)

# Gráfico de barras apiladas
barplot(tabla_SP,
        main = "Supervivencia según clase",
        xlab = "Clase",
        ylab = "Cantidad de pasajeros",
        col = c("seashell2", "skyblue3"),
        legend = rownames(tabla_SP))

# Gráfico de barras lado a lado
barplot(tabla_SP,
        beside = TRUE,
        main = "Supervivencia según clase",
        xlab = "Clase",
        ylab = "Cantidad de pasajeros",
        col = c("seashell2", "skyblue3"),
        legend = rownames(tabla_SP))



###################
### Ejercicio 3 ###
###################

# Leer los datos
iridio <- read.table("iridio.txt", header = TRUE)
rodio  <- read.table("rodio.txt", header = TRUE)

# Extraer los vectores numéricos
val_iridio <- iridio$iridio
val_rodio  <- rodio$rodio

# a)

# Histogramas
par(mfrow = c(1, 2))
hist(val_iridio, main = "Histograma - Iridio", xlab = "Temperatura", col = "lightblue", border = "black")
hist(val_rodio,  main = "Histograma - Rodio",  xlab = "Temperatura", col = "lightgreen", border = "black")

# Boxplots en paralelo
boxplot(val_iridio,
        names = "Iridio",
        main = "Boxplot Iridio",
        col = "lightblue")

boxplot(val_rodio,
        names = "Rodio",
        main = "Boxplot Rodio",
        col = "lightgreen")

# b)

# Medias y medianas
media_iridio   <- mean(val_iridio)
media_rodio    <- mean(val_rodio)
mediana_iridio <- median(val_iridio)
mediana_rodio  <- median(val_rodio)

# Medias podadas
media_pod10_iridio <- mean(val_iridio, trim = 0.10)
media_pod10_rodio  <- mean(val_rodio,  trim = 0.10)
media_pod20_iridio <- mean(val_iridio, trim = 0.20)
media_pod20_rodio  <- mean(val_rodio,  trim = 0.20)

resumen_central <- data.frame(
  Estadístico = c("Media", "Mediana", "Media podada 10%", "Media podada 20%"),
  Iridio = c(media_iridio, mediana_iridio, media_pod10_iridio, media_pod20_iridio),
  Rodio  = c(media_rodio,  mediana_rodio,  media_pod10_rodio,  media_pod20_rodio)
)
print(resumen_central)


# c)

# Desvío estándar, IQR y MAD
desvio_iridio <- sd(val_iridio)
desvio_rodio  <- sd(val_rodio)

iqr_iridio <- IQR(val_iridio)
iqr_rodio  <- IQR(val_rodio)

mad_iridio <- mad(val_iridio)
mad_rodio  <- mad(val_rodio)

resumen_dispersion <- data.frame(
  Medida = c("Desvío estándar", "IQR", "MAD"),
  Iridio = c(desvio_iridio, iqr_iridio, mad_iridio),
  Rodio  = c(desvio_rodio,  iqr_rodio,  mad_rodio)
)
print(resumen_dispersion)

# d)

# Cuantiles
probs <- c(0.90, 0.75, 0.50, 0.25, 0.10)

cuantiles_iridio <- quantile(val_iridio, probs)
cuantiles_rodio  <- quantile(val_rodio,  probs)

resumen_cuantiles <- data.frame(
  Probabilidad = probs,
  Iridio = cuantiles_iridio,
  Rodio  = cuantiles_rodio
)
print(resumen_cuantiles)


###################
### Ejercicio 4 ###
###################


# a)

# Leer cada archivo
A <- read.table("salchichas_A.txt", header = TRUE)
B <- read.table("salchichas_B.txt", header = TRUE)
C <- read.table("salchichas_C.txt", header = TRUE)

# Renombrar columnas para que sean iguales
colnames(A) <- c("CALORIAS", "SODIO")
colnames(B) <- c("CALORIAS", "SODIO")
colnames(C) <- c("CALORIAS", "SODIO")

# Agregar columna de tipo
A$TIPO <- "A"
B$TIPO <- "B"
C$TIPO <- "C"

# Unir todos en un solo data frame
salchichas <- rbind(A, B, C)

# Guardar en archivo
write.table(salchichas, "salchichas.txt", row.names = FALSE)


# b)

# Histogramas separados
par(mfrow = c(1, 3))  # 3 gráficos lado a lado

hist(salchichas$CALORIAS[salchichas$TIPO == "A"],
     main = "Calorías - Tipo A", xlab = "Calorías", col = "lightblue")

hist(salchichas$CALORIAS[salchichas$TIPO == "B"],
     main = "Calorías - Tipo B", xlab = "Calorías", col = "lightgreen")

hist(salchichas$CALORIAS[salchichas$TIPO == "C"],
     main = "Calorías - Tipo C", xlab = "Calorías", col = "lightpink")

par(mfrow = c(1, 1)) # reset layout


# c)

boxplot(CALORIAS ~ TIPO, data = salchichas,
        main = "Boxplots de Calorías",
        xlab = "Tipo de salchicha", ylab = "Calorías",
        col = c("lightblue", "lightgreen", "lightpink"))


# d)

boxplot(SODIO ~ TIPO, data = salchichas,
        main = "Boxplots de Sodio",
        xlab = "Tipo de salchicha", ylab = "Sodio",
        col = c("lightblue", "lightgreen", "lightpink"))


###################
### Ejercicio 5 ###
###################

# Leer los datos
datos <- read.table("estudiantes.txt", header = TRUE)

# a)

# Separar los grupos
grupo1 <- datos$GRUPO1
grupo2 <- datos$GRUPO2

# Histograma con curva normal para Grupo 1
hist(grupo1, breaks=10, probability=TRUE, col="lightblue",
     main="Histograma Grupo 1", xlab="Concentración de NO3 (µg/l)")
curve(dnorm(x, mean=mean(grupo1), sd=sd(grupo1)), add=TRUE, col="red", lwd=2)

# Histograma con curva normal para Grupo 2
hist(grupo2, breaks=10, probability=TRUE, col="lightgreen",
     main="Histograma Grupo 2", xlab="Concentración de NO3 (µg/l)")
curve(dnorm(x, mean=mean(grupo2), sd=sd(grupo2)), add=TRUE, col="red", lwd=2)

# QQplots
qqnorm(grupo1, main="QQPlot Grupo 1", col="blue")
qqline(grupo1, col="red", lwd=2)

qqnorm(grupo2, main="QQPlot Grupo 2", col="green")
qqline(grupo2, col="red", lwd=2)

# b)

# Medidas de tendencia central y dispersión
summary(grupo1)
summary(grupo2)
sd(grupo1)
sd(grupo2)

# Boxplots paralelos
boxplot(grupo1, grupo2, names=c("Grupo 1","Grupo 2"),
        main="Boxplots de concentración de NO3", col=c("lightblue","lightgreen"),
        ylab="Concentración de NO3 (µg/l)")


###################
### Ejercicio 6 ###
###################

# Cargar datos
# Leer los datos
datos <- read.table("nubes.txt", header = TRUE)

# a)

# Separar los grupos
controles  <- datos$CONTROLES
tratadas <- datos$TRATADAS


boxplot(controles, tratadas,
        names = c("Controles", "Tratadas"),
        main = "Boxplots paralelos: Nubes control vs tratadas",
        ylab = "Agua caída (mm)",
        col = c("lightblue", "lightgreen"))

# b)

par(mfrow = c(1, 2))


hist(controles, breaks=10, probability=TRUE, col="lightblue",
     main="Histograma Controles", xlab="Agua Caida (L)")
lines(density(controles), col="blue", lwd=2)
curve(dnorm(x, mean=mean(controles), sd=sd(controles)), add=TRUE, col="red", lwd=2)


hist(tratadas, breaks=10, probability=TRUE, col="lightgreen",
     main="Histograma Tratadas", xlab="Agua Caida (L)")
lines(density(controles), col="blue", lwd=2)
curve(dnorm(x, mean=mean(tratadas), sd=sd(tratadas)), add=TRUE, col="red", lwd=2)

par(mfrow = c(1, 1))


# QQplots
par(mfrow = c(1, 2))
qqnorm(controles, main="QQplot Controles"); qqline(controles, col="red")
qqnorm(tratadas, main="QQplot Tratadas"); qqline(tratadas, col="red")


# c)

log_controles <- log(controles)
log_tratadas <- log(tratadas)

# Histogramas con log

par(mfrow = c(1, 2))


hist(log_controles, breaks=10, probability=TRUE, col="lightblue",
     main="Histograma Log Controles", xlab="Agua Caida (L)")
lines(density(log_controles), col="blue", lwd=2)
curve(dnorm(x, mean=mean(log_controles), sd=sd(log_controles)), add=TRUE, col="red", lwd=2)


hist(log_tratadas, breaks=10, probability=TRUE, col="lightgreen",
     main="Histograma Log Tratadas", xlab="Agua Caida (L)")
lines(density(log_tratadas), col="blue", lwd=2)
curve(dnorm(x, mean=mean(log_tratadas), sd=sd(log_tratadas)), add=TRUE, col="red", lwd=2)

par(mfrow = c(1, 1))


# QQplots con log
qqnorm(log_controles, main="QQplot Log(Controles)"); qqline(log_controles, col="red")
qqnorm(log_tratadas, main="QQplot Log(Tratadas)"); qqline(log_tratadas, col="red")


# d)


boxplot(log_controles, log_tratadas,
        names = c("Log(Controles)", "Log(Tratadas)"),
        main = "Boxplots paralelos con log",
        ylab = "log(Agua caída)",
        col = c("lightblue", "lightgreen"))


###################
### Ejercicio 7 ###
###################


## Lectura de datos

df <- read.csv("data_credit_card.csv", stringsAsFactors = FALSE)



vars <- c("purchases","credit_limit","purchases_freq","tenure")

# a)

par(mfrow = c(2,2), mar=c(4,4,4,4))

for (v in vars) {
  plot(ecdf(df[[v]]),
       main = paste("ECDF -", v),
       xlab = v, ylab = "F_n(x)")
  grid()
}


# b)

par(mfrow = c(1,2))

hist(df$credit_limit, probability = TRUE, breaks = "FD",
     col = "lightblue", border = "black",
     main = "Histograma credit_limit", xlab = "credit_limit")
lines(density(df$credit_limit, na.rm = TRUE), lwd = 2, col = "blue")
grid()

plot(density(df$credit_limit, na.rm = TRUE),
     main = "Densidad kernel - credit_limit", xlab = "credit_limit", lwd = 2, type = "l")
rug(df$credit_limit)

# c)

par(mfrow = c(1,1))
prop_tenure <- prop.table(table(df$tenure))
barplot(prop_tenure, main = "Tenure - Frecuencias relativas",
        ylab = "Proporción", xlab = "Meses", col = "lightblue", border = "black")

# d)

medidas_pos <- function(x) {
  c(
    mean      = mean(x, na.rm = TRUE),
    median    = median(x, na.rm = TRUE),
    mean_0.1 = mean(x, trim = 0.10, na.rm = TRUE)
  )
}

pos_tbl <- t(sapply(df[vars], medidas_pos))
round(pos_tbl, 3)


# e)

dispersion <- function(x) {
  q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
  c(
    q25 = q[1],
    q75 = q[2],
    IQR = IQR(x, na.rm = TRUE),
    MAD = mad(x, constant = 1, na.rm = TRUE)  
  )
}

disp_tbl <- t(sapply(df[vars], dispersion))
round(disp_tbl, 3)

par(mfrow = c(2,2))

colors <- c("lightblue", "lightgreen", "lightpink", "lightgoldenrod")
i <- 1

for (v in vars) {
  boxplot(df[[v]], main = paste("Boxplot -", v), col = colors[i], horizontal = TRUE)
  grid()
  i <- i + 1
}

# f)

sd_values <- c()

for (i in 1:4) {
  colname <- vars[i]                     
  sd_values <- c(sd_values, round(sd(df[[colname]], na.rm = TRUE), 3))
}


resumen_sd <- data.frame(
  purchases = c(sd_values[1]),
  credit_limit  = c(sd_values[2]),
  purchases_freq  = c(sd_values[3]),
  tenure  = c(sd_values[4])
)

print(resumen_sd)


# g)

# Detección de atípicos

outlier_idx <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  which(x < (q1 - 1.5*iqr) | x > (q3 + 1.5*iqr))
}

# Índices y conteos de atípicos por variable
out_list <- lapply(df[vars], outlier_idx)

sapply(out_list, length)


# Eliminar outliers

sin_atipicos <- lapply(seq_along(vars), 
  function(i){
    v <- vars[i]
    x <- df[[v]]
    x[-out_list[[i]]]
})

names(sin_atipicos) <- vars

pos_tbl_no <- t(sapply(sin_atipicos, medidas_pos))
disp_tbl_no <- t(sapply(sin_atipicos, dispersion))

sd_values <- c()

for (i in 1:4) {
  colname <- vars[i]                     
  sd_values <- c(sd_values, round(sd(sin_atipicos[[colname]], na.rm = TRUE), 3))
}


resumen_sd <- data.frame(
  purchases = c(sd_values[1]),
  credit_limit  = c(sd_values[2]),
  purchases_freq  = c(sd_values[3]),
  tenure  = c(sd_values[4])
)

print(resumen_sd)


###################
### Ejercicio 8 ###
###################

# Paquete para leer Excel
library(readxl)

# Leer datos
datos <- read_excel("ciclocombinado.xlsx")

# a)

hist(datos$PE, breaks = 10, freq = FALSE, 
     col = "lightblue", main = "Histograma y densidad de PE",
     xlab = "Potencia entregada (MW)")
lines(density(datos$PE), col = "red", lwd = 2)


# b)

PE0 <- datos$PE[datos$HighTemp == 0]
PE1 <- datos$PE[datos$HighTemp == 1]

plot(density(PE0), col = "blue", lwd = 2, 
     main = "Densidad de PE según temperatura",
     xlab = "Potencia entregada (MW)", ylim = c(0, max(density(PE0)$y, density(PE1)$y)))
lines(density(PE1), col = "red", lwd = 2)
legend("topright", legend = c("HighTemp = 0 (<=20°C)", "HighTemp = 1 (>20°C)"),
       col = c("blue", "red"), lwd = 2)


# c)

p1 <- mean(PE0 < 450)   # P(PE < 450 | HighTemp = 0)
p2 <- mean(PE1 < 300)   # P(PE < 300 | HighTemp = 1)

cat("P(PE < 450 | HighTemp = 0) =", p1, "\n")
cat("P(PE < 300 | HighTemp = 1) =", p2, "\n")


# d)

p3 <- mean(datos$PE < 450)
cat("P(PE < 450) =", p3, "\n")

# e)

q1 <- quantile(PE1, probs = 0.1)
cat("Potencia mínima garantizada (90%) con HighTemp = 1:", q1, "\n")

# f)

q2 <- quantile(datos$PE, probs = 0.1)
cat("Potencia mínima garantizada (90%) en general:", q2, "\n")


###################
### Ejercicio 9 ###
###################


datos <- read.csv("datos.csv", sep = ",", dec = ".")

# Asegurar que diagnosis y sex sean factores
datos$diagnosis <- factor(datos$diagnosis, levels = c(1, 2, 3),
                          labels = c("Diag1", "Diag2", "Diag3"))
datos$sex <- factor(datos$sex, levels = c("M","F"))


# a)

par(mfrow = c(1,3))   # 3 histogramas en una fila
for(d in levels(datos$diagnosis)){
  hist(datos$LYVE1[datos$diagnosis == d], 
       main = paste("LYVE1 -", d),
       xlab = "LYVE1", col = "lightblue", border = "black")
}
par(mfrow = c(1,1))


# b)

plot(ecdf(datos$LYVE1[datos$diagnosis=="Diag1"]), 
     col="blue", main="ECDF de LYVE1 por diagnosis",
     xlab="LYVE1", ylab="F(x)")
lines(ecdf(datos$LYVE1[datos$diagnosis=="Diag2"]), col="red")
lines(ecdf(datos$LYVE1[datos$diagnosis=="Diag3"]), col="darkgreen")
legend("bottomright", legend=c("Diag1","Diag2","Diag3"),
       col=c("blue","red","darkgreen"), lwd=2)


# c)

boxplot(LYVE1 ~ diagnosis*sex, data = datos,
        main = "Boxplots de LYVE1 por diagnosis y sexo",
        col = c("lightblue","lightpink"))


# d)

plot(density(datos$LYVE1[datos$diagnosis=="Diag1"]), 
     col="blue", lwd=2, main="Densidades de LYVE1 por diagnosis",
     xlab="LYVE1", ylim=c(0, max(density(datos$LYVE1)$y)))
lines(density(datos$LYVE1[datos$diagnosis=="Diag2"]), col="red", lwd=2)
lines(density(datos$LYVE1[datos$diagnosis=="Diag3"]), col="darkgreen", lwd=2)
legend("topright", legend=c("Diag1","Diag2","Diag3"),
       col=c("blue","red","darkgreen"), lwd=2)


# e)

# Histogramas log(LYVE1)
par(mfrow = c(1,3))
for(d in levels(datos$diagnosis)){
  hist(datos$log_LYVE1[datos$diagnosis == d], 
       main = paste("log(LYVE1) -", d),
       xlab = "log(LYVE1)", col = "lightgreen", border = "black")
}
par(mfrow = c(1,1))


# Densidades log(LYVE1)
plot(density(datos$log_LYVE1[datos$diagnosis=="Diag1"]), 
     col="blue", lwd=2, main="Densidades de log(LYVE1) por diagnosis",
     xlab="log(LYVE1)", ylim=c(0, max(density(datos$log_LYVE1)$y)))
lines(density(datos$log_LYVE1[datos$diagnosis=="Diag2"]), col="red", lwd=2)
lines(density(datos$log_LYVE1[datos$diagnosis=="Diag3"]), col="darkgreen", lwd=2)
legend("topright", legend=c("Diag1","Diag2","Diag3"),
       col=c("blue","red","darkgreen"), lwd=2)



