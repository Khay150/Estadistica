
# 1 a
t <- 0

for(i in 1:1000) {
  acum <- acum + i
}

t

# 1 b

n <- 1
acum <- 1

while( acum <=10000){
  
  n <- n + 1
  acum <- acum + n
}

n

# 1 c
sumatoria_positiva <- function(vector){
  acum <- 0
  n <- length(vector)
  for(i in 1:n) {
    if (vector[i] > 0) {
      acum <- acum + vector[i]
    }
    
  }
  return(acum)
}

# 2

xs <-seq(0, 1, by = 0.2)
ys <- xs*(1-xs)

plot(xs, ys, type = "l", col = "darkorchid3")
plot(xs, ys, type = "p", col = "sandybrown")
plot(xs, ys, type = "s", col = "lightseagreen")


# 3


x <-seq(0, 2*pi, lenght.out = 100)

sin_x <- sin(x)
cos_x <- cos(x)
cos_2_x <- cos(x^2)

plot(x, sin_x, type = "l", col = "darkorchid3")

lines(x, cos_x, col="sandybrown")

lines(x, cos_2_x, col="lightseagreen")

# Leyenda donde es una linea y donde lwd es el grosor de la linea y 
#lty=1 le dice que dibuje líneas continuas

legend("topright", legend=c("sin(x)", "cos(x)", "cos(x^2)"),
       col=c("darkorchid3", "sandybrown", "lightseagreen"),  lwd=2, lty=1)



# Leyenda donde es un punto y donde pch es la forma del punto, 
# bty="n" le dice que quite el borde de la caja y 
# lty=NA elimina la línea en la leyenda

legend("topright", legend=c("sin(x)", "cos(x)", "cos(x^2)"),
       col=c("darkorchid3", "sandybrown", "lightseagreen"),
       pch=16,   
       lty=0,  
       bty="n") 



# 4


datos <- read.table("autos.txt", header = TRUE, sep = "")

# header = TRUE le dice a R que la primera fila son los nombres de las columnas
# sep="" significa “separador en blanco”, o sea espacios o tabulaciones


# getwd() te dice el directorio en el que estas trabajando
# setwd("C:/Users/TuUsuario/MiCarpeta")  cambia el directorio de trabajo (Windows)
# setwd("/home/usuario/MiCarpeta")       cambia el directorio de trabajo (Linux/Mac)


# 4 a

View(datos[3,])


# 4 b

View(datos[,2])

# 4 c

# encontrar el índice del auto más barato
i_min <- which.min(datos$precio)

# obtener el valor de calidad correspondiente
calidad_min_precio <- datos$calidad[i_min]

calidad_min_precio


# 4 d

suma_precios <- sum(datos$precio[1:4])
suma_precios


# 4 e

View(apply(datos, 2, sum)) # El 2 significa columnas.
# Esto devuelve un vector con la suma de precio y la suma de calidad

View(apply(datos, 1, sum)) # El 1 significa filas.
# Esto devuelve un vector donde cada elemento es la suma de precio + calidad de esa fila.

# 4 f

plot(datos$precio, datos$calidad, xlab = "Precio",
     ylab = "Calidad", type = "p", col = "magenta3",
     pch = 16)

# 4 g

datos_ordenados <- datos[order(datos$precio), ] # Ordenar por precio (ascendente)
datos_ordenados <- datos[order(-datos$precio), ] # Ordenar por precio (descendente)

# 5

# 5 a

autos_gear4 <- rownames(mtcars[mtcars$gear == 4, ])
autos_gear4


# 5 b

autos_gear4_manual <- rownames(mtcars[mtcars$gear == 4 & mtcars$am == 1, ])
autos_gear4_manual


# 5 c

cantidad <- sum(mtcars$gear == 4 | mtcars$am == 1)
cantidad

# mtcars$gear == 4 | mtcars$am == 1 genera un vector lógico (TRUE/FALSE).
# sum() cuenta los TRUE, es decir, la cantidad de autos que cumplen la condición.


# 5 d

mtcars$am <- factor(mtcars$am,
                    levels = c(0, 1),
                    labels = c("Automática", "Manual"))

str(mtcars$am)


# 6

# 6 a

set.seed(123)   # para reproducibilidad
x <- rnorm(1000)   # 1000 observaciones ~ N(0,1)


hist(x,
     breaks = 30,             # número de intervalos
     col = "skyblue",         # color
     main = "Histograma de N(0,1)",
     xlab = "Valor")

boxplot(x,
        col = "lightgreen",
        main = "Boxplot de N(0,1)",
        horizontal = TRUE)


qqnorm(x, main = "QQ-Plot de N(0,1)")
qqline(x, col = "firebrick", lwd = 2)   # línea de referencia


# Función auxiliar para graficar histograma, boxplot y QQ-plot
graficos <- function(x, titulo){
  par(mfrow = c(1,3))  # tres gráficos en una fila
  
  # Histograma
  hist(x, breaks = 30, col="skyblue", main=paste("Histograma", titulo), xlab="")
  
  # Boxplot
  boxplot(x, col="lightgreen", main=paste("Boxplot", titulo), horizontal=TRUE)
  
  # QQ-plot
  qqnorm(x, main=paste("QQ-Plot", titulo))
  qqline(x, col="firebrick", lwd=2)
  
  par(mfrow = c(1,1)) # restaurar layout
}

# 6 b

# 6 b 1 (Binomial)

x1 <- rbinom(1000, 10, 0.4)
graficos(x1, "Binomial(10, 0.4)")

# 6 b 2 (Uniforme)

x2 <- runif(1000, 4, 8)
graficos(x2, "Uniforme(4, 8)")

# 6 b 3 (t de Student)

x3 <- rt(1000, df=5)   # df=5 grados de libertad (df por defecto es obligatorio)
graficos(x3, "t-Student (df=5)")

# 6 b 4 (Chi-cuadrado)

x4 <- rchisq(1000, 50)
graficos(x4, "Chi-cuadrado(50)")

# 6 b 5 (F de Snedecor)

x5 <- rf(1000, 90, 40)
graficos(x5, "F(90,40)")

# 6 b 6 (Gamma)

x6 <- rgamma(1000, shape=0.7)
graficos(x6, "Gamma(0.7)")





