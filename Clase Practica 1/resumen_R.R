
## Operadores Basicos

"h"
123

17%/%3 # modulo


peso <- 9 # asignacion

peso

# Operadores para Vectores

vector <- c(1,2,3,4,5)
vector
vector + 2
vector^2
vector[1]  # imprime el primer elemento del vector
vector[-4] # quita el elemento que esta en la posicion seleccionada
vector[2:4] #devuelve los elementos que estan entre las posicones seleccionadas, incluyendo los elementos en dichas                      posiciones
length(vector) # devuelve la longitud del vector

sum(vector) # devuelve la suma de todos los elementos
mean(vector) # devuelve el promedio de todos los elementos

vector2 <- c()
vector2 <- c(10, vector2) # agregar un elemento al inicio del vector
vector2 <- c(vector2, 5) # agregar un elemento al final del vector
vector2

sort(vector2) # ordena el vector de menor a mayor
sort(vector2, decreasing = TRUE) # ordena el vector de mayor a menor

min(vector2) # devuelve el elemento mas bajo del vector
max(vector2) # devuelve el elemento mas alto del vector

# Operadores Logicos

a <- 3
b <- 5

(b>a)|(a==2) # el signo | es un or
(b>a)&(a==2) # el signo & es un and
!(a>b) # el signo ! cambia el valor del booleano


# Funciones

sumatoria <- function(vector){
  acum <- 0
  n <- length(vector)
  for(i in 1:n) {
    acum <- acum + vector[i]
  }
  return(acum)
}

sumatoria(vector)


# Bucles

while(condicion){
  
}

if (condicion) {
  
}

# Graficos

seq(-3, 3, by = 1) # crea un vector en el rango de los primeros dos parametros con un salto del parametro by
seq(-3, 3, lenght.out = 10) # crea un vector en el rango de los primeros dos parametros donde se crean una cantidad de
                            # puntos equispaceados igual al del paremtro lenght.out 

xs <-seq(-3, 3, lenght.out = 100)
ys <- xs^2

plot(xs, ys, type = "l", col = "darkorchid3")
plot(xs, ys, type = "p", col = "sandybrown")
plot(xs, ys, type = "s", col = "lightseagreen")


View(mtcars)
View(mtcars$mpg) # devuelve los valores de la columna mpg
View(mtcars[,2:8]) # devuelve todas las filas pero solo las columnas de la 2 a la 8 inclusive
View(mtcars[, c("mpg", "am")]) # devuelve todas las filas pero solo las columnas mpg y am
View(mtcars[ mtcars$mpg < 25,]) # devuelde solo los datos donde la columna mpg es menor a 100


ncol(mtcars)
nrow(mtcars)
attributes(mtcars)