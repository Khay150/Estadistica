
# Función para calcular el primer y segundo momento (media y varianza)
estimador_primer_momento <- function(v) {
  
  momento_1 <- 1/(mean(v))  # Estimación del primer momento ()
  
  return(momento_1)
}

estimador_segundo_momento <- function(v) {
  
  n <- length(v)
  
  promedio_cuadrado <- 0
  
  for ( i in 1:n) {
    
    promedio_cuadrado <- promedio_cuadrado + (v[i])^2
    
  }
  
  promedio_cuadrado <- promedio_cuadrado/n
  
  momento_2 = (-1 + sqrt(1 + 8*promedio_cuadrado))/ (2*promedio_cuadrado) 
  
  return(momento_2)
}


# Parámetro de la distribución
p <- 0.3

# Número de realizaciones
n <- 1000

# Generar el vector
datos <- rgeom(n, prob = p) + 1

estimadores <- c(estimador_primer_momento(datos), estimador_segundo_momento(datos))


estimador_generalizado <- function(v) {
  
  n <- length(v)
  
  promedio_caracteristico <- 0
  
  for ( i in 1:n) {
    
    if(v[i] == 1) {
      
      promedio_caracteristico <- promedio_caracteristico + 1
      
    }

  }
  
  promedio_caracteristico <- promedio_caracteristico/n
  
  return(promedio_caracteristico)
}


estimador_plug_in <- function(v) {
  
  estimador_plug = (2 - (3 - mean(v))/2)^2
  
}

# Generador de la distribución
rX <- function(n, theta) {
  
  valores <- c(-1, 0, 1)
  
  
  probs <- c((theta - 1)^2,
             2 * (theta - 1) * (2 - theta),
             (2 - theta)^2)
  
  
  # generar n observaciones
  sample(valores, size = n, replace = TRUE, prob = probs)
}


muestra <- rX(20, theta = 1.5)

estimadores_ej_2 <- c(estimador_generalizado(muestra), estimador_plug_in(muestra))

