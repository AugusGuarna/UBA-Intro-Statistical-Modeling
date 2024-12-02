# Título: Trabajo práctico IEyCD
# Autores: Victoria Aguirre, Augusto Guarnaccio, Bruno Muschietti

# En el script van a estar todo el código que haya sido necesario utilizar para
# responder las preguntas de programación del trabajo. Las respuestas a las preguntas
# se las puede encontrar en el informe. Esto con motivo de no sobrecargar de cosas
# el archivo

#-------------------------------------------------------------------------------#
# Importamos librerías 
library(sloop)

#-------------------------------------------------------------------------------#
# Pregunta 1

class(c(T,F))
class(c(T,F,1))
class(c(T,F,1,"1"))

# En R ocurre que los distintos tipos de datos básicos tienen una jerarquía entre
# sí que va desde el menos general que es el logical hasta el más general que es
# el character. Por lo tanto, cuando lee el vector le asigna la clase del elemento
# más general que esté conteniendo y considera que todos los otros valores
# comparten ese tipo.

#-------------------------------------------------------------------------------#
# Pregunta 2

class(density)
class(density(1:500))

# La diferencia está en que density es una función que recibe ciertos 
# parámetros de entrada y crea un objeto de clase `density` y que density(1:500)
# es justamente el objeto que crea la función. Como es un objeto pasa a tener 
# atributos y métodos particulares que son particulares a la clase "density".

#-------------------------------------------------------------------------------#
# Pregunta 3

# Primero nos fijamos la cantidad de clases a las que puede despachar "print"
sloop::s3_methods_generic("print")

# El genérico print sabe despachar a 255 clases en s3. Si miramos con atención
# el data frame sabemos que tiene 255 filas y 4 columnas, una de ellas bajo el
# nombre "class". Por lo tanto, utilizando la información brindada por la 
# dimensión sabemos que en s3 sabe despachar a 255 métodos.


# Ahora la cantidad de métodos de "density"

sloop::s3_methods_class("density")
methods(class="density")
sloop::s4_methods_class("density")

# En s3 density cuenta con el método print además de plot. Sin embargo, si
# usamos la función methods que viene con R vemos que aparecen más métodos.
# No obstante, estos métodos que se agregan son de s4 como podemos ver en el
# dataframe de arriba.

#-------------------------------------------------------------------------------#
# Pregunta 4

# Pegamos el código del test

## Genero la muestra
mu <- 1
sigma_sq <- 1
n <- 30
X <- rnorm(n, mean = mu, sd = sqrt(sigma_sq))
## Ejecuto el test
mu0 <- 0
alfa <- 0.05
test_t <- t.test(
  X,
  alternative = "two.sided",
  mu = mu0,
  conf.level = 1 - alfa
)

class(unclass(test_t))

# class(unclass(test_t)) devuelve una lista que tiene 10 elementos pues unclass()
# lo que hace es devolver una copia del argumento pero con su atributo removido 
# y test_t contiene el resultado de hacer una llamada con la función t.test.
# Función que devuelve una lista de clase htest que tiene la información sensible
# al test como el estadístico, el pvalor, el intervalo de confianza y demás. 
# Entre toda esta información sensible cuenta con 10 elementos