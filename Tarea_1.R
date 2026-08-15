### Ejercicio 2.2 ###
install.packages("devtools")
install.packages("tidyverse")
install.packages("demodelr")
library(devtools)
library(tidyverse)
library(demodelr)

### Ejercicio 2.3  What are the variables listed in the dataset phosphorous in the demodelr library? (Hint: try the command ?phosphorous.) ###
phosphorous
# se observan:
#- Algae --> Contenido de fosforo en la comida de alga (%)
#- Daphnia --> Contenido de fosforo en Daphnia (%)

### Ejercicio 2.4 Make a scatterplot of the dataset phosphourous in the demodelr library.
ggplot(phosphorous, aes(algae, daphnia)) +
  geom_point()

### Ejercicio 2.5 CambioFigura 2.3así que la línea es azul y el tamaño es de 4 mm
# Choose spacing that is "smooth enough"
days <- seq(from = 0, to = 1500, by = 1) 
weight <- 70 / (1 + exp(2.46 - 0.017 * days))

wilson_model <- tibble(
  days = days,
  weight = weight
)

ggplot(data = wilson_model) +
  geom_line(aes(x = days, y = weight)) +
  labs(
    x = "Days since birth",
    y = "Weight (pounds)"
  )

### Ejercicio 2.6 Cambiar el color de los puntos enFigura 2.2a un color hexadecimal o un color nombrado de su elección.

ggplot(data = wilson) +
  geom_point(aes(x = days, y = weight), color = "#660033") +
  
  labs(
    x = "Days since birth",
    y = "Weight (pounds)"
  )

### Ejercicio 2.7 Para este ejercicio harás algunas trazas:
  
## Definir secuencia llamada x que vaya de -12 a 12 con un espaciado de 0.05
x <- seq (from = -12, to = 12, by= 0.05)
x
## Definir la variable y tal que y= sin (x).
y <- sin(x)
y
## Haz un gráfico de dispersión para graficar. Configura los puntos para que sean rojos.
dispers <- tibble(
  x = x,
  y = y
  )

ggplot(dispers)+
  geom_point(aes(x,y), color="red")

## Haz un gráfico de líneas para graficar. Etiqueta el eje x con el título de tu libro favorito. Etiqueta el eje y con tu comida favorita para comer.
ggplot(dispers)+
  geom_line(aes(x,y), color="hotpink4")+
  labs(
    x = "El diario de Ana Frank ",
    y = "Pollo con crema y champiñones"
  )

### Ejercicio 2.10 Investigar algunas funciones. 
# - Explica con tus propias palabras: 
# - runif (1,100,1000): Esta función lo que hace es dar 1 número entre el 100 y 1000 aleatoriamente. 
# - ceiling()

