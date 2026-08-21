library(pacman)
library(deSolve)
library(tidyverse)
###Impementando las fuerzas vitales al SIR:

#Definir las condiciones iniciales:
initial_state <- c(S = 99, I = 1, R = 0)

# Define parameters including birth and death rate
parameters <- c(beta = 0.002,  # Tasa de transmisión
                gamma = 0.1,    # Tasa de recuperación
                mu = 0.02)      # Tasa de nacimiento/muertes

# Time span for simulation
times <- seq(0, 400, by = 1) #Empezar del 0 hasta el 400 de 1 en 1 

#Escribir el modelo SIR como una función ya con las fuerzas vitales
sir_model_vital <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {
    
    # Calcula total de la población
    N <- S + I + R
    
    # Ecuaciones diferenciales
    dS <- mu * N - beta * S * I - mu * S
    dI <- beta * S * I - gamma * I - mu * I
    dR <- gamma * I - mu * R
    
    # Return tasa de cambio
    list(c(dS, dI, dR))
  })
}
# Solve the modified SIR model with vital dynamics
sir_vital_out <- ode(
  y = initial_state,
  times = times,
  func = sir_model_vital, 
  parms = parameters
)

# Convert to dataframe
sir_vital_out <- as.data.frame(sir_vital_out)

# Plot the results
ggplot(sir_vital_out, aes(x = time)) +
  geom_line(aes(y = S, color = "Susceptible")) +
  geom_line(aes(y = I, color = "Infectious")) +
  geom_line(aes(y = R, color = "Recovered")) +
  labs(title = "SIR Model with Births and Deaths", x = "Time (days)", y = "Proportion of Population") +
  scale_color_manual(values = c("Susceptible" = "#2F5597", "Infectious" = "#C00000", "Recovered" = "#548235")) +
  theme_minimal() +
  theme(legend.title = element_blank())
