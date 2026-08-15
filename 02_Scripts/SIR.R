#11/agosto/2026

#Instalar paquetes
if(!require(pacman)) install.packages("pacman")
pacman::p_load(deSolve, # package to solve differential equations
               tidyverse, # package that includes ggplot2
               here)

### Step 1: Setting Initial Conditions and Parameters

condiciones_iniciales <- c(S=763, I=1, R=0)
condiciones_iniciales

### Define the parameters
parametros <- c(beta = 0.0026, # Transmission rate
                gamma = 0.5)   # Recovery rate

parametros

### Time span for the simulation
times <- seq(1, 14, by = 1)

### Step 2: Writing the SIR Model Function
sir_model <- function(time, state, parameters){
  with(as.list(c(state,parameters)), {
    
    #Rate of change: ecuaciones diferencialea para cada compartimento
    
    dS <- -beta*S*I # calcula la tasa de cambio de la población susceptible
    dI <- beta*S*I - gamma*I # calcula la tasa de cambio de la población infectada
    dR <- gamma*I # calculc la tasa de ca,bio de la población recuperada
    
    #Return the rate of the change
    list(c(dS,dI,dR))
  })
}
sir_model(times,condiciones_iniciales,parametros)

# Writing the SEIR model function
seir_model <- function(time, state, parameters){
  with(as.list(c(state,parameters)), {
    dS <- -beta * S * E
    dE <- beta * S * I - sigma * E # calcula la tasa de cambio en la población expuesta
    dI <- sigma * E * I - gamma * I
    dR <- gamma * I
    
    list(c(dS,dE,dI,dR))
  })
}

### Step 3: Solving the Differential Equations
sir_out <- ode(
  # inital state
  y = condiciones_iniciales,
  # sequence of time points
  times = times,
  # model function
  func = sir_model, 
  # parameters in the model
  parms = parametros
)
### Step 4: Converting the Output to a Data Frame
sir_outs <- as.data.frame(sir_out)

sir_outs

## Visualizing the results:
library(ggplot2)
# Create a plot to visualize the dynamics of the SIR model
sir_plot <- ggplot(sir_out, aes(x = time)) +
  # Graph Susceptible line
  geom_line(aes(y = S, color = "Susceptible")) +
  # Graph Infected line
  geom_line(aes(y = I, color = "Infectious")) +
  # Graph Recovered line
  geom_line(aes(y = R, color = "Recovered")) +
  # Add title and labels
  labs(title = "SIR Model Simulation", x = "Time (days)", y = "Population Count") +
  # Specify color for each line
  scale_color_manual(values = c("Susceptible" = "#2F5597", "Infectious" = "#C00000", "Recovered" = "#548235")) +
  # Select theme
  theme_minimal()
sir_plot

### Comparing Observed vs. Simulated Epidemic Curves
observed_data <-read_csv("01_RawData/influenza_boarding_school.csv")
observed_data

### Create a comparison graph
comparison_graph <- ggplot()+
  geom_line(data=observed_data, (aes(x=day, y=cases)))+
  geom_point(data=observed_data, (aes(x=day, y=cases)))+
  geom_line(data = sir_out, aes(x=time, y=I), color="hotpink4")
comparison_graph
    
