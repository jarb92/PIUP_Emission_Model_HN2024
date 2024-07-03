# PIUP_Emission_Model_HN2024

# Cargar librerías necesarias
library(readxl)
library(dplyr)
library(ggplot2)
library(forecast)

# Leer los datos desde el archivo Excel
data <- read_excel("C:/Users/DEES-JULIO/Desktop/GIZ/PIUP/PIUP_prueba.xlsx")

# Calcular las emisiones de CO2e utilizando la ecuación proporcionada
data <- data %>%
  mutate(
    emisiones_calculadas = ((Prod * 0.95) - Imp_clinker + Exp_clinker) * 0.525
  )

# Definir los años para la proyección
future_years <- 2021:2050

# Proyectar emisiones futuras utilizando un modelo ARIMA
fit <- auto.arima(data$emisiones_calculadas)
forecast_emissions <- forecast(fit, h=length(future_years))

# Crear un dataframe con las proyecciones
projection_data <- data.frame(
  year = future_years,
  emisiones_calculadas = as.numeric(forecast_emissions$mean)
)

# Función para decaimiento exponencial
decay_exponential <- function(initial, rate, years) {
  initial * exp(-rate * (years - min(years)))
}

# Aplicar escenarios de mitigación con decaimiento exponencial
reduccion_eficiencia <- 0.05 # 5% de reducción por eficiencia energética anual
reduccion_combustibles <- 0.07 # 7% de reducción por sustitución de combustibles anual
reduccion_CCS <- 0.10 # 10% de reducción por captura y almacenamiento de carbono anual

projection_data <- projection_data %>%
  mutate(
    emisiones_eficiencia = decay_exponential(emisiones_calculadas[1], reduccion_eficiencia, year),
    emisiones_combustibles = decay_exponential(emisiones_calculadas[1], reduccion_combustibles, year),
    emisiones_CCS = decay_exponential(emisiones_calculadas[1], reduccion_CCS, year)
  )

# Unir los datos históricos y las proyecciones
data_full <- bind_rows(
  data %>% select(year = year, emisiones_calculadas) %>% mutate(scenario = "Emisiones Calculadas"),
  projection_data %>% select(year, emisiones_calculadas) %>% mutate(scenario = "Emisiones Calculadas"),
  projection_data %>% select(year, emisiones_eficiencia) %>% rename(emisiones_calculadas = emisiones_eficiencia) %>% mutate(scenario = "Escenario Eficiencia Energética"),
  projection_data %>% select(year, emisiones_combustibles) %>% rename(emisiones_calculadas = emisiones_combustibles) %>% mutate(scenario = "Escenario Sustitución de Combustibles"),
  projection_data %>% select(year, emisiones_CCS) %>% rename(emisiones_calculadas = emisiones_CCS) %>% mutate(scenario = "Escenario CCS")
)

# Visualizar los resultados con decaimiento exponencial
ggplot(data_full, aes(x = year, y = emisiones_calculadas, color = scenario)) +
  geom_line(size = 1) +
  labs(
    title = "Trayectorias de Reducción de Emisiones de CO2e en el Sector PIUP",
    subtitle = "Proyecciones hasta 2050",
    x = "Año",
    y = "Emisiones de CO2e (toneladas)",
    color = "Escenario"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Emisiones Calculadas" = "red", 
                                "Escenario Eficiencia Energética" = "green", 
                                "Escenario Sustitución de Combustibles" = "blue",
                                "Escenario CCS" = "purple"))

# Análisis de Sensibilidad
sensitivity_data <- list()

for (ef in seq(0.01, 0.1, by=0.01)) {
  for (cb in seq(0.01, 0.1, by=0.01)) {
    for (ccs in seq(0.01, 0.1, by=0.01)) {
      temp_data <- projection_data %>%
        mutate(
          emisiones_eficiencia = decay_exponential(emisiones_calculadas[1], ef, year),
          emisiones_combustibles = decay_exponential(emisiones_calculadas[1], cb, year),
          emisiones_CCS = decay_exponential(emisiones_calculadas[1], ccs, year)
        ) %>%
        mutate(ef = ef, cb = cb, ccs = ccs)
      
      sensitivity_data <- bind_rows(sensitivity_data, temp_data)
    }
  }
}

# Visualizar algunos resultados del análisis de sensibilidad
sample_sensitivity <- sensitivity_data %>%
  filter(ef %in% c(0.02, 0.05, 0.08) & cb == 0.05 & ccs == 0.07)

ggplot(sample_sensitivity, aes(x = year, y = emisiones_eficiencia, color = as.factor(ef))) +
  geom_line(size = 1) +
  labs(
    title = "Análisis de Sensibilidad: Escenario de Eficiencia Energética",
    subtitle = "Variaciones en la tasa de reducción anual",
    x = "Año",
    y = "Emisiones de CO2e (toneladas)",
    color = "Tasa de Reducción"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("0.02" = "green", "0.05" = "blue", "0.08" = "purple"))

