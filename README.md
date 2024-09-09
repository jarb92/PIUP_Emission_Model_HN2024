# PIUP_Emission_Model_HN2024


# Cargar librerías necesarias
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)

# Leer los datos desde el archivo Excel
data <- read_excel("C:/Users/DEES-JULIO/Desktop/GIZ/PIUP/PIUP_prueba.xlsx")

# Definir las variables a partir del archivo
produccion_cemento <- data$`Producción de cemento (toneladas)`
fraccion_clinker <- 0.75  # Fracción de clínker fija según evidencia empírica
import_clinker <- data$`Importación de clínker (toneladas)`  # Importaciones de clínker
export_clinker <- data$`Exportación de clínker (toneladas)`  # Exportaciones de clínker

produccion_cal <- data$`Producción de cal (toneladas)`
import_cal <- data$`Importación de cal (toneladas)`  # Importaciones de cal
export_cal <- data$`Exportación de cal (toneladas)`  # Exportaciones de cal

produccion_vidrio <- data$`Producción de vidrio (toneladas)`
import_vidrio <- data$`Importación de vidrio (toneladas)`  # Importaciones de vidrio
export_vidrio <- data$`Exportación de vidrio (toneladas)`  # Exportaciones de vidrio

HFCs_cargados <- data$`HFCs cargados (toneladas)2`
tasa_fuga <- data$`Tasa de fuga anual (%)`
years <- data$Año

# Factores de emisión según la metodología del IPCC
factor_emision_clinker <- 0.525  # ton CO2 por ton de clínker
factor_emision_cal <- 0.785  # ton CO2 por ton de CaCO3 calcinado
factor_emision_vidrio <- 0.785  # ton CO2 por ton de carbonato en vidrio

# Calcular producción neta ajustada con importaciones y exportaciones

# Producción neta de clínker
produccion_clinker <- (produccion_cemento * fraccion_clinker) - import_clinker + export_clinker

# Producción neta de cal
produccion_cal_neta <- produccion_cal - import_cal + export_cal

# Producción neta de vidrio
produccion_vidrio_neta <- produccion_vidrio - import_vidrio + export_vidrio

# Calcular emisiones históricas

# Emisiones de CO2 por producción neta de clínker
emisiones_clinker <- produccion_clinker * factor_emision_clinker

# Emisiones de CO2 por producción neta de cal
emisiones_cal <- produccion_cal_neta * factor_emision_cal

# Emisiones de CO2 por producción neta de vidrio
emisiones_vidrio <- produccion_vidrio_neta * factor_emision_vidrio

# Emisiones de HFCs (usando tasa de fuga)
emisiones_HFCs <- HFCs_cargados * tasa_fuga

# Convertir las emisiones a Gigatoneladas (Gt)
emisiones_clinker_Gt <- emisiones_clinker / 1e3
emisiones_cal_Gt <- emisiones_cal / 1e3
emisiones_vidrio_Gt <- emisiones_vidrio / 1e3
emisiones_HFCs_Gt <- emisiones_HFCs / 1e3

# Unir todas las emisiones
data <- data %>%
  mutate(
    Emisiones_Clinker_Gt = emisiones_clinker_Gt,
    Emisiones_Cal_Gt = emisiones_cal_Gt,
    Emisiones_Vidrio_Gt = emisiones_vidrio_Gt,
    Emisiones_HFCs_Gt = emisiones_HFCs_Gt,
    Emisiones_Totales_Gt = Emisiones_Clinker_Gt + Emisiones_Cal_Gt + Emisiones_Vidrio_Gt + Emisiones_HFCs_Gt
  )

# Proyección de escenarios BAU hasta 2050
future_years <- 2000:2050

# Ajustar una regresión lineal para proyectar las variables
modelo_clinker <- lm(produccion_clinker ~ years)
modelo_cal <- lm(produccion_cal_neta ~ years)
modelo_vidrio <- lm(produccion_vidrio_neta ~ years)
modelo_HFCs <- lm(HFCs_cargados ~ years)

# Proyectar las producciones futuras usando la regresión lineal
produccion_clinker_futuro <- predict(modelo_clinker, newdata = data.frame(years = future_years))
produccion_cal_futuro <- predict(modelo_cal, newdata = data.frame(years = future_years))
produccion_vidrio_futuro <- predict(modelo_vidrio, newdata = data.frame(years = future_years))
HFCs_cargados_futuro <- predict(modelo_HFCs, newdata = data.frame(years = future_years))

# Calcular las emisiones futuras en Gt
Prod_cemento <- produccion_clinker_futuro * factor_emision_clinker / 1e3
Prod_cal <- produccion_cal_futuro * factor_emision_cal / 1e3
Prod_vidrio <- produccion_vidrio_futuro * factor_emision_vidrio / 1e3
Aire_Refrigerantes <- HFCs_cargados_futuro * tasa_fuga[length(tasa_fuga)] / 1e3

# Crear un dataframe con los resultados proyectados
proyeccion_BAU <- data.frame(
  Año = future_years,
  Prod_cemento = Prod_cemento,
  Prod_cal = Prod_cal,
  Prod_vidrio = Prod_vidrio,
  Aire_Refrigerantes = Aire_Refrigerantes,
  produccion_clinker_futuro=produccion_clinker_futuro,
  produccion_cal_futuro=produccion_cal_futuro,
  produccion_vidrio_futuro=produccion_vidrio_futuro,
  HFCs_cargados_futuro=HFCs_cargados_futuro
  
)

proyeccion_BAU <- proyeccion_BAU %>%
  mutate(Emisiones_Totales_Gt = Prod_cemento + Prod_cal + Prod_vidrio + Aire_Refrigerantes)

# Filtrar datos para años específicos
# Filtrar datos para años específicos
años_especificos <- c(2018, 2020, 2022, 2025, 2030, 2035, 2040, 2045, 2050)
data_filtrada <- proyeccion_BAU %>%
  filter(Año %in% años_especificos)

# Transformar a formato largo para crear gráfico apilado solo para los años específicos
data_long_filtrada <- data_filtrada %>%
  pivot_longer(cols = c(Prod_cemento, Prod_cal, Prod_vidrio, Aire_Refrigerantes),
               names_to = "Categoría", values_to = "Emisiones")

# Visualización
ggplot(data_long_filtrada, aes(x = factor(Año), y = Emisiones, fill = Categoría)) +
  geom_bar(stat = "identity", width = 0.6) +  # Ajuste del ancho de las barras
  geom_text(aes(label = round(Emisiones, 2)), position = position_stack(vjust = 0.5), size = 3) +  # Etiquetas en las barras
  labs(title = "",
       x = "Año", y = "Emisiones Totales (Gg CO2e)", fill = "Categoría") +
  scale_fill_manual(values = c("Prod_cemento" = "orange", 
                               "Prod_cal" = "grey", 
                               "Prod_vidrio" = "blue", 
                               "Aire_Refrigerantes" = "yellow")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 360, hjust = 0.5),  # Rotar los textos del eje x
        legend.position = "bottom")

# Guardar los resultados en Excel
library(openxlsx)
write.xlsx(proyeccion_BAU, "C:/Users/DEES-JULIO/Desktop/GIZ/PIUP/Proyecciones_Emisiones_PIUP.xlsx", row.names = FALSE)


