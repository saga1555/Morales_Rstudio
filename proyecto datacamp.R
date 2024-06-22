# Cargar bibliotecas necesarias
library(tidyverse)

# Leer los archivos CSV desde GitHub y suprimir el mensaje de tipo de columna
yearly <- read_csv('https://raw.githubusercontent.com/shukkkur/Analyzing-The-Discovery-of-Handwashing/main/datasets/yearly_deaths_by_clinic.csv', show_col_types = FALSE)
monthly <- read_csv('https://raw.githubusercontent.com/shukkkur/Analyzing-The-Discovery-of-Handwashing/main/datasets/monthly_deaths.csv', show_col_types = FALSE)

# Recuperar la especificación completa de las columnas de los datos anuales
yearly_spec <- spec(yearly)
print(yearly_spec)

# Recuperar la especificación completa de las columnas de los datos mensuales
monthly_spec <- spec(monthly)
print(monthly_spec)

# Inspeccionar los datos mensuales
head(monthly)
summary(monthly)
str(monthly)

# Añadir columna proportion_deaths a los datos anuales
yearly <- yearly %>%
  mutate(proportion_deaths = deaths / births)

# Añadir columna proportion_deaths a los datos mensuales
monthly <- monthly %>%
  mutate(proportion_deaths = deaths / births)

# Verificar las nuevas columnas
head(yearly)
head(monthly)


# Graficar la proporción de muertes anual a lo largo del tiempo por clínica
ggplot(yearly, aes(x = year, y = proportion_deaths, color = clinic)) +
  geom_line() +
  geom_point() +
  labs(title = "Proporción de muertes anual por clínica",
       x = "Año",
       y = "Proporción de muertes",
       color = "Clínica") +
  theme_minimal()

# Graficar la proporción de muertes mensual a lo largo del tiempo
ggplot(monthly, aes(x = date, y = proportion_deaths)) +
  geom_line() +
  geom_point() +
  labs(title = "Proporción de muertes mensual",
       x = "Fecha",
       y = "Proporción de muertes") +
  theme_minimal()

# Añadir columna handwashing_started a los datos mensuales
monthly <- monthly %>%
  mutate(handwashing_started = date >= as.Date("1847-06-01"))

ggplot(monthly, aes(x = date, y = proportion_deaths, color = handwashing_started)) +
  geom_line() +
  geom_point() +
  labs(title = "Proporción de muertes mensual con lavado de manos",
       x = "Fecha",
       y = "Proporción de muertes",
       color = "Lavado de manos iniciado") +
  theme_minimal()

# Calcular la media de la proporción de muertes antes y después del lavado de manos
monthly_summary <- monthly %>%
  group_by(handwashing_started) %>%
  summarise(mean_proportion_deaths = mean(proportion_deaths))
#presentar tabla
monthly_summary
