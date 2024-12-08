#------------------------------------------------------------------------------#
#                               Economía Urbana
#                     Profesor: Ignacio Sarmiento-Barbieri
#
#                               Proyecto Final
#
# Cadena-Guerrero Kelly Daniela - 202122032
# González Beltrán Nikolle Alejandra - 202111607
# González Beltrán Valentina - 202111608
#------------------------------------------------------------------------------#

# Limpiamos el ambiente
rm(list = ls())
gc()

# Configuramos el directorio de trabajo
setwd("~/Documents/Programacion/R/Urbana/Proyecto")

# Cargamos paquetes necesarios
if (!require("pacman")) install.packages("pacman")
pacman::p_load(DeclareDesign, dplyr, ggplot2, writexl, margins, ivreg, pwr, tidyr)

###############################################################################
#                          1: Declarar modelo                                 #
###############################################################################
set.seed(2003)  # Establece la semilla para reproducibilidad

# -------------------------------------------------------------------------
# 1. Definir parámetros y variables
# -------------------------------------------------------------------------
# 1. Tamaño de muestra 
#-----------------------
N <- 4500  # Cantidad de vendedores ambulantes de Kennedy

# 2. Ubicación de los participantes. 
#-------------------------------------
# Por simplicidad asumiremos que los trabajadores se distribuyen de manera 
# uniforme por las UPZ Más adelante exploraremos  si este es un supuesto que 
# pueda alterar los resultados del experimento. Se asume que es uniforme debido 
# a que no hay buenos estudios sobre la  distribucion de vendedores ambulantes 
# en Bogotá.
upz_kennedy = c("Américas", "Carvajal", "Castilla", "Kennedy Central", "Timiza", "Tintal Norte", "Calandaima", "Corabastos", "Gran Britalia", "Patio Bonito", "Las Margaritas", "Bavaria")
UPZ_Pre = sample (x = upz_kennedy , size = N, replace = TRUE)

# 3. Ingresos mensuales promedio
#---------------------------------
# De acuerdo con la GEIH de Junio de 2023 (Fuente: https://microdatos.dane.gov.co/index.php/catalog/782/get-microdata)
# El promedio del ingreso de los trabajadores cuenta propistas fue de 6,369,457
# Anuales. No hay registros para los ingresos mensuales de estos trabajadores.
# Por lo tanto se usaran las estadísticas para ingresos anuales.
## Todas las estadísticas se obtuvieron al procesar esta encuesta
sd_ingresosanuales = 13100000
mean_ingresosanuales = 6369457

# Convertir la media y la desviación estándar en los parámetros de una distribución log-normal
mu = log(mean_ingresosanuales^2 / sqrt(sd_ingresosanuales^2 + mean_ingresosanuales^2))
sigma = sqrt(log(1 + (sd_ingresosanuales^2 / mean_ingresosanuales^2)))

# Greneramos la variable de ingresos
IngresosAnuales_Pre = rlnorm(N, meanlog = mu, sdlog = sigma)

# La transformamos a logaritmo
lnIngresosAnuales_Pre = log(IngresosAnuales_Pre)

# 4. Percepción de seguridad
#-----------------------------
# Vamos a distribuir las percpciones de seguridad como si las personas en 
# general tuvieran una mala percepcion de la seguridad. En Bogotá esto es cierto.
PuntajesSeguridad = c(1,2,3,4,5)
PercepcionSeguridad_Pre = sample(x = PuntajesSeguridad, size=N, replace= TRUE, prob = c(2/10,3/10,3/10,1/10,1/10))

# 5. Numero de conexiones laborales 
#------------------------------------
# Vamos a distribuir el numero de conexiones laborales se distribuye 
# uniformemente. El minimo de conexiones que puede tener una persona es de 0.
# Hay evidencia que indica que las personas en promedio suelen tener conexiones
# personales y laborales cercanas a 150 personas (FUENTE). Se estima ademas que 
# las personas en el sector informal tienen menos conexiones que aquellas en el 
# sector formal (FUENTE). Si asumimos que el 30% (FUENTE) de las conexiones son 
# conexiones  laborales, y que los vendedores informales podrian tener hasta 30 
# conexiones laborales. as adelante confirmaremos si este es un supuesto crítico 
# del experimento.
ConexionesLaborales_Pre = runif(N, min=0, max=30)

# -------------------------------------------------------------------------
# 2. Elegibilidad de tratamiento
# -------------------------------------------------------------------------
# Hacemos una tabla de frecuencias para ver por donde se distribuyen las 
# personas de la muestra. 
Tabla_frecuencia_UPZ <- table(UPZ_Pre)
print(Tabla_frecuencia_UPZ)

# Dada la distribución de la población hacemos la siguiente división:
Grupo1 = c("Las Margaritas", "Patio Bonito", "Calandamia", "Tintal Norte", "Castilla", "Barvaria") 
Grupo2 = c("Américas", "Carvajal", "Corabastos", "Gran Britalia", "Kennedy Central", "Timiza")

# Ahora vamos a escoger el grupo elegible de manera aleatoria
Aleatorizacion = sample(c("Grupo1","Grupo2"), size=1)
  # Vemos el grupo escogido
  print(Aleatorizacion)
  
# Creamos variable dicotómica para elegibilidad de tratamiento
Elegible = ifelse(UPZ_Pre %in% Grupo2, 1, 0)

# -------------------------------------------------------------------------
# 3. Asignación de tratamiento
# -------------------------------------------------------------------------
# Creamos variable de asignación a tratamiento
Tratamiento_Asignado = sample(c(0, 1), size = N, replace = TRUE)

  # Ajustar tratamiento para quienes no son elegibles
  Tratamiento_Asignado = ifelse(Elegible == 1, Tratamiento_Asignado, 0)

# Creamos variable de quienes efectivamente reciben el tratamiento
Tratamiento_Recibido <- ifelse(Tratamiento_Asignado == 1, rbinom(N, 1, prob = 0.85), 0)

# -------------------------------------------------------------------------
# 4. Resultados Potenciales
# -------------------------------------------------------------------------
# 1. Ingresos 
#--------------
# Supongamos que el tratamiento incrementa los ingresos en un 3% para aquellos 
# que lo reciben. -> Esto está basado en literatura. Revisar Anexo del Paper
Incremento_Ingresos = 0.03  # 3% de incremento

# Aplicamos el incremento porcentual en la escala natural
IngresosAnuales_Post <- exp(lnIngresosAnuales_Pre) * (1 + Incremento_Ingresos * Tratamiento_Recibido) + 
  rnorm(N, mean = 0, sd = 0.2)

# Convertimos de nuevo a logaritmo
lnIngresosAnuales_Post <- log(IngresosAnuales_Post)

# 2. Percepción de Seguridad
# ----------------------------
# Supongamos que el tratamiento mejora la percepción de seguridad en 1 punto 
# en promedio. -> Esto está basado en literatura. Revisar Anexo del Paper
Mejora_Seguridad = 1

# Generamos la percepción de seguridad posterior
PercepcionSeguridad_Post <- pmin(PercepcionSeguridad_Pre + Mejora_Seguridad * Tratamiento_Recibido, 5) + 
  rnorm(N, mean = 0, sd = 0.3)

# 3. Conexiones Laborales
# -------------------------
# Supongamos que el tratamiento incrementa en 5 unidades las conexiones laborales.
Incremento_Conexiones = 5 

# Generamos las conexiones laborales posteriores
ConexionesLaborales_Post <- ConexionesLaborales_Pre + Incremento_Conexiones*Tratamiento_Recibido + + 
  rnorm(N, mean = 0, sd = 3)

# -------------------------------------------------------------------------
# 5. Base de datos
# -------------------------------------------------------------------------
# Creamos base de datos tipo wide
data <- data.frame(
  id = 1:N,
  UPZ_Pre = UPZ_Pre,
  lnIngresosAnuales_Pre = lnIngresosAnuales_Pre,
  PercepcionSeguridad_Pre = PercepcionSeguridad_Pre,
  ConexionesLaborales_Pre = ConexionesLaborales_Pre,
  Elegible = Elegible,
  Tratamiento_Asignado = Tratamiento_Asignado,
  Tratamiento_Recibido = Tratamiento_Recibido,
  lnIngresosAnuales_Post = lnIngresosAnuales_Post,
  PercepcionSeguridad_Post = PercepcionSeguridad_Post,
  ConexionesLaborales_Post = ConexionesLaborales_Post
)

################################################################################
#                        Simulación de resultados                              #
################################################################################
# -------------------------------------------------------------------------
# 6. Transformar la base de datos
# -------------------------------------------------------------------------
# Transformar la base de datos a formato long
data <- data %>%
  pivot_longer(
    cols = starts_with("lnIngresosAnuales_") | starts_with("PercepcionSeguridad_") | starts_with("ConexionesLaborales_"),
    names_to = c(".value", "time"),
    names_sep = "_"
  )

# Transformamos variable Tiempo a variable Post
data$time = ifelse(data$time=="Post",1,0)

# -------------------------------------------------------------------------
# 7. Regresiones - Ingreso
# -------------------------------------------------------------------------
# Modelo DiD sin variable instrumental
# ---------------------------------------
DiD_model <- lm(
  lnIngresosAnuales ~ Tratamiento_Recibido * time + Elegible,
  data = data
)

summary(DiD_model)

# 2. Modelo DiD con variable instrumental estimado en dos etapas
# ----------------------------------------------------------------
# Primera etapa: Predicción del Tratamiento Recibido
IV_first_stage <- lm(
  Tratamiento_Recibido ~ Tratamiento_Asignado * time + Elegible,
  data = data
)

summary(IV_first_stage)

# Agregamos la predicción del tratamiento recibido a la base de datos
data <- data %>%
  mutate(Tratamiento_Recibido_Predicho = predict(IV_first_stage))

# Segunda etapa: Modelo de DiD con tratamiento instrumentado
IV_DiD_model <- lm(
  lnIngresosAnuales ~ Tratamiento_Recibido_Predicho * time + Elegible,
  data = data
)

summary(IV_DiD_model)

# -------------------------------------------------------------------------
# 8.. Regresiones - Percepción seguridad
# -------------------------------------------------------------------------
# Modelo DiD sin variable instrumental
# ---------------------------------------
DiD_model <- lm(
  PercepcionSeguridad ~ Tratamiento_Recibido * time + Elegible,
  data = data
)

summary(DiD_model)

# 2. Modelo DiD con variable instrumental estimado en dos etapas
# ----------------------------------------------------------------
# Primera etapa: Predicción del Tratamiento Recibido
IV_first_stage <- lm(
  Tratamiento_Recibido ~ Tratamiento_Asignado * time + Elegible,
  data = data
)

summary(IV_first_stage)

# Agregamos la predicción del tratamiento recibido a la base de datos
data <- data %>%
  mutate(Tratamiento_Recibido_Predicho = predict(IV_first_stage))

# Segunda etapa: Modelo de DiD con tratamiento instrumentado
IV_DiD_model <- lm(
  PercepcionSeguridad ~ Tratamiento_Recibido_Predicho * time + Elegible,
  data = data
)

summary(IV_DiD_model)

# -------------------------------------------------------------------------
# 9. Regresiones - Conexiones laborales
# -------------------------------------------------------------------------
# Modelo DiD sin variable instrumental
# ---------------------------------------
DiD_model <- lm(
  ConexionesLaborales ~ Tratamiento_Recibido * time + Elegible,
  data = data
)

summary(DiD_model)

# 2. Modelo DiD con variable instrumental estimado en dos etapas
# ----------------------------------------------------------------
# Primera etapa: Predicción del Tratamiento Recibido
IV_first_stage <- lm(
  Tratamiento_Recibido ~ Tratamiento_Asignado * time + Elegible,
  data = data
)

summary(IV_first_stage)

# Agregamos la predicción del tratamiento recibido a la base de datos
data <- data %>%
  mutate(Tratamiento_Recibido_Predicho = predict(IV_first_stage))

# Segunda etapa: Modelo de DiD con tratamiento instrumentado
IV_DiD_model <- lm(
  ConexionesLaborales ~ Tratamiento_Recibido_Predicho * time + Elegible,
  data = data
)

summary(IV_DiD_model)

################################################################################
#                    Diagnostico del modelo - Poder                            #
################################################################################
# -------------------------------------------------------------------------
# 10. Poder estadístico - Ingresos
# -------------------------------------------------------------------------
# Especificamos el tamaño del efecto esperado (Cohen's d), alfa y poder deseado
effect_size_income <- abs(mean(lnIngresosAnuales_Post) - mean(lnIngresosAnuales_Pre)) / sd(lnIngresosAnuales_Pre)
alpha <- 0.1  # Nivel de significancia
power <- 0.8   # Poder deseado

# Calcular tamaño de muestra necesario para detectar el efecto
sample_size <- pwr.t.test(d = effect_size_income, sig.level = alpha, power = power, type = "two.sample")$n

cat("Tamaño de muestra necesario para ingresos:", sample_size, "\n")

# -------------------------------------------------------------------------
# 11. Poder estadístico - Percepción de Seguridad
# -------------------------------------------------------------------------
# Especificamos el tamaño del efecto esperado (Cohen's d), alfa y poder deseado
effect_size_income <- abs(mean(PercepcionSeguridad_Post) - mean(PercepcionSeguridad_Pre)) / sd(PercepcionSeguridad_Pre)
alpha <- 0.1  # Nivel de significancia
power <- 0.8   # Poder deseado

# Calcular tamaño de muestra necesario para detectar el efecto
sample_size <- pwr.t.test(d = effect_size_income, sig.level = alpha, power = power, type = "two.sample")$n

cat("Tamaño de muestra necesario para ingresos:", sample_size, "\n")

# -------------------------------------------------------------------------
# 12. Poder estadístico - Conexiones Laborales
# -------------------------------------------------------------------------
# Especificamos el tamaño del efecto esperado (Cohen's d), alfa y poder deseado
effect_size_income <- abs(mean(ConexionesLaborales_Post) - mean(ConexionesLaborales_Pre)) / sd(ConexionesLaborales_Pre)
alpha <- 0.1  # Nivel de significancia
power <- 0.8   # Poder deseado

# Calcular tamaño de muestra necesario para detectar el efecto
sample_size <- pwr.t.test(d = effect_size_income, sig.level = alpha, power = power, type = "two.sample")$n

cat("Tamaño de muestra necesario para ingresos:", sample_size, "\n")

################################################################################
#                    Diagnostico del modelo - Sesgo                            #
################################################################################
# -------------------------------------------------------------------------
# 13. Sesgo Estadístico - Ingresos
# -------------------------------------------------------------------------
# Crear un marco de datos para almacenar resultados de simulaciones
simulations <- 1000  # Número de simulaciones
bias_results <- data.frame(
  Simulation = 1:simulations,
  True_Effect = Incremento_Ingresos,
  Estimated_Effect = numeric(simulations)
)

# Simular y calcular sesgo
for (i in 1:simulations) {
  # Crear muestras aleatorias de ingresos antes y después del tratamiento
  sample_pre <- sample(lnIngresosAnuales_Pre, size = N, replace = TRUE)
  sample_post <- sample(lnIngresosAnuales_Post, size = N, replace = TRUE)
  
  # Estimar efecto observado
  observed_effect <- mean(sample_post) - mean(sample_pre)
  bias_results$Estimated_Effect[i] <- observed_effect
}

# Calcular sesgo promedio y desviación estándar
bias_results$Bias <- bias_results$Estimated_Effect - bias_results$True_Effect
mean_bias <- mean(bias_results$Bias)
sd_bias <- sd(bias_results$Bias)

cat("Sesgo promedio:", mean_bias, "\n")
cat("Desviación estándar del sesgo:", sd_bias, "\n")

# -------------------------------------------------------------------------
# 14. Sesgo Estadístico - Percepcion de seguridad
# -------------------------------------------------------------------------
# Crear un marco de datos para almacenar resultados de simulaciones
simulations <- 1000  # Número de simulaciones
bias_results <- data.frame(
  Simulation = 1:simulations,
  True_Effect = Incremento_Ingresos,
  Estimated_Effect = numeric(simulations)
)

# Simular y calcular sesgo
for (i in 1:simulations) {
  # Crear muestras aleatorias de ingresos antes y después del tratamiento
  sample_pre <- sample(PercepcionSeguridad_Pre, size = N, replace = TRUE)
  sample_post <- sample(PercepcionSeguridad_Post, size = N, replace = TRUE)
  
  # Estimar efecto observado
  observed_effect <- mean(sample_post) - mean(sample_pre)
  bias_results$Estimated_Effect[i] <- observed_effect
}

# Calcular sesgo promedio y desviación estándar
bias_results$Bias <- bias_results$Estimated_Effect - bias_results$True_Effect
mean_bias <- mean(bias_results$Bias)
sd_bias <- sd(bias_results$Bias)

cat("Sesgo promedio:", mean_bias, "\n")
cat("Desviación estándar del sesgo:", sd_bias, "\n")

# -------------------------------------------------------------------------
# 15. Sesgo Estadístico - Conexiones Laborales
# -------------------------------------------------------------------------
# Crear un marco de datos para almacenar resultados de simulaciones
simulations <- 1000  # Número de simulaciones
bias_results <- data.frame(
  Simulation = 1:simulations,
  True_Effect = Incremento_Ingresos,
  Estimated_Effect = numeric(simulations)
)

# Simular y calcular sesgo
for (i in 1:simulations) {
  # Crear muestras aleatorias de ingresos antes y después del tratamiento
  sample_pre <- sample(ConexionesLaborales_Pre, size = N, replace = TRUE)
  sample_post <- sample(ConexionesLaborales_Post, size = N, replace = TRUE)
  
  # Estimar efecto observado
  observed_effect <- mean(sample_post) - mean(sample_pre)
  bias_results$Estimated_Effect[i] <- observed_effect
}

# Calcular sesgo promedio y desviación estándar
bias_results$Bias <- bias_results$Estimated_Effect - bias_results$True_Effect
mean_bias <- mean(bias_results$Bias)
sd_bias <- sd(bias_results$Bias)

cat("Sesgo promedio:", mean_bias, "\n")
cat("Desviación estándar del sesgo:", sd_bias, "\n")






