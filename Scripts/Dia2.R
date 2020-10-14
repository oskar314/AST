# 1. Graficar la evolucion del PIB anual

# Instalar el conjunto de paquetes tidyverse
# install.packages("tidyverse")
# Se procede a cargar los paquetes
library(ggplot2)
library(readxl)

pib_raw <- read_xlsx("./Datos/PIB.xlsx", sheet = 1, range = "B10:AG11", col_names = FALSE)
pib_raw <- as.data.frame(t(pib_raw), stringAsFactors = FALSE)
rownames(pib_raw) <- NULL
colnames(pib_raw) <- c("Fecha", "PIB")
head(pib_raw)

# Procedemos a graficar el PIB Anual de Bolivia
fecha <- seq(as.Date("01/01/1988", "%d/%m/%Y"), as.Date("01/01/2019", "%d/%m/%Y"), by = "year")
pib_raw$Fecha <- fecha
g <- ggplot(data = pib_raw, aes(x = Fecha, y = as.numeric(as.character(PIB))))
g + geom_line() + labs(title = "Evolución PIB Anual Bolivia", x = "Años", y = "PIB Anual")

# 2. Funcion Impulso-Respuesta ecuacion en dif primer orden

phi <- 0.8
j <- 1:100
x <- phi ^ j
datos <- data.frame(Periodo = j, Respuesta = x)
g <- ggplot(data = datos, aes(x = Periodo, y = Respuesta))
g + geom_line()

phi <- -0.8
j <- 1:100
x <- phi ^ j
datos <- data.frame(Periodo = j, Respuesta = x)
g <- ggplot(data = datos, aes(x = Periodo, y = Respuesta))
g + geom_line()

# 3. Proceso Ruido Blanco
# Generar 100 números aleatorios normales estándar
set.seed(12345)
y <- rnorm(100)
x <- 1:100
datos <- data.frame(y = y, x = x)
g <- ggplot(data = datos, aes(x = x, y = y))
g + geom_line()

# 4. Billetes y Monedas Bolivia
datos <- read.csv("./Datos/billetes.csv") # Se procede a leer los datos
fecha <- as.Date(datos$ï..fecha,"%m/%d/%Y")# Convertir en formato fecha la variable fecha de la base de datos
datos2 <- data.frame(fecha=fecha,bm = datos$BM/1000000)
g <- ggplot(data=datos2,aes(x=fecha, y=bm)) 
g + geom_line() + labs(x="Fecha",y="Biletes y Monedas (billones Bs)") + geom_hline(yintercept=0)