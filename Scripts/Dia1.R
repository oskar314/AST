# 1. Asignacion de variables
x <- 5
y = 3.4

# 2. Tipos de datos 
x <- 6 # Tipo numerico
class(x)
x <- "hola" # Tipo caracter
class(x)
x <- TRUE # Tipo logical
class(x)

# 3. Tipos de datos estructurados
# Tipo vector
x <- c(1, 3, 6 ,7)
x[2]
x[2:4]
x <- c(1, "hola", TRUE, 9)
# Tipo matriz
x <- matrix(c(1, 2, 3, 4), ncol = 2, nrow = 2, byrow = TRUE)
x[1, 2]
x[2,]
x
# Tipo data.frame
x <- data.frame(t = c(1, 2), genero = c("hombre", "mujer"))
View(x)
x$t
x$genero[2]
x[, 1]
x[, "genero"]
# Tipo Lista
x <- list(t = 1, j = c(TRUE, FALSE), k = c("hola", "dos", "tres"))
x$t
x$k[3]
# Tipo factor
genero <- factor(c("hombre", "mujer", "mujer", "mujer"), levels = c("mujer", "hombre"))
as.numeric(genero)

# 4. Estructuras de control
# Estructura if

x <- 0

if (x > 0) {
        print("Es positivo")
} else if (x < 0) {
        print("Es negativo")
} else {
        print("Es 0")
}

# Estructura for

for (i in c(1, 3, 4)) {
        print(i)
}

for (i in 1:10) {
        print(i)
}

# 5. Funciones 

saludo <- function() {
        print("Hola a todos")
}
saludo()

suma <- function(x, y) {
        x + y
}
suma(1, 3)
suma(y = 2, x = 1)

resta <- function(x, y = 3) {
        x - y
}
resta(4)
resta(4, 5)

# 5. Evolucion PIB Bolivia Anual

if (!(file.exists("./Datos/PIB.xlsx"))){
        url1 <- "https://www.ine.gob.bo/index.php/descarga/491/pib-segun-actividad-economica/46183/bolivia-producto-interno-bruto-a-precios-constantes-segun-actividad-economica-1988-2019.xlsx"
        download.file(url1, destfile = "./Datos/PIB.xlsx", mode = "wb")
}

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