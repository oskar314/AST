# 1. Evolución crecimiento PIB trimestral Bolivia

if (!file.exists("./Datos/PIBTrim.xlsx")) {
        url <- "https://nube.ine.gob.bo/index.php/s/I0Wq5MdeiEwdZEC/download"
        download.file(url, destfile = "./Datos/PIBTrim.xlsx", mode = "wb")
}

library(readxl)

pib <- read_xlsx("./Datos/PIBTrim.xlsx", sheet = 1, range = "A10:H163")

datos <- data.frame(Fecha = pib[, 1], pib = pib[, 8])

colnames(datos) <- c("Fecha", "PIB")

View(datos)

filas <- which(datos$Fecha == "I   Trimestre" | datos$Fecha == "II  Trimestre" | 
                       datos$Fecha == "III Trimestre" | datos$Fecha == "IV Trimestre")

datos <- datos[filas, ]

datos$Fecha <- seq(as.Date("01-01-1990", "%d-%m-%Y"), as.Date("01-04-2020", "%d-%m-%Y"), by = "quarter")

library(ggplot2)

g <- ggplot(data = datos, aes(x = Fecha, y = PIB))
g + geom_line()

pib <- ts(datos$PIB, start = c(1990,1), end = c(2020, 2), frequency = 4)

descom <- stl(pib, s.window = 9, s.degree = 1, t.window = 9, t.degree = 1)

plot(descom)

pib_us <- pib - descom$time.series[, 1]

datos$pib_us <- as.numeric(pib_us)

g <- ggplot(data = datos, aes(x = Fecha, y = pib_us))
g + geom_line()

n <- nrow(datos)

datos_mod <- datos[1:(n - 10), ]

g <- ggplot(data = datos_mod, aes(x = Fecha, y = pib_us))
g + geom_line()

datos_mod$crec <- c(NA, (datos_mod$pib_us[2:112] / datos_mod$pib_us[1:111] - 1) * 100)

g <- ggplot(data = datos_mod[2:112, ], aes(x = Fecha, y = crec))
g + geom_line()

# 2. Caso de Estudio

#install.packages("MSwM")
library("MSwM")

ols_mod <- lm(crec ~ 1, data = datos_mod)

ms_mod <- msmFit(ols_mod, k = 2, sw = c(TRUE, TRUE, TRUE), p = 1)

summary(ms_mod)