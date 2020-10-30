# 1. Simulación proceso camino aleatorio con constante
set.seed(12345)
epsilon <- rnorm(500, mean = 0, sd = 2); y <- rep(0, 500); y[1] <- 1
for (i in 2:500) {
        y[i] <- 1.2 + y[i - 1] + epsilon[i]
}

datos <- data.frame(t = 1:100, y = y[401:500])

library(ggplot2)

g <- ggplot(data = datos, aes(x = t, y = y))
g + geom_line()

library(readxl)

pib <- read_xlsx("./Datos/PIB2.xlsx", sheet = 1, range = "B11:AG11", col_names = FALSE)

Fecha <- seq(as.Date("01-01-1988", "%d-%m-%Y"), as.Date("01-01-2019", "%d-%m-%Y"), by = "year")

datos_pib <- data.frame(Fecha = Fecha, PIB = t(pib))

g <- ggplot(data = datos_pib, aes(x = Fecha, y = log(PIB)))
g + geom_line()

#2. Proceso I(1) en diferencias

datos$dy <- c(NA, diff(datos$y))
g <- ggplot(data = datos[2:100, ], aes(x = t, y = dy))
g + geom_line()

#3. Implicancias I(1)

#3.1 Pronósticos

set.seed(12345)
epsilon <- rnorm(500, mean = 0, sd = 1.5); u <- rep(0, 500); t <- 1:100; y <- rep(0, 100); u[1] <- 1
for (i in 2:500) {
        u[i] <- 0.6 * u[i - 1] + epsilon[i]
}
for (i in 1:100) {
        y[i] <- 1.2 + 0.8 * t[i] + u[400 + i]
}

datos <- data.frame(t = t, y = y)

g <- ggplot(data = datos, aes(x = t, y = y))
g + geom_line()

ar1_mod <- arima(datos$y, order = c(1, 0, 0), xreg = datos$t)
ar1_mod

ar1_for <- predict(ar1_mod, n.ahead = 50, newxreg = 101:150)

tend <- 1.2 + 0.8 * (101:150)

datos_for <- data.frame(t = 101:150, tend = tend, pred = ar1_for$pred)

g <- ggplot(data = datos_for, aes(x = t))
g + geom_line(aes(y = pred)) + geom_line(aes(y = tend), colour = "red")

set.seed(12345)
epsilon <- rnorm(500, mean = 0, sd = 1.5); u <- rep(0, 500); t <- 1:100; y <- rep(0, 100); u[1] <- 1
for (i in 2:500) {
        u[i] <- u[i - 1] + epsilon[i]
}
for (i in 1:100) {
        y[i] <- 1.2 + 0.8 * t[i] + u[400 + i]
}

datos <- data.frame(t = t, y = y)

g <- ggplot(data = datos, aes(x = t, y = y))
g + geom_line()

ur1_mod <- arima(datos$y, order = c(1, 1, 0), xreg = datos$t)
ur1_mod

ur1_for <- predict(ur1_mod, n.ahead = 50, newxreg = 101:150)

tend <- 1.2 + 0.8 * (101:150)

datos_for <- data.frame(t = 101:150, tend = tend, pred = ur1_for$pred)

g <- ggplot(data = datos_for, aes(x = t))
g + geom_line(aes(y = pred)) + geom_line(aes(y = tend), colour = "red")

#3.2 Implicancia varianza de los pronosticos

datos_vol <- data.frame(t = 1:50, vol_ar1 = ar1_for$se, vol_ur1 = ur1_for$se)

g <- ggplot(data = datos_vol, aes(x = t))
g + geom_line(aes(y = vol_ar1)) + geom_line(aes(y = vol_ur1), colour = "red")

# 4. Pruebas de raiz unitaria

#install.packages("urca")
library(urca)
adf_test <- ur.df(log(datos_pib$PIB), type = c("drift"), selectlags = c("BIC"))

summary(adf_test)

plot(adf_test)

adf_gls <- ur.ers(log(datos_pib$PIB), type = "DF-GLS", model = "constant", lag.max = 1)
summary(adf_gls)

plot(adf_gls)

kpss_test <-ur.kpss(log(datos_pib$PIB), type = "tau", use.lag = 1)
summary(kpss_test)

plot(kpss_test)

pp_test <- ur.pp(log(datos_pib$PIB), type = "Z-tau", model = "constant", use.lag = 1)
summary(pp_test)
plot(pp_test)

# 5. Caso de Estudio

if (!file.exists("./Datos/tcr.xls")) {
        url <- "http://fhayashi.fc2web.com/hayashi%20econometrics/ch9/lt.xls"
        download.file(url, destfile = "./Datos/tcr.xls", mode = "wb")
}

library(readxl)

datos <- read_xls("./Datos/tcr.xls", sheet = 1, range = "A1:D201")

View(datos)

datos$Fecha <- seq(as.Date("01-01-1791", "%d-%m-%Y"), as.Date("01-01-1990", "%d-%m-%Y"), by = "year")

datos$z <- log(datos$S) - log(datos$USWPI) + log(datos$UKWPI)

g <- ggplot(data = datos, aes(x = Fecha, y = z))
g + geom_line()

adf_test <- ur.df(datos$z, type = c("drift"), selectlags = c("BIC"))
summary(adf_test)

plot(adf_test)

adf_gls <- ur.ers(datos$z, type = "DF-GLS", model = "constant", lag.max = 1)
summary(adf_test)

kpss_test <-ur.kpss(datos$z, type = "tau", use.lag = 1) 
summary(kpss_test)

pp_test <- ur.pp(datos$z, type = "Z-tau", model = "constant", use.lag = 1)
summary(pp_test)