# 1. Regresion Espurea

set.seed(12345)
epsilon <- rnorm(500); nu <- rnorm(500, mean = 0, sd = 1.5)
y <- rep(0, 500); x <- rep(0, 500)
y[1] <- 1; x[1] <- 500
for (i in 2:500) {
        y[i] <- 0.4 + y[i - 1] + epsilon[i]
        x[i] <- 0.6 + x[i - 1] + nu[i]
}

datos <- data.frame(y = y[401:500], x = x[401:500])

ols_mod <- lm(y ~ x, data = datos)

summary(ols_mod)

# 2. Pruebas de cointegracion

library(readxl)

pib <- read_xlsx("./Datos/PIB2.xlsx", sheet = 1, range = "B11:AG11", col_names = FALSE)
cons <- read_xlsx("./Datos/PIB2.xlsx", sheet = 1, range = "B13:AG13", col_names = FALSE)

Fecha <- seq(as.Date("01-01-1988", "%d-%m-%Y"), as.Date("01-01-2019", "%d-%m-%Y"), by = "year")

datos <- data.frame(Fecha = Fecha, C = t(log(cons)), Y = t(log(pib)))

library(ggplot2)

g <- ggplot(data = datos, aes(x = Fecha)) 
g + geom_line(aes(y = Y)) + geom_line(aes(y = C), colour = "blue")

library(urca)
adf_pib <- ur.df(datos$Y, type = c("drift"), selectlags = c("BIC"))

summary(adf_pib)

adf_c <- ur.df(datos$C, type = c("drift"), selectlags = c("BIC"))

summary(adf_c)

gls_pib <- ur.ers(datos$Y, type = "DF-GLS", model = "constant", lag.max = 1)

gls_c <- ur.ers(datos$C, type = "DF-GLS", model = "constant", lag.max = 1)

summary(gls_pib)
summary(gls_c)

ols_mod <- lm(C ~ Y, data = datos)

summary(ols_mod)

datos$res <- ols_mod$residuals

g <- ggplot(data = datos, aes(x = Fecha, y = res))
g + geom_line()

adf_res <- ur.df(datos$res, type = c("none"), selectlags = c("BIC"))

summary(adf_res)

coin_po <- ca.po(as.matrix(datos[,2:3]), demean = "none", lag = "short", type = "Pz")
summary(coin_po)

library(vars)

var_lag <- VARselect(y = as.matrix(datos[, 2:3]), lag.max = 10, type = "const")
View(var_lag$criteria)

coint_jo <- ca.jo(as.matrix(datos[, 2:3]), type = "eigen", ecdet = "none", K = 2, spec = "transitory")
summary(coint_jo)

coint_jo2 <- ca.jo(as.matrix(datos[, 2:3]), type = "trace", ecdet = "none", K = 2, spec = "transitory")
summary(coint_jo2)

# 4. Caso de Estudio

if (!file.exists("./Datos/tasas.xls")) {
        url <- "http://time-series.net/yahoo_site_admin/assets/docs/quarterly.7775706.xls"
        download.file(url, destfile = "./Datos/tasas.xls", mode = "wb")
}

library(readxl)

datos <- read_xls("./Datos/tasas.xls", sheet = 1, range = "A1:D213")

View(datos)

datos$Fecha <- seq(as.Date("01-01-1960", "%d-%m-%Y"), as.Date("01-12-2012", "%d-%m-%Y"), by = "quarter")

g <- ggplot(data = datos, aes(x = Fecha))
g + geom_line(aes(y = Tb1yr)) + geom_line(aes(y = Tbill), colour = "blue")

n <- nrow(datos)
datos_mod <- datos[1:(n - 3), ]

adf_Tb1 <- ur.df(datos_mod$Tb1yr, type = c("none"), selectlags = c("BIC"))
summary(adf_Tb1)

gls_Tb1 <- ur.ers(datos_mod$Tb1yr, type = "DF-GLS", model = "constant", lag.max = 1)
summary(gls_Tb1)

adf_Tb <- ur.df(datos_mod$Tbill, type = c("none"), selectlags = c("BIC"))
summary(adf_Tb)

gls_Tb <- ur.ers(datos_mod$Tbill, type = "DF-GLS", model = "constant", lag.max = 1)
summary(gls_Tb)

ols_mod <- lm(Tb1yr ~ Tbill, data = datos_mod)
summary(ols_mod)

datos_mod$res <- ols_mod$residuals

g <- ggplot(data = datos_mod, aes(x = Fecha, y = res))
g + geom_line()

adf_res <- ur.df(datos_mod$res, type = c("none"), selectlags = c("BIC"))
summary(adf_res)

po_test <- ca.po(as.matrix(datos_mod[, 3:4]), demean = "none", lag = "short", type = "Pz")
summary(po_test)

var_lag <- VARselect(y = as.matrix(datos_mod[, 3:4]), lag.max = 10, type = "const")
View(var_lag$criteria)

coint_jo <- ca.jo(as.matrix(datos_mod[, 3:4]), type = "eigen", ecdet = "none", K = 6, spec = "longrun")
summary(coint_jo)

vecm_mod <- cajorls(coint_jo, r = 1)

summary(vecm_mod$rlm)

library(tsDyn)

vecm_mod2 <- VECM(datos_mod[, 3:4], lag = 6, r = 1, include = "const", estim = "2OLS", LRinclude = "none")
vecm_for <- predict(vecm_mod2, n.ahead = 4)

datos_for <- datos[(n - 3):n, c("Fecha", "Tb1yr")]

datos_for$Tb1_for <- vecm_for[, 2]

View(datos_for)

rmse <- sqrt(mean((datos_for$Tb1yr - datos_for$Tb1_for)^2))
rmse