# 1. Simulacion proceso AR(1) - GARCH(1, 1)

set.seed(12345)
u <- rep(0, 500)
u[1] <- rnorm(1)
v <- rnorm(500)
h <- rep(0, 500)
h[1] <- 1
y <- rep(0, 500)
y[1] <- 1
for (i in 2:500) {
        h[i] <- 1.3 + 0.3 * h[i - 1] + 0.4 * u[i - 1]^2
        u[i] <- v[i] * sqrt(h[i])
        y[i] <- 0.75 * y[i - 1] + u[i]
}

# 2. Evaluacion de los residuos al cuadrado
ar1_mod <- arima(y[401:500], order = c(1, 0, 0), method = "CSS", include.mean = FALSE)
acf(ar1_mod$residuals^2, lag.max = 20, main = "ACF u^2_t Modelo AR(1)", xlim = c(1, 20))
pacf(ar1_mod$residuals^2, lag.max = 20, main = "PACF u^2_t Modelo AR(1)", xlim = c(1, 20))

# 3. Estimación AR(1) - GARCH(1, 1)
set.seed(12345)
u <- rep(0, 500)
u[1] <- rnorm(1); v <- rnorm(500); h <- rep(0, 500)
h[1] <- 1; y <- rep(0, 500); y[1] <- 1
for (i in 2:500) {
        h[i] <- 1.1 + 0.2 * h[i - 1] + 0.7 * u[i - 1]^2
        u[i] <- v[i] * sqrt(h[i])
        y[i] <- 0.55 * y[i - 1] + u[i]
}

# install.packages("rugarch")

library(rugarch)

mod_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                       mean.model = list(armaOrder = c(1, 0), include.mean = FALSE), distribution.model = "norm")
argarch_mod <- ugarchfit(spec = mod_spec, data = y[401:500], fit.control = list(stationarity = 1))
show(argarch_mod)

mod_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                       mean.model = list(armaOrder = c(1, 0), include.mean = FALSE), distribution.model = "std")
argarch_mod <- ugarchfit(spec = mod_spec, data = y[401:500], fit.control = list(stationarity = 1))
show(argarch_mod)

# 4. Diagnóstico

acf(argarch_mod@fit$residuals, lag.max = 20, main = "ACF residuos AR(1)", xlim = c(1, 20))

stand_res <- argarch_mod@fit$residuals / argarch_mod@fit$sigma
acf(stand_res, lag.max = 20, main = "ACF de los residuos estandarizados", xlim = c(1, 20))

# 5. Pronóstico varianza condicional AR(1) - GARCH(1, 1)
set.seed(12345)
u <- rep(0, 500)
u[1] <- rnorm(1); h <- rep(0, 500); h[1] <- 1
v <- rnorm(500); y <- rep(0, 500); y[1] <- 1
for (i in 2:500) {
        h[i] <- 0.6 + 0.3 * h[i - 1] + 0.6 * u[i - 1]^2
        u[i] <- v[i] * sqrt(h[i])
        y[i] <- 1.2 + 0.4 * y[i - 1] + u[i]
}

mod_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                       mean.model = list(armaOrder = c(1, 0), include.mean = TRUE), distribution.model = "norm")
argarch_mod <- ugarchfit(spec = mod_spec, data = y[401:500], fit.control = list(stationarity = 1))

argarch_for <- ugarchforecast(argarch_mod, n.ahead = 50)
argarch_for

# 6. Caso de Estudio S&P500

sp_datos <- read.csv("./Datos/SP.csv")
tail(sp_datos)

n <- nrow(sp_datos)
ret_sp <- sp_datos$close[2:n] / sp_datos$close[1:(n - 1)] - 1
sp_datos$ret <- c(NA, ret_sp)

sp_datos$date <- as.Date(as.character(sp_datos$date), "%Y-%m-%d")

g <- ggplot(data = sp_datos[2:n,], aes(x = date, y = ret))
g + geom_line() + labs(x = "Fecha", y = "Retorno Diario")

acf(sp_datos$ret[2:(n - 4)], lag.max = 20, main = "ACF Retornos Diaros S&P500", xlim = c(1, 20))
pacf(sp_datos$ret[2:(n - 4)], lag.max = 20, main = "PACF Retornos Diarios S&P500", xlim = c(1, 20))

for (i in c(0, 1, 3, 5)) {
        for (j in c(0, 5)) {
                arma_mod <- arima(sp_datos$ret[2:(n - 4)], order = c(i, 0, j), method = "ML", include.mean = TRUE)
                AIC_val <- AIC(arma_mod)
                BIC_val <- BIC(arma_mod)
                res <- paste("Para el modelo ARMA(", i, ",", j, ") tenemos un AIC de ", AIC_val, 
                             " y un BIC de", BIC_val, sep = "")
                print(res)
        }
}

ar1_mod <- arima(sp_datos$ret[2:(n - 4)], order = c(5, 0, 0), method = "ML", include.mean = TRUE)
acf(ar1_mod$residuals^2, lag.max = 20, main = "ACF Residuos al cuadrado", xlim = c(1, 20))
pacf(ar1_mod$residuals^2, lag.max = 20, main = "PACF Residuos al cuadrado", xlim = c(1, 20))

garch_spec <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), 
                         mean.model = list(armaOrder = c(5, 0), include.mean = TRUE), distribution.model = "std")
garch_mod <- ugarchfit(spec = garch_spec, data = sp_datos$ret[2:(n - 4)], fit.control = list(stationarity = 1))
show(garch_mod)

acf(garch_mod@fit$residuals, lag.max = 20, main = "ACF residuos GARCH(1, 1)", xlim = c(1, 20))

for (i in c(5, 10, 15)) {
        port_test <- Box.test(garch_mod@fit$residuals, lag = i, type = "Ljung-Box")
        res <- paste("Para los ", i, "primeros rezagos el p.value es ", port_test$p.value)
        print(res)
}

stan_res1 <- garch_mod@fit$residuals / garch_mod@fit$sigma
acf(stan_res1, lag.max = 20, main = "ACF Residuos Estandarizados GARCH(1, 1)", xlim = c(1, 20))

for (i in c(5, 10, 15, 20)) {
        port_test <- Box.test(stan_res1, lag = i, type = "Ljung-Box")
        res <- paste("Para los ", i, "primeros rezagos el p.value es ", port_test$p.value)
        print(res)
}

garch_for <- ugarchforecast(garch_mod, n.ahead = 5)

eval_for <- data.frame(Fecha = sp_datos$date[(n - 4):n], ret_obs = sp_datos$ret[(n - 4):n],
                       Ret_for1 = garch_for@forecast$seriesFor, 
                       Vol_for1 = garch_for@forecast$sigmaFor)

colnames(eval_for)[3:4] <- c("Ret_for1", "Vol_for1")
eval_for
