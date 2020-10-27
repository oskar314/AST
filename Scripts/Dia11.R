# Caso de Estudio VAR(p) monetario Estados Unidos

datos <- read.csv("./Datos/VAR data.csv")
colnames(datos) <- c("Fecha", "Infl", "UR", "IntR")

View(datos)

datos$Fecha <- as.Date(datos$Fecha, "%m/%d/%Y")

library(ggplot2)
g <- ggplot(data = datos, aes(x = Fecha, y = UR))
g + geom_line()

g <- ggplot(data = datos, aes(x = Fecha, y = Infl))
g + geom_line()

g <- ggplot(data = datos, aes(x = Fecha, y = IntR))
g + geom_line()

library(vars)
n <- nrow(datos)
y <- datos[1:(n - 3), 2:4]
crit_var <- VARselect(y = y, lag.max = 10, type = "const")
View(crit_var$criteria)

var6_mod <- VAR(y = y, p = 6, type = "const")
roots(var6_mod)

for (i in c(10, 15, 20)) {
        ser_test <- serial.test(var6_mod, lags.pt = i, type = "PT.adjusted")
        res <- paste("Rezago ", i, " con p.value de ", ser_test$serial$p.value, sep = "")
        print(res)
}

var6_irf <- irf(var6_mod, impulse = c("Infl", "IntR", "UR"), response = "Infl", n.ahead = 50, ortho = TRUE , boot = TRUE)
plot(var6_irf)

caus_test <- causality(var6_mod, cause = c("IntR", "UR"))
caus_test$Granger

var6_for <- predict(var6_mod, n.ahead = 3)
View(var6_for$fcst$Infl)

eval_for <- data.frame(Fecha = as.Date(c("2006-31-03", "2006-30-06", "2006-30-09"), "%Y-%d-%m"), 
                       Infl_Obs = datos[(n - 2):n, "Infl"], Infl_For = var6_for$fcst$Infl[, 1], 
                       Infl_ForL = var6_for$fcst$Infl[, 2], Infl_ForU = var6_for$fcst$Infl[, 3])
View(eval_for)

rmse <- sqrt(mean((eval_for$Infl_Obs - eval_for$Infl_For)^2))
rmse

# 2. Filtro de Kalman

FiltroKalman <- function(y, x, F, A, H, Q, R, E_ini, P_ini) {
        T <- length(y)
        E_u <- rep(0, T)
        E_f <- rep(0, (T + 1)) 
        E_f[1] <- E_ini
        P_u <- rep(0, T)
        P_f <- rep(0, (T + 1))
        P_f[1] <- P_ini
        for (i in 1:T) {
                # 1. Actualizar pronósticos
                E_u[i] <- E_f[i] + P_f[i] * H * (H^2 * P_f[i] + R)^(-1) * (y[i] - A * x[i] - H * E_f[i])
                P_u[i] <- P_f[i] - P_f[i] * H * (H^2 * P_f[i] + R)^(-1) * H * P_f[i]
                # 2. Realizar los pronósticos
                E_f[i + 1] <- F * E_u[i]
                P_f[i + 1] <- F^2 * P_u[i] + Q
        }
        results <- list(E_u = E_u, P_u = P_u, E_f = E_f, P_f = P_f)    
        return(results)
}

set.seed(12345)
v <- rnorm(100, mean = 0, sd = sqrt(2))
E_sim <- rep(0, 100)
for (i in 2:100) {
        E_sim[i] <- 0.7 * E_sim[i - 1] + v[i]
}

w <- rnorm(100, mean = 0, sd = sqrt(3))
y <- rep(0, 100)
for (i in 1:100) {
        y[i] <- 1.3 * E_sim[i] + w[i]
}

# 3. Estimación de la variable estado por el Filtro de Kalman
F <- 0.7; Q = 2; A = 0; x <- rep(0, 100); H <- 1.3; R <- 3; E_ini = 0; P_ini = 100
res <- FiltroKalman(y, x, F, A, H, Q, R, E_ini, P_ini)

datos <- data.frame(t = 1:100, E_sim = E_sim, E_est = res$E_u)

g <- ggplot(data = datos, aes(x = t))
g + geom_line(aes(y = E_sim)) + geom_line(aes(y = E_est), colour = "red")

# 4. Estimacion Maxima Verosimilitud

set.seed(12345)
v <- rnorm(100, mean = 0, sd = 1); E_sim <- rep(0, 100)
for (i in 2:100) {
        E_sim[i] <- 0.6 * E_sim[i - 1] + v[i]
}

w <- rnorm(100, mean = 0, sd = 1); y <- rep(0, 100)
for (i in 1:100) {
        y[i] <- 0.4 * E_sim[i] + w[i]
}

#install.packages("dlm")
library(dlm)

fun_dlm <- function(param) {
        FF <- param[1]; GG <- param[2]; V <- exp(param[3]); W <- exp(param[4])
        dlm(FF = FF, GG = GG, V = V, W = W, m0 = 0, C0 = 1)
} 

MLE_mod <- dlmMLE(y = y, parm = c(1, 0.5, 1, 1), build = fun_dlm, hessian = TRUE)

par_est <- MLE_mod$par
par_est

Kalman_res <- dlmFilter(y, mod = dlm(FF = par_est[1], GG = par_est[2], V = exp(par_est[3]), W = exp(par_est[4]), 
                                     m0 = 0, C0 = 1))

datos <- data.frame(t = 1:100, E_sim = E_sim, E_est = Kalman_res$a)

g <- ggplot(data = datos, aes(x = t))
g + geom_line(aes(y = E_sim)) + geom_line(aes(y = E_est), colour = "red")

# 4. Suavizamiento

set.seed(12345)
v <- rnorm(100, mean = 0, sd = sqrt(2)); E_sim <- rep(0, 100)
for (i in 2:100) {
        E_sim[i] <- 0.8 * E_sim[i - 1] + v[i]
}

w <- rnorm(100, mean = 0, sd = 2); y <- rep(0, 100)
for (i in 1:100) {
        y[i] <- 1.3 * E_sim[i] + w[i]
}

Kalman_mod <- dlm(FF = 1.3, GG = 0.8, V = 4, W = 2, m0 = 0, C0 = 10)

Kalman_smooth <- dlmSmooth(y, mod = Kalman_mod)
Kalman_est <- dlmFilter(y, mod = Kalman_mod)

datos <- data.frame(t = 1:100, E_sim = E_sim, E_s = Kalman_smooth$s[2:101], E_est = Kalman_est$a)

g <- ggplot(data = datos, aes(x = t))
g + geom_line(aes(y = E_sim)) + geom_line(aes(y = E_s), colour = "red") + geom_line(aes(y = E_est), colour = "blue")

# 5. Caso de Estudio Propension Marginal a Consumir

if (!file.exists("./Datos/PIB2.xlsx")) {
        url <- "https://www.ine.gob.bo/index.php/descarga/492/pib-segun-tipo-de-gasto/46189/bolivia-producto-interno-bruto-a-precios-constantes-por-tipo-de-gasto-1988-2019.xlsx"
        download.file(url, destfile = "./Datos/PIB2.xlsx", mode = "wb")
}

library(readxl)

pib <- read_xlsx("./Datos/PIB2.xlsx", sheet = 1, range = "B11:AG11", col_names = FALSE)
cons <- read_xlsx("./Datos/PIB2.xlsx", sheet = 1, range = "B13:AG13", col_names = FALSE)

Fecha <- seq(as.Date("01-01-1988", "%d-%m-%Y"), as.Date("01-01-2019", "%d-%m-%Y"), by = "year")

datos <- data.frame(Fecha = Fecha, C = t(cons), Y = t(pib), const = rep(1, length(Fecha)))

g <- ggplot(data = datos, aes(x = Fecha))
g + geom_line(aes(y = Y)) + geom_line(aes(y = C), colour = "blue")

dlm_fun <- function(param, x) {
        FF <- 1; JFF <- 1; GG <- param[1]; V <- 0; W <- exp(param[2])
        dlm(FF = JFF, JFF = JFF, GG = GG, V = V, W = W, m0 = 0.1, C0 = 10, X = x)
}

MLE_mod <- dlmMLE(y = datos$C, parm = c(0.5, 1), build = dlm_fun, x = as.matrix(datos[, 3]), hessian = TRUE)

par_est <- MLE_mod$par
par_est

Kalman_mod <- dlm(FF = 1, JFF = 1, GG = par_est[1], V = 0, W = exp(par_est[2]), 
                  m0 = 0.1, C0 = 10, X = as.matrix(datos[, 3]))

Kalman_smooth <- dlmSmooth(datos$C, mod = Kalman_mod)

n <- length(Kalman_smooth$s)
datos$k <- Kalman_smooth$s[2:n]

g <- ggplot(data = datos, aes(x = Fecha, y = k))
g + geom_line()