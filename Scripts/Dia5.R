# 1. RMSE ARMA(1, 1)

set.seed(12345)
epsilon <- rnorm(510)
y <- rep(0, 510)
y[1] <- 1
for (i in 2:510) {
        y[i] <- 1.2 + 0.5 * y[i - 1] + epsilon[i] + 0.8 * epsilon[i - 1]
}
arma11_mod <- arima(y[401:500], order = c(1, 0, 1), method = "ML")
h <- 10
arma11_for <- predict(arma11_mod, n.ahead = h)
RMSE_arma11 <- sqrt(mean((y[501:510] - as.numeric(arma11_for$pred))^2))
print(paste("El RMSE del modelo ARMA(1,1) es", RMSE_arma11))

# 2. Criterios de selección
AIC(arma11_mod) # Para el criterio de Akaike
BIC(arma11_mod) # Para el criterio de Schwartz

# 3. Caso de Estudio: Proyeccion Inflacion Mensual

url <- "https://nube.ine.gob.bo/index.php/s/UonLRDITXiVjHf0/download"
if (!file.exists("./Datos/IPC.xlsx")) {
        download.file(url, destfile = "./Datos/IPC.xlsx", mode = "wb")
}

library(readxl)
datos <- read_xlsx("./Datos/IPC.xlsx", sheet = 3, range = "BT5:CF17")
datos$Mes <- 1:12
tail(datos)

library(tidyr)

# Transformar los datos de formato wide a formato long

datos_long <- pivot_longer(datos, cols = !Mes, names_to = "Fecha", values_to = "IPC")
head(datos_long)

# Reordenar los datos
datos_n <- data.frame(Fecha = rep(0, nrow(datos_long)), IPC = rep(0, nrow(datos_long)))
fecha <- seq(as.Date("01/01/2007", "%d/%m/%Y"), as.Date("01/12/2019", "%d/%m/%Y"), by = "month")
datos_n$Fecha <- fecha
years <- c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)
for (i in 1:13) {
        datos_n[(1 + 12 * (i - 1)):(12 * i), "IPC"] <- datos_long[which(datos_long$Fecha == years[i]), "IPC"] 
}

# Descomposicion stl

infl <- ts(datos_n$IPC[1:144], start = c(2007, 1), end = c(2018, 12), frequency = 12)
infl_decomp <- stl(infl, s.window = 15, t.window = 15, s.degree = 1, t.degree = 1)
plot(infl_decomp)

# Estimacion componente tendencia
n <- nrow(datos_n)
datos_n$t <- 1:n
datos_n$t2 <- datos_n$t^2
datos_n$t3 <- datos_n$t^3
datos_n$t4 <- datos_n$t^4
m_mod <- lm(IPC ~ t + t2 + t3 + t4, data = datos_n[1:144,])
summary(m_mod)

# Graficar tendencia
datos_n$m <- c(predict(m_mod), rep(0, 12))
library(ggplot2)
g <- ggplot(data = datos_n[1:144,], aes(x = Fecha, y = m))
g + geom_line()

# Remover los datos y periodograma
datos_n$infl_nt <- datos_n$IPC - datos_n$m
library(TSA)
period_infl <- periodogram(datos_n$infl_nt[1:144])

period_datos <- data.frame(freq = period_infl$freq, spec = period_infl$spec)
head(period_datos, 30)

# Estimación componente estacional
omega_j <- c(0.028, 0.125, 0.167)
144 * omega_j

m <- ncol(datos_n)
for (i in 1:length(omega_j)) {
        datos_n[, m + i] <- cos(2*pi*omega_j[i]*datos_n$t)
}

colnames(datos_n)[9:11] <- paste("W", 1:3, sep ="")

m <- ncol(datos_n)
for (i in 1:length(omega_j)) {
        datos_n[, m + i] <- sin(2*pi*omega_j[i]*datos_n$t)
}

colnames(datos_n)[12:14] <- paste("Z", 1:3, sep = "")

s_mod <- lm(infl_nt ~ W1 + W2 + W3 + Z1 + Z2 + Z3, data = datos_n[1:144,])
summary(s_mod)

# Estimacion estacionalidad

datos_n$s <- c(predict(s_mod), rep(0, 12))
g <- ggplot(data = datos_n[1:144,], aes(x = Fecha, y = s))
g + geom_line()

# Remover componente estacional
datos_n$infl_ntns <- datos_n$infl_nt - datos_n$s

# Modelacion ARMA
acf(datos_n$infl_ntns[1:144], lag.max = 20, main = "ACF Inflación Mensual", xlim = c(1, 20))

pacf(datos_n$infl_ntns[1:144], lag.max = 20, main = "PACF Inflación Mensual", xlim = c(1, 20))

# Seleccion modelo ARMA idoneo
for (j in c(0, 1, 8)) {
        for (i in c(0, 1, 8, 13)) {
                arma_mod <- arima(datos_n$infl_ntns[1:144], order = c(i, 0, j), method = "ML")
                AIC_val <- AIC(arma_mod)
                BIC_val <- BIC(arma_mod)
                res_1 <- paste("ARMA(", i, ",", j, ") con un AIC de ", AIC_val, sep = "")
                res_2 <- paste("ARMA(", i, ",", j, ") con un BIC de ", BIC_val, sep = "")
                print(res_1)
                print(res_2)
        }
}

# Estimacion modelo AR(1) Inflacion mensual
ar1_mod <- arima(datos_n$infl_ntns[1:144], order = c(1, 0, 0), method = "CSS")

# Pronosticos
ar1_for <- predict(ar1_mod, n.ahead = 3)

datos_n$ar1_for <- c(rep(0, 144), ar1_for$pred, rep(0, 9))

s_for <- predict(s_mod, newdata = datos_n[145:147,])
m_for <- predict(m_mod, newdata = datos_n[145:147,])

datos_n$s_for <- c(rep(0, 144), s_for, rep(0, 9))
datos_n$m_for <- c(rep(0, 144), m_for, rep(0, 9))

datos_n$infl_for <- datos_n$ar1_for + datos_n$s_for + datos_n$m_for
datos_for <- data.frame(Fecha = datos_n$Fecha[145:147], Infl_Obs = datos_n$IPC[145:147], Infl_For = datos_n$infl_for[145:147])
View(datos_for)

rmse_infl <- sqrt(mean((datos_for$Infl_Obs - datos_for$Infl_For)^2))
rmse_infl

g <- ggplot(data = datos_for, aes(x = Fecha))
g + geom_line(aes(y = Infl_Obs)) + geom_line(aes(y = Infl_For), colour = "red")

# 4. Simulacion proceso AR(1) - ARCH(1)

set.seed(12345)
u_1 <- rnorm(500)
y_1 <- rep(0, 500)
y_1[1] <- 1
for (i in 2:500) {
        y_1[i] <- 0.85 * y_1[i - 1] + u_1[i] 
}

u_2 <- rep(0, 500)
u_2[1] <- rnorm(1)
v <- rnorm(500)
h <- rep(0, 500)
y_2 <- rep(0, 500)
y_2[1] <- 1
for (i in 2:500) {
        h[i] <- 1.5 + 0.4 * u_2[i - 1]^2
        u_2[i] <- v[i] * sqrt(h[i])
        y_2[i] <- 0.85 * y_2[i - 1] + u_2[i]
}

datos_sim <- data.frame(t = 1:100, y_1 = y_1[401:500], y_2 = y_2[401:500])
g <- ggplot(data = datos_sim, aes(x = t))
g + geom_line(aes(y = y_1)) + geom_line(aes(y = y_2), color = "red")

