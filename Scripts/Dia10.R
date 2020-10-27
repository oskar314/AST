# 1. Verificación estacionaridad VAR(1)

library(MASS)
# install.packages("vars")
library(vars)

set.seed(12345)
Sigma <- matrix(c(1, 0.3, 0.3, 1), nrow = 2, ncol = 2, byrow = TRUE)
epsilon <- mvrnorm(500, mu = c(0, 0), Sigma = Sigma)
y <- matrix(rep(0, 1000), ncol = 2, nrow = 500)
y[1,] <- c(1, 1)
for (i in 2:500) {
        y[i, 1] <- 0.5 * y[(i - 1), 1] + 0.3 * y[(i - 1), 2] + epsilon[(i - 1), 1] 
        y[i, 2] <- 0.02 * y[(i - 1), 1] + 0.8 * y[(i - 1), 2] + epsilon[(i - 1), 2]
}

datos <- data.frame(t = 1:100, y1 = y[401:500, 1], y2 = y[401:500, 2])
library(ggplot2)
g <- ggplot(data = datos, aes(x = t))
g + geom_line(aes(y = y1)) + geom_line(aes(y = y2), colour = "red")

var_mod <- VAR(y = datos[, 2:3], p = 1, type = "none")
summary(var_mod)

roots(var_mod)

# 2. Ley de grandes numeros procesos VAR

set.seed(12345)
Sigma <- matrix(c(1, 0.5, 0.5, 1), nrow = 2, ncol = 2, byrow = TRUE)
epsilon <- mvrnorm(500, mu = c(0, 0), Sigma = Sigma)
y <- matrix(rep(0, 1000), nrow = 500, ncol = 2)
y[1, ] <- c(1, 1)
for (i in 2:500) {
        y[i, 1] <- -0.7 + 0.7 * y[(i - 1), 1] + 0.2 * y[(i - 1), 2] + epsilon[i, 1]
        y[i, 2] <- 1.3 + 0.2 * y[(i - 1), 1] + 0.7 * y[(i - 1), 2] + epsilon[i, 2]
}

Phi <- matrix(c(0.7, 0.2, 0.2, 0.7), nrow = 2, ncol = 2, byrow = TRUE)
const <- matrix(c(-0.7, 1.3), nrow = 2, ncol = 1, byrow = TRUE)
solve(diag(2) - Phi) %*% const

colMeans(y[401:500, ])

# 3. Test de ratio de verosimilitud
set.seed(12345)
Sigma <- matrix(c(1, 0.5, 0.5, 1), nrow = 2, ncol = 2, byrow = TRUE)
epsilon <- mvrnorm(500, mu = c(0, 0), Sigma = Sigma)
y <- matrix(rep(0, 1000), nrow = 500, ncol = 2)
y[1, ] <- c(1, 1)
for (i in 2:500) {
        y[i, 1] <- 1.3 + 0.3 * y[(i - 1), 1] + 0.5 * y[(i - 1), 2] + epsilon[i, 1]
        y[i, 2] <- 0.4 + 0.5 * y[(i - 1), 1] + 0.3 * y[(i - 1), 2] + epsilon[i, 2]
}

datos <- data.frame(y1 = y[401:500, 1], y2 = y[401:500, 2])

var2_mod <- VAR(y = datos, p = 2, type = "const")
var1_mod <- VAR(y = datos, p = 1, type = "const")

l_test <- (100 - 5) * (logLik(var2_mod) - logLik(var1_mod))

pchisq(l_test, df = 2^2 * (2 - 1), lower.tail = FALSE)

# 4. Proyeccion VAR(1)

set.seed(12345)
Sigma <- matrix(c(1, 0.3, 0.3, 1), nrow = 2, ncol = 2, byrow = TRUE)
epsilon <- mvrnorm(500, mu = c(0, 0), Sigma = Sigma)
y <- matrix(rep(0, 1000), nrow = 500, ncol = 2)
y[1, ] <- c(1, 1)
for (i in 2:500) {
        y[i, 1] <- 2.4 + 0.2 * y[(i - 1), 1] + 0.1 * y[(i - 1), 2] + epsilon[i, 1]
        y[i, 2] <- 0.5 + 0.1 * y[(i - 1), 1] + 0.2 * y[(i - 1), 2] + epsilon[i, 2]
}

datos <- data.frame(y1 = y[401:500, 1], y2 = y[401:500, 2])

var1_mod <- VAR(datos, p = 1, type = "const")

var1_for <- predict(var1_mod, n.ahead = 50)

View(var1_for$fcst$y1)
View(var1_for$fcst$y2)

# 5. Causalidad de Granger

set.seed(12345)
Sigma <- matrix(c(1, 0.4, 0.4, 1), nrow = 2, ncol = 2, byrow = TRUE)
epsilon <- mvrnorm(500, mu = c(0, 0), Sigma = Sigma)
y <- matrix(rep(0, 1000), nrow = 500, ncol = 2)
y[1, ] <- c(1, 1)
for (i in 2:500) {
        y[i, 1] <- 1.3 + 0.6 * y[(i - 1), 1] + epsilon[i, 1]
        y[i, 2] <- 0.4 + 0.1 * y[(i - 1), 1] + 0.6 * y[(i - 1), 2] + epsilon[i, 2]
}

datos <- data.frame(y1 = y[401:500, 1], y2 = y[401:500, 2])

var1_mod <- VAR(y = datos, p = 1, type = "const")

caus_test <- causality(var1_mod, cause = "y2")
caus_test$Granger

# 6. Funcion de Impulso Respuesta

set.seed(12345)
Sigma <- matrix(c(1, 0.3, 0.3, 1), nrow = 2, ncol = 2, byrow = TRUE)
epsilon <- mvrnorm(500, mu = c(0, 0), Sigma = Sigma)
y <- matrix(rep(0, 1000), nrow = 500, ncol = 2)
y[1, ] <- c(1, 1)
for (i in 2:500) {
        y[i, 1] <- 2.4 + 0.2 * y[(i - 1), 1] + 0.1 * y[(i - 1), 2] + epsilon[i, 1]
        y[i, 2] <- 0.5 + 0.1 * y[(i - 1), 1] + 0.2 * y[(i - 1), 2] + epsilon[i, 2]
}

datos <- data.frame(y1 = y[401:500, 1], y2 = y[401:500, 2])

var1_mod <- VAR(y = datos, p = 1, type = "const")

var1_irf <- irf(var1_mod, impulse = c("y1", "y2"), response = "y1", n.ahead = 20, ortho = TRUE , boot = TRUE)
plot(var1_irf)

# 7. Criterios de Seleccion

set.seed(12345)
Sigma <- matrix(c(1, 0.4, 0.4, 1), nrow = 2, ncol = 2, byrow = TRUE)
epsilon <- mvrnorm(500, mu = c(0, 0), Sigma = Sigma)
y <- matrix(rep(0, 1000), nrow = 500, ncol = 2)
y[1, ] <- c(1, 1)
for (i in 2:500) {
        y[i, 1] <- 2.5 + 0.4 * y[(i - 1), 1] + 0.2 * y[(i - 1), 2] + epsilon[i, 1]
        y[i, 2] <- 1.3 + 0.2 * y[(i - 1), 1] + 0.4 * y[(i - 1), 2] + epsilon[i, 2]
}

datos <- data.frame(y1 = y[401:500, 1], y2 = y[401:500, 2])

crit_res <- VARselect(y = datos, lag.max = 10, type = "const")
View(crit_res$criteria)

# 8. Diagnostico

var1_mod <- VAR(y = datos, p = 1, type = "const")

for (i in c(3, 5, 10, 15, 20)) {
        ser_test <- serial.test(var1_mod, lags.pt = i, type = "PT.adjusted")
        res <- paste("Rezago ", i, " con p.value de ", ser_test$serial$p.value, sep = "")
        print(res)
}

