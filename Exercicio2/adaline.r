rm(list=ls())

library("corpcor")
library("scatterplot3d")

s1 <- 0.4
s2 <- 0.4
num_amostras <- 180
xc1 <- matrix(rnorm(num_amostras*2), ncol = 2) * s1 + matrix(c(2,2), nrow = num_amostras, ncol = 2)
xc2 <- matrix(rnorm(num_amostras*2), ncol = 2) * s2 + matrix(c(4,4), nrow = num_amostras, ncol = 2);
xc3 <- matrix(rnorm(num_amostras*2), ncol = 2) * s1 + matrix(c(4,2), nrow = num_amostras, ncol = 2)
xc4 <- matrix(rnorm(num_amostras*2), ncol = 2) * s2 + matrix(c(2,4), nrow = num_amostras, ncol = 2)

# Rótulos das classes
yc1 <- matrix(1, nrow = num_amostras, ncol = 1)
yc2 <- matrix(-1, nrow = num_amostras, ncol = 1)
yc3 <- matrix(1, nrow = num_amostras, ncol = 1)
yc4 <- matrix(-1, nrow = num_amostras, ncol = 1)

plot(xc1[,1], xc1[,2], col = "red", xlim = c(0,8), ylim = c(0,8))
par(new = TRUE)
plot(xc2[,1], xc2[,2], col = "blue", xlim = c(0,8), ylim = c(0,8))
par(new = TRUE)
plot(xc3[,1], xc3[,2], col = "red", xlim = c(0,8), ylim = c(0,8))
par(new = TRUE)
plot(xc4[,1], xc4[,2], col = "blue", xlim = c(0,8), ylim = c(0,8))

X <- rbind(xc1, xc2, xc3, xc4)
Y <- rbind(yc1, yc2, yc3, yc4)
X <- cbind(X, 1)
X <- cbind(X, 1)

W <- pseudoinverse(X) %*% Y

yhat1 = X[1, 1] * W[1] + X[1, 2] * W[2] + X[1, 3] * W[3]
yhat40 = X[40, 1] * W[1] + X[40, 2] * W[2] + X[40, 3] * W[3]

yhat1 = sign(X[1, 1] * W[1] + X[1, 2] * W[2] + X[1, 3] * W[3])
yhat40 = sign(X[40, 1] * W[1] + X[40, 2] * W[2] + X[40, 3] * W[3])

seqi <- seq(0, 8, 0.5)
seqj <- seq(0, 8, 0.5)

for(i in seqi) {
  for(j in seqj) {
    par(new = T)
    plot(i, j, col = "green", pch = 2, xlim = c(0, 8), ylim = c(0, 8))
  }
}

seqi <- seq(0, 8, 0.5)
seqj <- seq(0, 8, 0.5)
M <- matrix(0, nrow = length(seqi), ncol = length(seqj))

ci <- 0
for(i in seqi) {
  ci <- ci + 1
  cj <- 0
  for(j in seqj) {
    cj <- cj + 1
    xg <- c(i, j, 1)
    M[ci, cj] <- sign(xg %*% W)
  }
}

plot(xc1[,1], xc1[,2], col = "red", xlim = c(0,8), ylim = c(0,8))
par(new = TRUE)
plot(xc2[,1], xc2[,2], col = "blue", xlim = c(0,8), ylim = c(0,8))
par(new = TRUE)
plot(xc3[,1], xc3[,2], col = "red", xlim = c(0,8), ylim = c(0,8))
par(new = TRUE)
plot(xc4[,1], xc4[,2], col = "blue", xlim = c(0,8), ylim = c(0,8))
par( new = TRUE )
contour(seqi, seqj, M, xlim = c(0, 8), ylim = c(0, 8), nlevels = 0)

# Regra Delta

eta = 0.01
tolerancia = 0.01
max_epocas = 1000

N <- dim(X)[1]
n <- dim(X)[2]

wt <- as.matrix(runif(n) - 0.5)

n_epocas <- 0
erro_epoca <- tolerancia + 1
erro_evec <- matrix(nrow = 1, ncol = max_epocas)

while(n_epocas < max_epocas && erro_epoca > tolerancia) {
  erro_i2 <- 0
  xseq <- sample(N)
  for(i in 1:N) {
    irand <- xseq[i]
    yhati <- 1.0*(X[irand,] %*% wt)
    erro_i <- Y[irand] - yhati
    gradiente <- eta * erro_i * X[irand,]
    
    wt <- wt + gradiente
    
    erro_i2 <- erro_i2 + erro_i * erro_i
  }
  
  n_epocas <- n_epocas + 1
  erro_evec[n_epocas] <- erro_i2 / N
  erro_epoca = erro_evec[n_epocas]
}

1.
plot(erro_evec[1,], type = 'l', xlab = "Época")

seqi <- seq(0, 8, 0.5)
seqj <- seq(0, 8, 0.5)
Md <- matrix(0, nrow = length(seqi), ncol = length(seqj))

ci <- 0
for(i in seqi) {
  ci <- ci + 1
  cj <- 0
  for(j in seqj) {
    cj <- cj + 1
    xg <- c(i, j, 1)
    Md[ci, cj] <- sign(xg %*% wt)
  }
}

plot(xc1[,1], xc1[,2], col = "red", xlim = c(0,8), ylim = c(0,8))
par(new = TRUE)
plot(xc2[,1], xc2[,2], col = "blue", xlim = c(0,8), ylim = c(0,8))
par(new = TRUE)
plot(xc3[,1], xc3[,2], col = "red", xlim = c(0,8), ylim = c(0,8))
par(new = TRUE)
plot(xc4[,1], xc4[,2], col = "blue", xlim = c(0,8), ylim = c(0,8))
par( new = TRUE )
contour(seqi, seqj, M, col = 'green', xlim = c(0, 8), ylim = c(0, 8), nlevels = 0)
par(new = T)
contour(seqi, seqj, Md, col = 'pink', xlim = c(0, 8), ylim = c(0, 8), nlevels = 0)

# Perceptron

step_function <- function(x) {
  ifelse(x >= 0, 1, -1)
}

eta = 0.01
tolerancia = 0.01
max_epocas = 1000

N <- dim(X)[1]
n <- dim(X)[2]

wt <- as.matrix(runif(n) - 0.5)

n_epocas <- 0
erro_epoca <- tolerancia + 1

erro_evec <- matrix(nrow = 1, ncol = max_epocas)

while(n_epocas < max_epocas && erro_epoca > tolerancia) {
  erro_i2 <- 0
  xseq <- sample(N)
  for(i in 1:N) {
    irand <- xseq[i]
    yhati <- step_function((X[irand,] %*% wt))
    erro_i <- Y[irand] - yhati
    gradiente <- eta * erro_i * X[irand,]
    wt <- wt + gradiente
    erro_i2 <- erro_i2 + erro_i * erro_i
  }
  n_epocas <- n_epocas + 1
  erro_evec[n_epocas] <- erro_i2 / N
  erro_epoca = erro_evec[n_epocas]
}
1.
plot(erro_evec[1,], type = 'l', xlab = "Época")

seqi <- seq(0, 8, 0.5)
seqj <- seq(0, 8, 0.5)
Md <- matrix(0, nrow = length(seqi), ncol = length(seqj))

ci <- 0
for(i in seqi) {
  ci <- ci + 1
  cj <- 0
  for(j in seqj) {
    cj <- cj + 1
    xg <- c(i, j, 1)
    Md[ci, cj] <- sign(xg %*% wt)
  }
}

plot(xc1[,1], xc1[,2], col = "red", xlim = c(0,8), ylim = c(0,8))
par(new = TRUE)
plot(xc2[,1], xc2[,2], col = "blue", xlim = c(0,8), ylim = c(0,8))
par(new = TRUE)
plot(xc3[,1], xc3[,2], col = "red", xlim = c(0,8), ylim = c(0,8))
par(new = TRUE)
plot(xc4[,1], xc4[,2], col = "blue", xlim = c(0,8), ylim = c(0,8))
par( new = TRUE )
contour(seqi, seqj, M, col = "green", xlim = c(0, 8), ylim = c(0, 8), nlevels = 0)
par(new = T)
contour(seqi, seqj, Md, col = "pink", xlim = c(0, 8), ylim = c(0, 8), nlevels = 0)