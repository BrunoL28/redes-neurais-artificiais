rm(list = ls())
library('corpcor')

# Função Radial de Base Gaussiana

rbf <- function(x, center, s) {
  return(exp(-0.5 * (rowSums((x - center)^2) / s^2)))
}

# Geração de Dados

set.seed(1000)
s1 <- 0.4
s2 <- 0.4
s3 <- 0.4
s4 <- 0.4
nc <- 100

xc1 <- matrix(rnorm(nc * 2), ncol = 2) * s1 + t(matrix(c(2, 2), ncol = nc, nrow = 2))
xc2 <- matrix(rnorm(nc * 2), ncol = 2) * s2 + t(matrix(c(2, 6), ncol = nc, nrow = 2))
xc3 <- matrix(rnorm(nc * 2), ncol = 2) * s3 + t(matrix(c(6, 2), ncol = nc, nrow = 2))
xc4 <- matrix(rnorm(nc * 2), ncol = 2) * s4 + t(matrix(c(6, 6), ncol = nc, nrow = 2))

x <- rbind(xc1, xc2, xc3, xc4)
d1 <- dim(xc1)[1]
d2 <- dim(xc2)[1]
d3 <- dim(xc3)[1]
d4 <- dim(xc4)[1]

corDegrade1 <- colorRampPalette(c("red","orange"))
corDegrade2 <- colorRampPalette(c("cyan", "magenta"))

y <- c(rep(1, d1), rep(-1, d2), rep(-1, d3), rep(1, d4))

plot(x[which(y == 1), 1], x[which(y == 1), 2], col = corDegrade1(10), xlim = c(0, 8), ylim = c(0, 9), xlab = 'x1', ylab = 'x2')
par(new = T)
plot(x[which(y == -1), 1], x[which(y == -1), 2], col = corDegrade2(10), xlim = c(0, 8), ylim = c(0, 9), xlab = '', ylab = '')
legend('top', legend = ('Função de Base Radial Gaussiana R = 1.6'))

# 4 gaussianas

S <- 1.6
c1 = c(2, 2)
c2 = c(2, 6)
c3 = c(6, 2)
c4 = c(6, 6)

# Camada oculta calculada com a função rbf

h1 <- rbf(x, c1, S)
h2 <- rbf(x, c2, S)
h3 <- rbf(x, c3, S)
h4 <- rbf(x, c4, S)

H <- cbind(h1, h2, h3, h4, 1)
W <- pseudoinverse(H) %*% y

yhat <- H %*% W

# Plotando o hiperplano de separação

xgrid1 <- seq(1, 8, 0.2)
xgrid2 <- seq(1, 8, 0.2)

YHATG <- matrix(0, length(xgrid1), length(xgrid2))

i = 1
j = 1
k = 1

for (i in 1:length(xgrid1)) {
  for (j in 1:length(xgrid2)) {
    h1 <- rbf(t(as.matrix(c(xgrid1[i], xgrid2[j]))), c1, S)
    h2 <- rbf(t(as.matrix(c(xgrid1[i], xgrid2[j]))), c2, S)
    h3 <- rbf(t(as.matrix(c(xgrid1[i], xgrid2[j]))), c3, S)
    h4 <- rbf(t(as.matrix(c(xgrid1[i], xgrid2[j]))), c4, S)
    YHATG[i, j] = sign(t(as.matrix(c(h1, h2, h3, h4, 1))) %*% W)
    k = k + 1
  }
}

plot(x[which(y == 1), 1], x[which(y == 1), 2], col = corDegrade1(10), xlim = c(0, 8), ylim = c(0, 9), xlab = 'x1', ylab = 'x2')
par(new = T)
plot(x[which(y == -1), 1], x[which(y == -1), 2], col = corDegrade2(10), xlim = c(0, 8), ylim = c(0, 9), xlab = '', ylab = '')
par(new = T)
contour(xgrid1, xgrid2, YHATG, levels = 0, labels = '', col = 'black',  xlim = c(0, 8), ylim = c(0, 9))
legend('top', legend = ('Função de Base Radial Gaussiana R = 1.6'))

# Clustering

# Número de grupos em que o conjunto de dados será dividido para treinamento e teste
k <- 10

# Conjunto de teste, com 10% dos dados
test_size <- round(0.1 * nrow(x))

# Vetor de Acurácias
accuracies <- numeric(k)

# Validação Cruzada
for (fold in 1:k) {
  set.seed(fold)
  test_indices <- sample(1:nrow(x), test_size)
  train_indices <- setdiff(1:nrow(x), test_indices)
  
  x_train <- x[train_indices, ]
  y_train <- y[train_indices]
  x_test <- x[test_indices, ]
  y_test <- y[test_indices]
  
  # Treinando a rede e calculando os pesos de W
  S <- 1.6
  c1 = c(2, 2)
  c2 = c(2, 6)
  c3 = c(6, 2)
  c4 = c(6, 6)
  
  h1 <- rbf(x_train, c1, S)
  h2 <- rbf(x_train, c2, S)
  h3 <- rbf(x_train, c3, S)
  h4 <- rbf(x_train, c4, S)
  
  H <- cbind(h1, h2, h3, h4, 1)
  W <- pseudoinverse(H) %*% y_train
  
  # Fazendo previsões nos dados de teste
  h1_test <- rbf(x_test, c1, S)
  h2_test <- rbf(x_test, c2, S)
  h3_test <- rbf(x_test, c3, S)
  h4_test <- rbf(x_test, c4, S)
  
  H_test <- cbind(h1_test, h2_test, h3_test, h4_test, 1)
  Yhat_test <- sign(H_test %*% W)
  
  # Cálculo da acurácia
  # Comparamos as previsões com os dados verdadeiros para
  # Saber quais delas estão corretas
  
  accuracies[fold] <- sum(Yhat_test == y_test) / length(y_test)
}

# Calculamos a média e desvio padrão das acurácias

mean_accuracy <- mean(accuracies)
std_accuracy <- sd(accuracies)

# Imprimir a média e desvio padrão das acurácias

cat("Acurácia Média: ", mean_accuracy, "\n")
cat("Desvio Padrão da Acurácia: ", std_accuracy, "\n")

# Guardando os valores de acurácia, média e desvio padrão em um arquivo

write.table(accuracies, file = "accuracies.txt", row.names = FALSE, col.names = FALSE)
write.table(mean_accuracy, file = "mean_accuracy.txt", row.names = FALSE, col.names = FALSE)
write.table(std_accuracy, file = "std_accuracy.txt", row.names = FALSE, col.names = FALSE)