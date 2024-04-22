library(ggplot2)

funcao_geradora <- function(x) 0.5 * x^2 + 3 * x + 10

mean <- 0
sd <- 4

x_values <- seq(-15, 10, length.out = 10)

p_degrees <- 1:8

set.seed(123)
data <- data.frame(x = x_values, y = funcao_geradora(x_values) +
                     rnorm(length(x_values), mean = mean, sd = sd))

plot_polynomial_approximation <- function(degree) {
  p <- lm(y ~ poly(x, degree), data = data)
  x_range <- seq(-15, 10, length.out = 10)
  y_pred <- predict(p, newdata = data.frame(x = x_range))
  p_plot <- ggplot(data, aes(x = x, y = y)) +
    geom_point() +
    geom_line(data = data.frame(x = x_range, y = y_pred),
              aes(x = x, y = y_pred), color = "blue") +
    geom_line(data = data.frame(x = x_range, y = funcao_geradora(x_range)),
              aes(x = x, y = funcao_geradora(x_range)),
              color = "red", linetype = "dashed") +
    labs(title = paste("Grau do Polinomio: ", degree)) +
    theme_minimal()
  return(p_plot)
}

for (degree in p_degrees) {
  p <- plot_polynomial_approximation(degree)
  print(p)
}