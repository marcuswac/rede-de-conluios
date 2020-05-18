library(ggplot2)

data <- data.frame(
  n_threads = c(1, 2, 3, 4, 5),
  tempo_execucao = c(1041, 985, 896, 795, 573))

ggplot(data, aes(x = n_threads, y = tempo_execucao)) +
  geom_line() + 
  geom_point()