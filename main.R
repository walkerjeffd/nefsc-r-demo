library(ggplot2)

mtcars |>
  ggplot(aes(disp, mpg)) +
  geom_point()
