library(ggplot2)

ggplot(iris, aes(Sepal.Length,Sepal.Width)) +
  geom_point() + 
  facet_grid(.~ Species) +
  theme_bw()
