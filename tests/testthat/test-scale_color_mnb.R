ggplot2::ggplot(iris) + 
  ggplot2::aes(Sepal.Length, Sepal.Width, color = Species) +
  ggplot2::geom_point(size = 3) + 
  scale_color_mnb(values = c("red4", "blue", "yellow"))

ggplot2::ggplot(iris) + 
  ggplot2::aes(Sepal.Length, Sepal.Width, color = Species) +
  ggplot2::geom_point(size = 3) + 
  scale_color_mnb()
