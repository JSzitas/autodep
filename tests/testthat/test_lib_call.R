


library( dplyr)
  require(ggplot2 )
# library( tidyr)


df <- iris %>%
    mutate( flower_volume = Sepal.Length*Sepal.Width + Petal.Length*Petal.Width ) %>%
    filter( Species %in% c("setosa","versicolor") )

ggplot( df, aes(x=flower_volume, color = Species, fill = Species) ) +
  geom_density( alpha = 0.5)
