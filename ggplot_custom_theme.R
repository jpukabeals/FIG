
# creating dummy data
# data("iris")
# library(tidyverse)
# 
# iris %>% 
#   rename_all(tolower) ->iris
# iris %>% 
#   ggplot(aes(species,sepal.length,
#              col=species)) +
#   geom_point() -> dummy;dummy

# Setting a custom ggplot2 function
theme_jpb <- function() {
  theme_bw() +
    theme(
      axis.text.x = element_text(size = 12,
                                 vjust = 1),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 14, face = "plain"),
      axis.title.y = element_text(size = 14, face = "plain"),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      plot.title = element_text(
        size = 20,
        vjust = 1,
        hjust = 0.5
      ),
      legend.text = element_text(size = 12, face = "italic"),
      legend.title = element_blank()
    )
}

theme_custom <- function() {
  theme_bw() +
    theme(
      axis.text.x = element_text(
        size = 12,
        angle = 45,
        vjust = 1,
        hjust = 1
      ),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 14, face = "plain"),
      axis.title.y = element_text(size = 14, face = "plain"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
      plot.title = element_text(
        size = 20,
        vjust = 1,
        hjust = 0.5
      ),
      legend.text = element_text(size = 12, face = "italic"),
      legend.title = element_blank(),
      legend.position = c(0.9, 0.9)
    )
}

# applying custom theme
# dummy +
#   theme_custom() +
#   theme(legend.position = c(.1,.9)) +
#   labs(x="Species",
#        y="Sepal length (units)")
