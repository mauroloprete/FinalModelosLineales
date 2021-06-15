if (!require(pacman)) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  reshape2,
  knitr,
  tidymodels,
  kableExtra,
  magrittr,
  GGally
)

Datos <- read.table(
    "UScrime.txt",
    header = TRUE,
    dec = ","
)

LPlot <- function(data,mapping) {
    gf <- ggplot(
        data = data,
        mapping = mapping
    ) +
    geom_point(
        color = "#E8DAEF"
    ) +
    geom_smooth(
        method = "lm",
        se = FALSE,
        color = "#280434FF"
    )
}

Datos %>% 
    select(
        M,
        NW,
        U1,
        y
    ) %>% 
ggpairs(
    lower = list(
        continuous = LPlot
    )
)

Datos  %>% 
 select(
     -So
 ) %>% 
 ggpairs(
     lower = list(
         continuous = LPlot
     )
 ) +
 theme(
      axis.text.x = element_text(
       color = "#8C04C2"
      ),
      axis.text.y = element_text(
        color = "#8C04C2"
      )
 )