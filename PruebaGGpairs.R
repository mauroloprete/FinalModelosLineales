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

read.table(
    "UScrime.txt",
    header = TRUE,
    dec = ","
) %>% 
mutate(
    So = factor(So)
) %>% 
assign(
    "Datos",
    .,
    envir = .GlobalEnv
)

LPlot <- function(data,mapping) {
    gf <- ggplot(
        data = data,
        mapping = mapping,
        aes(
            colour = colour
        )
    ) +
    geom_point(
        color = "#C39BD3"
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
        y,
        So
    ) %>% 
ggpairs(
    lower = list(
        continuous = LPlot
    )
)

Datos  %>% 
 ggpairs(
     aes(color = So),
     lower = list(
         continuous = LPlot
     ),
     upper = list(
         continuous = wrap(
             "cor",
             color = "#4A235A"
         )
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
 ggsave(
     "Grafico.png",
      width = 15,
      height = 13,
      dpi = 300,
      units = "in",
      device = 'png'
 )