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
   GGally,
   car,
   ggfortify
 )

read.table(
   "UScrime.txt",
   header = TRUE,
   dec = ","
 ) %>%
   mutate(
     Prob = Prob * 100
   ) %>%
   assign(
     "Datos",
     .,
     envir = .GlobalEnv
   )

lm(
    y ~ . - Ineq - Po2 - NW,
    data = Datos
) %T>%
  assign(
    "ModeloInicial",
    .,
    envir = .GlobalEnv
  )


lm(
  y ~ M + So + Ed + Po1 + LF + M.F + Pop + U1 + U2 + GDP + Prob + Time,
  data = Datos
) %>%
  mixlm::stepWise() %T>%
  assign(
    "ModeloRed",
    .,
    envir = .GlobalEnv
  ) %>%
  autoplot()