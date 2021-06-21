## ----include=FALSE------------------------------------------------------------
## Configuración 
knitr::opts_chunk$set(
  echo = FALSE,
  fig.pos = 'H',
  warning = FALSE
)

## Paquetes

if(!require(pacman)) {
  install.packages("pacman")
}
pacman::p_load(
  here,
   reshape2,
   knitr,
   tidymodels,
   kableExtra,
   magrittr,
   GGally,
   car,
   nortest,
   tseries,
   lmtest,
   tidyverse,
   ggfortify,
   skedastic,
   RefManageR,
   bibtex
 )

# Para citar los paquetes

 knitr::write_bib(c(.packages(), "kntir"), "packages.bib")


 # Bibliografia :

BibOptions(
  check.entries = FALSE,
  bib.style = "authortitle",
  cite.style = "alphabetic",
  style = "markdown",
  hyperlink = FALSE,
  max.names = 2,
  dashed = FALSE
)
bib <- ReadBib(
  here("packages.bib"),
  check = FALSE,
  .Encoding = "UTF-8"
)
rbib <- ReadBib(
  here("packages.bib"),
  check = FALSE,
  .Encoding = "UTF-8"
)

# Cargar los datos

read.table(
  "UScrime.txt",
  header = TRUE,
  dec = ","
) %>% 
  mutate(
    Prob = Prob*100
  ) %>% 
  assign(
    "Datos",
    .,
    envir = .GlobalEnv
  )

# Para poder recuperar el script de los chunk

knitr::purl("Entrega.Rnw")



## ----fig.cap = "Histograma de la Tasa de Criminalidad",out.width = "0.4\\textwidth",fig.align = "center"----
# Histograma tasa de criminalidad

# Regla de Struges

Sturges<-log2(47)+1
ggplot(Datos,aes(x=y))+
  geom_histogram(bins = Sturges,
                 fill="#8F3A84FF",
                 colour="black")+
  theme(aspect.ratio = 1,
        plot.subtitle = element_text(
          size=6.5
        ),
        plot.title = element_text(
          face="bold",
          size=10,
          color = "#280434FF"
        ),
        axis.text.x = element_text(
          color = "#8C04C2"
        ),
        axis.text.y = element_text(
          color="#8C04C2"
        ))+
  labs(title="Histograma de Y",
       subtitle = "Tasa de criminalidad",
       x="Número de ofensas reportadas",
       y="Cantidad de Estados")


## ----echo=FALSE,fig.cap = "Histogramas (1)"-----------------------------------

# Histogramas multiples 1

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  
  if (is.null(layout)) {
  
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

HistM<-ggplot(Datos,aes(x=M))+
  geom_histogram(bins=Sturges,
                 fill="#8F3A84FF",
                 colour="black")+
  theme(aspect.ratio = 1,
        plot.subtitle = element_text(
          size=6.5
        ),
        plot.title = element_text(
          face="bold",
          size=10,
          color = "#280434FF"
        ),
        axis.text.x = element_text(
          color = "#8C04C2"
        ),
        axis.text.y = element_text(
          color="#8C04C2"
        ))+
  labs(title="Histograma variable M",
       subtitle="Número de hombres entre 14 y 24
       años por 1000 habitantes",
       x="Cantidad de Hombres",
       y="Cantidad de Estados")


BarpSo<-ggplot(data=Datos, aes(x=factor(So))) +
  geom_bar(stat="count", 
           fill="#8F3A84FF",
           colour="black")+
  theme(plot.subtitle = element_text(
          size=6.5
        ),
        plot.title = element_text(
          face = "bold",
          size=10,
          color = "#280434FF"
        ),
        axis.text.x = element_text(
          color = "#8C04C2"
        ),
        axis.text.y = element_text(
          color="#8C04C2"
        ),
        aspect.ratio=1)+
  labs(title = "Barplot variable So",
       subtitle = "¿Es este un estado sureño?",
       x="Respuesta",
       y="Cantidad de respuestas")

HistEd<-ggplot(Datos,aes(x=Ed))+
  geom_histogram(bins=Sturges,
                 fill="#8F3A84FF",
                 colour="black")+
  theme(aspect.ratio = 1,
        plot.subtitle = element_text(
          size=6.5
        ),
        plot.title = element_text(
          size=10,
          face="bold",
          color = "#280434FF"
        ),
        axis.text.x = element_text(
          color = "#8C04C2"
        ),
        axis.text.y = element_text(
          color="#8C04C2"
        ))+
  labs(title="Histograma variable Ed",
       subtitle="Escolaridad del Estado",
       x="Puntuación",
       y="Cantidad de Estados")

HistPo1<-ggplot(Datos,aes(x=Po1))+
  geom_histogram(bins=Sturges,
                 fill="#8F3A84FF",
                 colour="black")+
  theme(aspect.ratio = 1,
        plot.subtitle = element_text(
          size=6.5
        ),
        plot.title = element_text(
          face="bold",
          size=10,
          color = "#280434FF"
        ),
        axis.text.x = element_text(
          color = "#8C04C2"
        ),
        axis.text.y = element_text(
          color="#8C04C2"
        ))+
  labs(title="Histograma variable Po1",
       subtitle = "1960",
       x="Gasto",
       y="Cantidad de Estados")

HistPo2<-ggplot(Datos,aes(x=Po2))+
  geom_histogram(bins=Sturges,
                 fill="#8F3A84FF",
                 colour="black")+
  theme(aspect.ratio = 1,
        plot.subtitle = element_text(
          size=6.5
        ),
        plot.title = element_text(
          face="bold",
          size=10,
          color = "#280434FF"
        ),
        axis.text.x = element_text(
          color = "#8C04C2"
        ),
        axis.text.y = element_text(
          color="#8C04C2"
        ))+
  labs(title="Histograma variable Po2",
       subtitle = "1959",
       x="Gasto",
       y="Cantidad de Estados")

HistLF<-ggplot(Datos,aes(x=LF))+
  geom_histogram(bins=Sturges,
                 fill="#8F3A84FF",
                 colour="black")+
  theme(aspect.ratio = 1,
        plot.subtitle = element_text(
          size=6.5
        ),
        plot.title = element_text(
          face="bold",
          size=10,
          color = "#280434FF"
        ),
        axis.text.x = element_text(
          color = "#8C04C2"
        ),
        axis.text.y = element_text(
          color="#8C04C2"
        ))+
  labs(title="Histograma variable LF",
       subtitle = "TPFLM de 14 a 24 años",
       x="Gasto",
       y="Cantidad de Estados")

HistMF<-ggplot(Datos,aes(x=M.F))+
  geom_histogram(bins=Sturges,
                 fill="#8F3A84FF",
                 colour="black")+
  theme(aspect.ratio = 1,
        plot.subtitle = element_text(
          size=6.5
        ),
        plot.title = element_text(
          face="bold",
          size=10,
          color = "#280434FF"
        ),
        axis.text.x = element_text(
          color = "#8C04C2"
        ),
        axis.text.y = element_text(
          color="#8C04C2"
        ))+
  labs(title="Histograma variable M.F",
       subtitle = "Cantidad de hombres cada 1000 mujeres",
       x="Cantidad de Hombres",
       y="Cantidad de Estados")

HistPop<-ggplot(Datos,aes(x=Pop))+
  geom_histogram(bins=Sturges,
                 fill="#8F3A84FF",
                 colour="black")+
  theme(aspect.ratio = 1,
        plot.subtitle = element_text(
          size=6.5
        ),
        plot.title = element_text(
          face="bold",
          size=10,
          color = "#280434FF"
        ),
        axis.text.x = element_text(
          color = "#8C04C2"
        ),
        axis.text.y = element_text(
          color="#8C04C2"
        ))+
  labs(title="Histograma variable Pop",
       subtitle = "Tamaño de la población en cienmiles",
       x="Pop",
       y="Cantidad de Estados")

HistNW<-ggplot(Datos,aes(x=NW))+
  geom_histogram(bins=Sturges,
                 fill="#8F3A84FF",
                 colour="black")+
  theme(aspect.ratio = 1,
        plot.subtitle = element_text(
          size=6.5
        ),
        plot.title = element_text(
          face="bold",
          size=10,
          color = "#280434FF"
        ),
        axis.text.x = element_text(
          color = "#8C04C2"
        ),
        axis.text.y = element_text(
          color="#8C04C2"
        ))+
  labs(title="Histograma variable NW",
       subtitle = "Cantidad de no caucásicos cada 1000 habitantes",
       x="Cantidad de no caucásicos",
       y="Cantidad de Estados")

HistU1<-ggplot(Datos,aes(x=U1))+
  geom_histogram(bins=Sturges,
                 fill="#8F3A84FF",
                 colour="black")+
  theme(aspect.ratio = 1,
        plot.subtitle = element_text(
          size=6.5
        ),
        plot.title = element_text(
          face="bold",
          size=10,
          color = "#280434FF"
        ),
        axis.text.x = element_text(
          color = "#8C04C2"
        ),
        axis.text.y = element_text(
          color="#8C04C2"
        ))+
  labs(title="Histograma variable U1",
       subtitle = "TD cada 1000 habitantes,
hombres de 14 a 24 ",
       x="U1",
       y="Cantidad de Estados")

HistU2<-ggplot(Datos,aes(x=U2))+
  geom_histogram(bins=Sturges,
                 fill="#8F3A84FF",
                 colour="black")+
  theme(aspect.ratio = 1,
        plot.subtitle = element_text(
          size=6.5
        ),
        plot.title = element_text(
          face="bold",
          size=10,
          color = "#280434FF"
        ),
        axis.text.x = element_text(
          color = "#8C04C2"
        ),
        axis.text.y = element_text(
          color="#8C04C2"
        ))+
  labs(title="Histograma variable U2",
       subtitle = "TD cada 1000 habitantes, 
hombres de 35 a 39 ",
       x="U2",
       y="Cantidad de Estados")

HistGDP<-ggplot(Datos,aes(x=GDP))+
  geom_histogram(bins=Sturges,
                 fill="#8F3A84FF",
                 colour="black")+
  theme(aspect.ratio = 1,
        plot.subtitle = element_text(
          size=6.5
        ),
        plot.title = element_text(
          face="bold",
          size=10,
          color = "#280434FF"
        ),
        axis.text.x = element_text(
          color = "#8C04C2"
        ),
        axis.text.y = element_text(
          color="#8C04C2"
        ))+
  labs(title="Histograma variable GDP",
       subtitle = "PIB per cápita",
       x="GDP",
       y="Cantidad de Estados")+
  scale_y_continuous(breaks = c(2,4,6,8,10,12,14,16))

HistIneq<-ggplot(Datos,aes(x=Ineq))+
  geom_histogram(bins=Sturges,
                 fill="#8F3A84FF",
                 colour="black")+
  theme(aspect.ratio = 1,
        plot.subtitle = element_text(
          size=6.5
        ),
        plot.title = element_text(
          face="bold",
          size=10,
          color = "#280434FF"
        ),
        axis.text.x = element_text(
          color = "#8C04C2"
        ),
        axis.text.y = element_text(
          color="#8C04C2"
        ))+
  labs(title="Histograma variable Ineq",
       subtitle = "Desigualdad de Ingresos",
       x="Ineq",
       y="Cantidad de Estados")+
   scale_y_continuous(breaks = c(2,4,6,8,10,12))

HistProb<-ggplot(Datos,aes(x=Prob))+
  geom_histogram(bins=Sturges,
                 fill="#8F3A84FF",
                 colour="black")+
  theme(aspect.ratio = 1,
        plot.subtitle = element_text(
          size=6.5
        ),
        plot.title = element_text(
          face="bold",
          size=10,
          color = "#280434FF"
        ),
        axis.text.x = element_text(
          color = "#8C04C2",
          angle = 90
        ),
        axis.text.y = element_text(
          color="#8C04C2"
        ))+
  labs(title="Histograma variable Prob",
       subtitle = "Probabilidad de encarcelamiento",
       x="P",
       y="Cantidad de Estados")

HistTime<-ggplot(Datos,aes(x=Time))+
  geom_histogram(bins=Sturges,
                 fill="#8F3A84FF",
                 colour="black")+
  theme(aspect.ratio = 1,
        plot.subtitle = element_text(
          size=6.5
        ),
        plot.title = element_text(
          face="bold",
          size=10,
          color = "#280434FF"
        ),
        axis.text.x = element_text(
          color = "#8C04C2"
        ),
        axis.text.y = element_text(
          color="#8C04C2"
        ))+
  labs(title="Histograma variable Time",
       subtitle = "Tiempo promedio de estadía en cárceles estatales",
       x="Tiempo",
       y="Cantidad de Estados")

multiplot(HistM,BarpSo,HistEd,HistPo1,HistNW,HistPo2,HistLF,HistMF,HistPop,cols=3)


## ----fig.cap = "Histogramas (2)"----------------------------------------------
multiplot(HistU1,HistU2,HistGDP,HistIneq,HistProb,HistTime,cols=3)


## -----------------------------------------------------------------------------
# Tabla descriptiva

DatosTab <- Datos
names(DatosTab) <- c(
  "Número de Hombres 14-24 / 1.000",
  "Indicadora Estado Sur",
  "Indice Escolaridad",
  "Gasto per cápita 1.960",
  "Gasto per cápita 1.959",
  "Tasa participación masculina 14-24 por 1.000",
  "Hombres cada 1.000 mujeres",
  "Población cada 100.000",
  "Número de no caucásicos cada 1.000 habitantes",
  "Tasa desempleo urbana Hombres 14-24 por 1.000",
  "Tasa desempleo urbana Hombres 35-39 por 1.000",
  "Producto bruto interno per cápita",
  "Desigualdad ingreso",
  "Probabilidad Encarcelamiento",
  "Tiempo de estadía en carceles",
  "Tasa de criminalidad"
)

summaryMod <- function(x) {
  c(
    quantile(x, probs = seq(0, 1, 0.25)),
    mean(x),
    (sd(x)/mean(x))*100
  )
}
DatosTab %>%
  select(
    -"Indicadora Estado Sur"
  ) %>% 
  summarise_if(
    is.numeric,
    summaryMod
  ) %>% 
  mutate(
    Estadistico = c(
      "Min",
      "1er Qu.",
      "Mediana",
      "3er Qu.",
      "Max",
      "Media",
      "CV*100"
    )
  ) %>% 
  relocate(
    where(is.numeric),
    .after = where(is.character)
  ) %>%
  mutate_if(
    is.numeric,
    format,
    digits = 2
  ) %>% 
  pivot_longer(
    cols = -Estadistico,
    names_to = "Variable",
    values_to = "Valor"
  ) %>% 
  pivot_wider(
    values_from = "Valor",
    names_from = "Estadistico"
  ) %>% 
  kbl(
    booktabs = T, 
    caption = "Medidas descriptivas para variables númericas"
  ) %>% 
  kable_styling(
    latex_options = c("striped", "hold_position"),
    font_size = 8.5
  )


## ----fig.cap = "Mapa de correlación de variables incluidas",out.width = "0.5\\textwidth",fig.align = "center"----
# Mapa de correlación entre variables

qplot(x=Var1,
      y=Var2,
      data = melt(cor(Datos, use = "p")),
      fill = value,
      geom="tile"
)+scale_fill_gradient2(limits = c(-1, 1),
                       low = "#A50303", high = "#250455")+
  theme(aspect.ratio = 1,
        plot.title = element_text(
          face = "bold",
          color = "#280434FF"),
        plot.subtitle = element_text(
          size=8,
          color="#5C485F"
        ),
        axis.text.x = element_text(
          color = "#8C04C2",
          angle=90
        ),
        axis.text.y = element_text(
          color="#8C04C2"
        )
        )+
  labs(title="Mapa de correlación de variables",
       subtitle = "Variables referidas a tasa de criminalidad en USA",
       x="Variables",
       y="",
       fill="Coef Corr.")


## -----------------------------------------------------------------------------
# Se define el modelo completo

lm(
    y ~ .,
    data = Datos
) %T>% 
  assign(
    "ModeloCompleto",
    .,
    envir = .GlobalEnv
  ) %>% 
  glance() %>% 
  mutate(
    "$R^{2}.adj$" = adj.r.squared*100,
    "RSE" = sigma,
    "F Obs." = statistic,
    "P-valor*100" = p.value*100,
    "Regresión.gl" = df,
    "Residuos.gl" = df.residual,
    .keep = "none"
  ) %>% 
  summarise_if(
    is.numeric,
    ~round(.x,3)
  ) %>% 
  kbl(
    booktabs = T,
    caption = "Test sobre el modelo completo",
    escape = FALSE
  ) %>% 
    kable_styling(
    latex_options = c("striped", "hold_position"),
    font_size = 7.5
  )


## -----------------------------------------------------------------------------
# Test de forma independiente Modelo Completo

ModeloCompleto %>% 
  tidy() %>% 
  summarise_if(
    is.numeric,
    ~round(.x,3)
  ) %>% 
  mutate(
    Variable = c(
      "Intercepto",
      names(DatosTab)[-length(DatosTab)]
    ),
    "Estimación" = estimate,
    "Error estandar" = std.error,
    "Estadístico F" = statistic,
    "P valor" = p.value,
    "$\\left(H_{0}^{\\alpha = 0.05}\\right)\\beta_{i} = 0$" = case_when(
      p.value < 0.05 ~ "Se rechaza H0",
      p.value >= 0.05 ~ "No se rechaza H0"
    ),
    .keep = "none"
  ) %>% 
  kbl(
    booktabs = T, 
    caption = "Estimación,error estandar y test individual del modelo completo ",
    escape = FALSE
  ) %>% 
  kable_styling(
    latex_options = c("striped", "hold_position"),
    font_size = 7.5
  )


## -----------------------------------------------------------------------------

# Multicolinealidad modelo completo

vif(ModeloCompleto) %>% 
  tidy() %>% 
  mutate(
  Variable = c(
    names(DatosTab)[-length(DatosTab)]
  ),
    VIF = x,
    "Prueba" = case_when(
      x >= 10 ~ "Problema de colinealidad",
      x < 10 ~ "No hay problema de colinealidad"
    ),
    .keep = "none"
  ) %>%  
  mutate_if(
    is.numeric,
    round,
    3
  ) %>% 
  kbl(
    booktabs = T, 
    caption = "Prueba de multicolinealidad : Factor de incremento de Varianza \\textbf{VIF} ",
    escape = FALSE
  ) %>% 
  kable_styling(
    latex_options = c("striped", "hold_position"),
    font_size = 7.5
  )


## ----results='hide'-----------------------------------------------------------

# Stepwise

lm(
  reformulate(names(Datos)[-16], names(Datos[16])),
  data = Datos
) %>% 
  mixlm::stepWise() %>% 
  assign(
    "ModeloRed",
    .,
    envir = .GlobalEnv
  )


## -----------------------------------------------------------------------------
ModeloRed %>%  
  tidy() %>% 
  summarise_if(
    is.numeric,
    ~round(.x,3)
  ) %>% 
  mutate(
    Variable = c(
      "Intercepto",
      "Gasto per capita en policía 1960",
      "Desigualdad del ingreso",
      "Indice que refleja la escolaridad del estado",
      "Número de hombres entre 14 y 24 / 1000",
      "Probabilidad de encarcelamiento",
      "Tasa de desempleo urbana hombres 35-39 años x 1000"
    ),
    "Estimación" = estimate,
    "Error estandar" = std.error,
    "Estadístico F" = statistic,
    "P valor" = p.value,
    "$\\left(H_{0}^{\\alpha = 0.05}\\right)\\beta_{i} = 0$" = case_when(
      p.value < 0.05 ~ "Se rechaza H0",
      p.value >= 0.05 ~ "No se rechaza H0"
    ),
    .keep = "none"
  ) %>% 
  kbl(
    booktabs = T, 
    caption = "Estimación,error estandar y test individual tras aplicar el método Stepwise ",
    escape = FALSE
  ) %>% 
  kable_styling(
    latex_options = c("striped", "hold_position"),
    font_size = 7.5
  )


## ----fig.align= "center",out.width = "0.8\\textwidth"-------------------------

# Distancia de Cook

ModeloRed %>% 
  cooks.distance() %>% 
  as.data.frame() %>% 
  mutate(
    .,
    Valor = .,
    .keep = "none"
  ) %>% 
  ggplot(
    aes(
      x = seq(1, 47, 1), 
      y = Valor
    )
  ) +
  geom_bar(stat="identity",fill="#8F3A84FF")+
  labs(title="Distancias de Cook",
       x="Observación",
       y="Valor de la distancia de cook")+
  geom_hline(yintercept = (4/40),
             colour="#280434FF",
          size=1)+
  theme(aspect.ratio = 1,
        plot.title = element_text(
          colour = "#280434FF",
          face="bold"
        ),
         axis.text.x = element_text(
          color = "#8C04C2"
        ),
        axis.text.y = element_text(
          color="#8C04C2"
        ))+
geom_text( label="Regla empírica",
           x=3,
           y=0.105,
           size=4,
           colour="#6C246269")



## ----fig.align= "center",out.width = "0.8\\textwidth"-------------------------

# Observaciones atipicas

ModeloRed  %>% 
  rstudent() %>% 
  as.data.frame() %>% 
  mutate(
    Ati = case_when(
      abs(.) >= qt(1-0.01/2,ModeloRed %$% df.residual) ~ .,
      NA ~ 0
    )
  ) %>% 
  ggplot(
    aes(
      x = 1:nrow(Datos),
      y = .
    )
  ) + 
  geom_point(colour="#8C04C2") + 
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "#8F3A847A",
    size = 1.2
  ) +
  geom_hline(
    yintercept = qt(
      1-0.01/2,
      ModeloRed %$% df.residual
    ),
    size = 1.2,
    color = "#280434FF"
  ) +
  geom_hline(
      yintercept = - qt(
        1-0.01/2,
        ModeloRed %$% df.residual
      ),
      size = 1.2,
      color = "#280434FF"
  ) + 
  geom_point(
    aes(
      y = Ati
    ),
    shape = 18, 
    color = "#FC4E07",
    size = 3
  ) + 
  scale_x_continuous(
    breaks = seq(1,nrow(Datos),2)
  ) +
  labs(
    x = "Observación",
    y = "Residudos studentisados"
  ) +
  theme(
    aspect.ratio = 1,
    plot.title = element_text(
      colour = "#280434FF",
      face="bold"
      ),
    axis.text.x = element_text(
      color = "#8C04C2"
      ),
    axis.text.y = element_text(
          color="#8C04C2"
      )
    )


## ----results = 'hide'---------------------------------------------------------
# Se interviene el modelo
Datos %<>%
  slice(
    -c(
      11,
      29,
      18
    )
  )

ModeloRed %>%
  update(
    .,
    data = Datos
  ) %T>% 
  assign(
    "ModeloRedInter",
    .,
    envir = .GlobalEnv
  )


## ----out.width = "0.65\\textwidth",fig.align = "center"-----------------------

# Gráfico de erorres y qq plot
ModeloRedInter %>% 
  autoplot(
    which = c(1,2)
  ) + 
  theme(
    aspect.ratio = 1,
    plot.title = element_text(
      colour = "#280434FF",
      face="bold"
    ),
    axis.text.x = element_text(
    color = "#8C04C2"
    ),
    axis.text.y = element_text(
      color="#8C04C2"
    )
  )


## ----out.width = "0.5\\textwidth",fig.align = "center"------------------------

# Distribución teorica vs normal 

ModeloRedInter  %>% 
  rstandard() %>% 
  as.data.frame() %>% 
  ggplot(
    aes(
      x = .
    )
  ) + 
  geom_density(
    size = 1.7,
    colour = "#8F3A84FF"
  ) + 
  geom_line(
    aes(
      x = seq(-4,4,length.out = 44),
      y = dnorm(seq(-4,4,length.out = 44))
    ),
    colour = "#280434FF",
    size = 1.7
  ) + 
  labs(
    title = "Valores téoricos y errores " ,
    x="x"
  ) +
  theme(
    aspect.ratio = 1,
    plot.title = element_text(
      colour = "#280434FF",
      face="bold"
    ),
    axis.text.x = element_text(
      color = "#8C04C2"
    ),
    axis.text.y = element_text(
      color="#8C04C2"
    )
  )


## -----------------------------------------------------------------------------
# Test normalidad

TestNormalidad <- function(lm) {
  x = rstudent(lm) 
  Esta = c(
    lillie.test(x)$statistic,
    shapiro.test(x)$statistic,
    jarque.bera.test(x)$statistic
  )
  Pvalue = c(
    lillie.test(x)$p.value,
    shapiro.test(x)$p.value,
    jarque.bera.test(x)$p.value
  )
  return(
    data.frame(
      "Test" = c(
        "Lillie",
        "Shapiro",
        "Jarque Bera"
      ),
      "Pvalor" = Pvalue,
      "Estadistico" = Esta
    )
  )
}

ModeloRedInter %>% 
  TestNormalidad() %>% 
  mutate_if(
    is.numeric,
    round,
    3
  ) %>% 
  mutate(
    Resultado = case_when(
      Pvalor > 0.05 ~ "No rechazo normalidad",
      NA ~ "Rechazo H0"
    )
  ) %>% 
  kbl(
    booktabs = T, 
    caption = "Test de Normalidad ",
    escape = FALSE,
    row.names = FALSE
  ) %>% 
  kable_styling(
    latex_options = c("striped", "hold_position"),
    font_size = 7.5
  )


## -----------------------------------------------------------------------------

# Test de heterocedasticidad

Het<-function(lm) {
  Esta = c(
    bptest(lm)$statistic,
    white_lm(lm)$statistic,
    glejser(lm)$statistic
  )
  Pvalue = c(
    bptest(lm)$p.value,
    white_lm(lm)$p.value,
    glejser(lm)$p.value
  )
  return(
    data.frame(
      "Test" = c(
        "Breusch-Pagan",
        "White",
        "Glejser"),
      "Pvalor" = Pvalue,
      "Estadistico" = Esta
    )
  )
}

Het(ModeloRedInter)%>%
  mutate_if(
    is.numeric,
    round,
    3
  ) %>% 
  mutate(
    Resultado = case_when(
      Pvalor > 0.05 ~ "No rechazo Homocedasticidad",
      NA ~ "Rechazo H0"
    )
  ) %>% 
  kbl(
    booktabs = T, 
    caption = "Test de Homocedasticidad ",
    escape = FALSE,
    row.names = FALSE
  ) %>% 
  kable_styling(
    latex_options = c("striped", "hold_position"),
    font_size = 7.5
  )


## -----------------------------------------------------------------------------
lm(
  reformulate(names(Datos)[-16], names(Datos[16])),
  data = Datos
) %>% 
  mixlm::stepWise(full = TRUE)


## -----------------------------------------------------------------------------
# No anda la biblio en mi pc
# NoCite(bib)
# PrintBibliography(bib, .opts = list(bib.style = "alphabetic"))

