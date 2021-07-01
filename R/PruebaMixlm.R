Datos <- read.table(
    "UScrime.txt",
    header = TRUE,
    dec = ","
)

lm(
    reformulate(names(Datos)[-16], names(Datos[16])),
    data = Datos
) -> Modelo 
ModelMod1<-mixlm::backward(Modelo)
ModelMod <- mixlm::forward(Modelo)
mixlm::stepWise(Modelo) %>% tidy()
mixlm::stepWise(Modelo)  %>% summary()


library(ggfortify)

autoplot(Modelo)
ggsave(
    filename = "Modelo.png",
    path = here(),
    device = 'png',
    dpi = 400
)

autoplot(ModelMod)

ggsave(
    filename = "Modelo1.png",
    path = here(),
    device = 'png',
    dpi = 400
)
autoplot(ModelMod1)

