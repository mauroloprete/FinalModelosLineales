lm(
    reformulate(names(Datos)[-16], names(Datos[16])),
    data = Datos
) -> Modelo 
mixlm::backward(Modelo)
mixlm::forward(Modelo)
mixlm::stepWise(Modelo)
