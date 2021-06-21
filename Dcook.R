ModeloRedInter %>%
  cooks.distance() %>%
  as.data.frame() %>% 
  ggplot(aes(x = seq(1, 41, 1), y = .)) +
  geom_bar(stat = "identity", fill = "#8F3A84FF") +
  labs(title = "Distancias de Cook",
       x = "Observación",
       y = "Valor de la distancia de cook") +
       geom_hline(yintercept = (4 / 40),
             colour = "#280434FF",
          size = 1) +
          theme(aspect.ratio = 1,
        plot.title = element_text(
          colour = "#280434FF",
          face = "bold"
        ),
         axis.text.x = element_text(
          color = "#8C04C2"
        ),
        axis.text.y = element_text(
          color = "#8C04C2"
        )) +
        geom_text(label = "Regla empírica",
           x = 3,
           y = 0.105,
           size = 4,
           colour = "#6C246269")