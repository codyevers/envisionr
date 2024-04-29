library(animation)
saveGIF({
  for(y in 2:40){
    pdat <- data.frame(y = y)
    plot <- ggplot(pdat) +
      geom_segment(linewidth = 10, aes(y = 0, x = -3, yend = 0, xend = y)) +
      geom_text(aes(y = 0, x = y, label = y + 2020), nudge_x = -2, color = 'white', size = 5) +
      scale_x_continuous(limits = c(-3,40), expand = c(0,0)) +
      theme_void() +
      theme(panel.background = element_rect(fill = 'lightgrey '))
    print(plot)
  }
}, movie.name = 'year_animation.gif', interval = .5, ani.height = 20, ani.width = 500)
