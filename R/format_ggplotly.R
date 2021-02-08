##Format ggplotly
format_ggplotly <- function(gg){
  p <- ggplotly(gg + ylab(" ") + xlab(" "))
  x <- list(title = "")
  y <- list(title = "")
  p %>% layout(xaxis = x, yaxis = y, margin = list(l = 75, b =50))
}