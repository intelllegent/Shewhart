a <- data.frame(x = c(rep(1:4, each = 2), 6, 6), y = 1:10)
b <- data.frame(x = c(1:4, 6), y =  seq(from = 1.5, to = 9.5,by = 2))


p <- plot_ly(data = a,
             x = rep(1:5, each = 2),
             y = ~y,
             name = 'A',
             mode = 'markers',
             type = 'scatter')
p <- add_trace(p = p,
               data = b,
             x = 1:5,
             y = ~y,
             name = 'B',
             mode = 'lines+markers',
             type = 'scatter')

p <- layout(p=p,
            xaxis = list(
              tickmode = 'array',
              tickvals = c(1:5),
              ticktext = c("A","B","C","D","E")
            ))
p