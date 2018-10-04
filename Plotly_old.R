add_ribbons(p=p,
            ymin = LCL,
            ymax = LCL + Sigm,
            color = '#FCBBA1',
            hoverinfo = 'none',
            showlegend = FALSE,
            line = list(
              width = 0)
)%>%
  add_ribbons(ymin = LCL + Sigm,
              ymax = LCL + 2 * Sigm,
              color = I('#FFFFBF'),
              hoverinfo = 'none',
              showlegend = FALSE,
              line = list(
                width = 0)
  )%>%
  add_ribbons(ymin = LCL + 2 * Sigm,
              ymax = LCL + 3 * Sigm,
              color = I('#C7E9C0'),
              hoverinfo = 'none',
              showlegend = FALSE,
              line = list(
                width = 0)
  )%>%
  add_ribbons(ymin = LCL + 3 * Sigm,
              ymax = LCL + 4 * Sigm,
              color = I("#C7E9C0"),
              hoverinfo = 'none',
              showlegend = FALSE,
              line = list(
                width = 0)
  )%>%
  add_ribbons(ymin = LCL + 4 * Sigm,
              ymax = LCL + 5 * Sigm,
              color = I('#FFFFBF'),
              hoverinfo = 'none',
              showlegend = FALSE,
              line = list(
                width = 0)
  )%>%
  add_ribbons(ymin = LCL + 5 * Sigm,
              ymax = LCL + 6 * Sigm,
              color = '#FCBBA1',
              hoverinfo = 'none',
              showlegend = FALSE,
              line = list(
                width = 0)
  )%>%