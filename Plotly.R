library(dplyr)
library(tidyr) 
library(openxlsx)
library(ggplot2)
library(plotly)

file_path <- 'FRT Weekly Monitoting.xlsx'
Data <- Data_read <- read.xlsx(file_path ,sheet = 1, detectDates = TRUE)

USG_fifth <- Data[Data$Material=="USG - 5k",]
USG_first <- Data[Data$Material=="USG - 1k",]

A_Th_first <- data.frame(USG_first$Date, USG_first$`Average.Thickness(nm)`)
A_Th_first <- na.omit(A_Th_first)
rownames(A_Th_first)= c(1:length(A_Th_first$USG_first.Date))
names(A_Th_first) <- c('Date','Average_Thickness_nm')
A_Th_first$Average_Thickness_nm <- round(A_Th_first$Average_Thickness_nm,2)
A_Th_first$Date <- as.Date(as.character(A_Th_first$Date))
A_Th_first<-A_Th_first[order(A_Th_first$Date),]

A_Th_fifth <- data.frame(USG_fifth$Date, USG_fifth$`Average.Thickness(nm)`)
A_Th_fifth <- na.omit(A_Th_fifth)
rownames(A_Th_fifth)= c(1:length(A_Th_fifth$USG_fifth.Date))
names(A_Th_fifth) <- c('Date','Average_Thickness_nm')
A_Th_fifth$Average_Thickness_nm <- round(A_Th_fifth$Average_Thickness_nm,2)
#A_Th_fifth$Date <-  as.Date(as.character(A_Th_fifth$Date))
A_Th_fifth<-A_Th_fifth[order(A_Th_fifth$Date),]

Av_usg <- mean(A_Th_first$Average_Thickness_nm, na.rm = TRUE)

Sigm <- sd(A_Th_first$Average_Thickness_nm, na.rm = TRUE)

LCL <- Av_usg - 3 * Sigm
UCL <- Av_usg + 3 * Sigm



plot_ly(A_Th_first, x = ~Date, y = ~Average_Thickness_nm,
        name = 'Average Thickness',
        type = 'scatter',
        mode = 'lines+markers+text')%>%
  add_lines(y = Av_usg,
            name = 'Average', 
            type = 'scatter',
            hoverinfo = 'none', 
            showlegend = FALSE, 
            color = I('green'),
            text = paste('Internet ')) %>%
  add_lines(y = LCL, 
            name = 'LCL', 
            hoverinfo = 'none', 
            showlegend = FALSE,
            color = I('red')) %>%
  add_lines(y = UCL,
            name = 'UCL',
            hoverinfo = 'none',
            showlegend = FALSE,
            color = I('red')) %>%
  add_ribbons(ymin = LCL,
              ymax = LCL + Sigm,
              color = '#FCBBA1',
              hoverinfo = 'none',
              showlegend = FALSE,
              line = list(
                width = 0
              ))%>%
  add_ribbons(ymin = LCL + Sigm,
              ymax = LCL + 2 * Sigm,
              color = I('#FFFFBF'),
              hoverinfo = 'none',
              showlegend = FALSE,
              line = list(
                width = 0
              ))%>%
  add_ribbons(ymin = LCL + 2 * Sigm,
              ymax = LCL + 3 * Sigm,
              color = I('#C7E9C0'),
              hoverinfo = 'none',
              showlegend = FALSE,
              line = list(
                width = 0
              ))%>%
  add_ribbons(ymin = LCL + 3 * Sigm,
              ymax = LCL + 4 * Sigm,
              color = I("#C7E9C0"),
              hoverinfo = 'none',
              showlegend = FALSE,
              line = list(
                width = 0
              ))%>%
  add_ribbons(ymin = LCL + 4 * Sigm,
              ymax = LCL + 5 * Sigm,
              color = I('#FFFFBF'),
              hoverinfo = 'none',
              showlegend = FALSE,
              line = list(
                width = 0
              ))%>%
  add_ribbons(ymin = LCL + 5 * Sigm,
              ymax = LCL + 6 * Sigm,
              color = '#FCBBA1',
              hoverinfo = 'none',
              showlegend = FALSE,
              line = list(
                width = 0
              ))%>%
  
    layout(title = 'Film thickness distribution',
           legend = list(orientation = 'h'),
           xaxis = list(title = 'Process date'),
           yaxis = list(title = 'Thickness, [nm]')
    )
