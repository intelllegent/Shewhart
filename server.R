# Подгрузка пакетов для работы --------------------------------------------


library(shiny)
library(tidyverse)
library(readxl)
library(DT)

get_types <- function(column_names){
  i=1
  k <- ""
  for (i in 1:(length(column_names))){
     k[i] <- switch(column_names[i],
                   Date = "date",
                   Material = "text",
                   `FOUP ID` = "text",
                   `Wafer ID` = "text",
                   `Deposition/Sputter` = "text",
                   `Test's type` = "text",
                   Use = "text",
                   `Test type` = "text",
                   Recipe = "text",
                   Operator = "text",
                   Point = "numeric",
                   `X(mm)` = "numeric",
                   `Y(mm)` = "numeric",
                   `Thickness(nm)` = "numeric",
                   `Thickness PRE(nm)` = "numeric",
                   `Thickness POST(nm)` = "numeric",
                   `Dep time(s)` = "numeric",
                   `Dep Rate (nm/min)` = "numeric",
                   `Average Thickness(nm)` = "numeric",
                   `Max(nm)` = "numeric",
                   `Min(nm)` = "numeric",
                   Delta = "numeric",
                   `Delta, nm` = "numeric",
                   `Delta (nm)` = "numeric",
                   `NU, %` = "numeric",
                   "guess")
  }
  return(k)
}

# Проверка пустого вектора
is.integer0 <- function(x)
{
  is.integer(x) && length(x) == 0L
}

server <- function(input, output,session) {

# Получение названий столбцов для выбора 
  data <- reactive({
    if (is.null(input$file1)) return(NULL)
    read_excel(input$file1$datapath, sheet = 1, col_names = TRUE, n_max = 0)
  })
  
  col_names <- reactive({
    colnames(data())
  })
  #str(col_names)

  output$vars <- renderUI({
    if (is.null(data())) return(NULL)
    tagList(
      checkboxGroupInput("show_vars", "Выберите столбцы:", 
                         col_names(),
                         choices = col_names())
    )
  })
# ---
  col_sort <- reactive({
    input$show_vars
  })
  
  col_type <- reactive({ 
    get_types(col_names()) 
  })
  
  col_trash <- reactive({
    setdiff(col_names(),col_sort())
  }) 
  
# Выгрузка выбранных столбцов
  observeEvent(input$file1, {
    insertUI("#show_vars", "afterEnd",
             actionButton("form_table", "Сформировать таблицу"))
    observeEvent(input$form_table, {
      i_col_trash <- isolate(col_trash())
      i_col_type <- isolate(col_type())
      
      if (length(i_col_trash)!=0) {
        i_col_names <- isolate(col_names())
        n_skip <- vector(mode="integer")

        for (i in 1: length(i_col_trash)){
          if (is.integer0(grep(i_col_trash[i],i_col_names))){
            next()
          }else{
           n_skip[i] <- grep(i_col_trash[i],i_col_names) 
          }
        }
        
        for (i in 1:length(n_skip)){
          i_col_type[n_skip[i]] <- "skip" 
        }
      }  
    
      
      data <- read_excel(input$file1$name, sheet = 1, col_names = TRUE, col_types = i_col_type)
      
      # Преобразование данных
      buff <- get_types(isolate(col_sort())) 
      for (i in 1:length(buff)){
        if (i_col_type[i] == "numeric"){
          data[i] <- round(data[i], 2)
        }
      }
      
      #---
      
      
      output$tbl <- renderDataTable({
          if (is.null(data)) return(NULL)
          if (length(input$show_vars) == 0L)
            return(data)
          else
            return(select(data, one_of(input$show_vars)))
        })
      removeUI("#show_vars")
      insertUI("#vars", "afterEnd",
               actionButton("plot", "Построить график"))
      removeUI("#form_table")
      
    }, ignoreInit = TRUE, once = TRUE) 
  })
  
  #Запрашивание построения графика
  observeEvent(input$plot,{
    output$graph <- renderUI({
      if (is.null(data())) return(NULL)
      tagList(
        radioButtons("x_graph", "Данные по оси Х:", 
                     colnames(data())
                     ),
        radioButtons("y_graph", "Данные по оси Y:", 
                     c("1","2","3")
        )
      )
    })
  })
  
  
  
  #---
  
  output$text <- renderText({
    req(input$file1)
    return(col_trash()) 
  })
 
}
