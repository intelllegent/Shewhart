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

server <- function(input, output,session) {

# Получение названий столбцов для выбора 
  data <- reactive({
    if (is.null(input$file1)) return(NULL)
    read_excel(input$file1$datapath, sheet = 1, col_names = TRUE, n_max = 1)
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
  
  col_types <- reactive({ 
    get_types(col_sort()) 
  })
  
  col_trash <- reactive({
    setdiff(col_names(),col_sort())
  }) 
  
# Выгрузка выбранных столбцов
  observeEvent(input$file1, {
    insertUI("#show_vars", "afterEnd",
             actionButton("form_table", "Сформировать таблицу"))
    observeEvent(input$form_table, {
      a <- isolate(col_trash())
      a[1]
      b <- isolate(col_names())
      for (i in 1: length(a)){
        n_skip <- vector(mode="integer")
        n_skip[i] <- grep(a[i],b)
      }
      n_skip
        
      isolate(col_types())
      data <- read_excel(input$file1$name, sheet = 1, col_names = TRUE, col_types = isolate(col_names()))
      output$tbl <- renderDataTable({
          if (is.null(data)) return(NULL)
          if (length(input$show_vars) == 0L)
            return(data)
          else
            return(select(data, one_of(input$show_vars)))
        })
      removeUI("#show_vars")
      removeUI("#form_table")
    }, ignoreInit = TRUE, once = TRUE) 
  })
  
  output$text <- renderText({
    req(input$file1)
    return(col_trash()) 
  })
 
}
