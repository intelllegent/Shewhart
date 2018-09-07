library(shiny)
library(shinythemes)
library(ggplot2)

# Define UI for data upload app ----
ui <- fluidPage(
  
  theme=shinytheme("readable"),
  
  # App title ----
  titlePanel("Shewchart 2.0"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      
      # Input: Select a file ----
      fileInput("file1", "Choose File",
                multiple = FALSE),

      tags$hr(),
      
      uiOutput("vars"),
      uiOutput("graph")
    ),
     
      
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        tabPanel("Plot"),
        tabPanel("Summary", textOutput("text")),
        tabPanel("Summary Table", dataTableOutput("tbl"))
      )
      # Output: Data file ----
      
      
    )
    
  )
)