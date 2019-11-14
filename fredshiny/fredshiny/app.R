#########
# FRED Shiny app by Thomas Geissberger & Shafer Powell
#########

library(shiny)
library(FRED1pkg) # package containing the FRED dataset
library(ggplot2)

ui <- fluidPage(
  titlePanel("FRED 1 data explorer"),

  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(4,
           selectInput("bgpart",
                       "Belowground part",
                       c("All",
                         unique(as.character(fred1$Belowground.part))))
    ),
    column(4,
           selectInput("family",
                       "Plant family:",
                       c("All",
                         unique(as.character(fred1$Plant.taxonomy_Family_TPL))))
    ),
    column(4,
           selectInput("pot",
                       "Growth environment:",
                       c("All",
                         unique(as.character(fred1$Notes_In.situ..pot..or.hydroponic))))
    )
  ),
  # Create a new row for the table.
  DT::dataTableOutput("table")
)

server <- function(input, output) {

  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- fred1
    if (input$bgpart != "All") {
      data <- data[data$Belowground.part == input$bgpart,]
    }
    if (input$family != "All") {
      data <- data[data$Plant.taxonomy_Family_TPL == input$family,]
    }
    if (input$pot != "All") {
      data <- data[data$Notes_In.situ..pot..or.hydroponic == input$pot,]
    }
    data
  }))

}


shinyApp(ui, server)
