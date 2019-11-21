#########
# FRED Shiny app by Thomas Geissberger & Shafer Powell
#########

library(shiny)
library(FRED1pkg) # package containing the FRED dataset. Downloadable from Github at https://github.com/shaferpowell2/geol590repos/tree/master/FRED1pkg
library(tidyverse) #so that pipe function works

#Subset FRED for simpler display
set.seed(50)
fred1subset <- fred1 %>%
  sample_n(1000) %>%
  select(Belowground.part,Plant.taxonomy_Family_TPL, Accepted.genus_TPL, Accepted.species_TPL, Notes_In.situ..pot..or.hydroponic, Root.diameter, Belowground.biomass.per.ground.area, Root.length.density..RLD._Root.length.per.ground.area, Root.C.N.ratio)


ui <- fluidPage(
  titlePanel("FRED 1 data explorer"),

  # Create a new Row in the UI for selectInputs
  fluidRow(
    column(4,
           selectInput("bgpart",
                       "Belowground part",
                       c("All",
                         sort(unique(as.character(fred1$Belowground.part)))))
    ),
    column(4,
           selectInput("family",
                       "Plant family:",
                       c("All",
                         sort(unique(as.character(fred1$Plant.taxonomy_Family_TPL)))))
    ),
    column(4,
           selectInput("pot",
                       "Growth environment:",
                       c("All",
                         sort(unique(as.character(fred1$Notes_In.situ..pot..or.hydroponic))))),
    )
  ),
  # Add download button
  downloadButton("downloadData", "Download"),

  # Create a new row for the table.
  tableOutput("table")
)

server <- function(input, output) {

  # Filter data based on selections
  output$table <- renderTable(({
    #stop("Unable to render table")
    data <- fred1subset
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

  #Create output that is not in datatable form and can therefore be exported as a CSV

    datasetInput <- reactive({
      data2 <- FRED1pkg::fred1
    if (input$bgpart != "All") {
      data2 <- data2[data2$Belowground.part == input$bgpart,]
    }
    if (input$family != "All") {
      data2 <- data2[data2$Plant.taxonomy_Family_TPL == input$family,]
    }
    if (input$pot != "All") {
      data2 <- data2[data2$Notes_In.situ..pot..or.hydroponic == input$pot,]
    }
      data2
    })



  # Downloadable csv of selected dataset
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )

}


shinyApp(ui, server)
