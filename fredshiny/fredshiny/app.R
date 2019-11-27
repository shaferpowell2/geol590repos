#########
# FRED Shiny app by Thomas Geissberger & Shafer Powell
#########

library(shiny)
library(FRED1pkg) # package containing the FRED dataset. Downloadable from Github at https://github.com/shaferpowell2/geol590repos/tree/master/FRED1pkg
library(tidyverse) #so that pipe function works

#Subset FRED for simpler display
set.seed(50)
fred1subset <- fred1 %>%
  select(Belowground.part,Plant.taxonomy_Family_TPL, Accepted.genus_TPL, Accepted.species_TPL, Notes_In.situ..pot..or.hydroponic, Root.diameter, Belowground.biomass.per.ground.area, Root.length.density..RLD._Root.length.per.ground.area, Root.C.N.ratio)


ui <- fluidPage(
  navbarPage("FRED 1",

############# Download/table tab

             tabPanel("Data Download",

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
  DT::dataTableOutput("table")
),

######### Interactive graph tab

tabPanel("Interactive Graph",
         # Application title
         titlePanel('FRED 1 plot'),

         # Sidebar with 2 select inputs and a numeric input
         sidebarPanel(
           selectInput('xCol', 'X', names(fred1)),
           selectInput('yCol', 'Y', names(fred1))),

         # Shows the plot
         mainPanel(plotOutput('plot'))
)))

server <- function(input, output) {

############# Download/table tab

  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    #renderDataTable is used instead of renderTable because the former vertically separates the table into manageable pages.
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
    filename = "fred1_selected_download.csv",
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )

############# Interactive graph tab

  # Get the data from the variables declared on the ui.R file
  df <- reactive({fred1[, c(input$xCol, input$yCol)]})

  # Create the plot
  output$plot <- renderPlot({plot(df(), pch = 20, cex = 3, col = "blue",
                                  main = "FRED 1 plot")})
}


shinyApp(ui, server)
