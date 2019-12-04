#########
# FRED Shiny app by Thomas Geissberger & Shafer Powell
#########

library(shiny)
library(FRED1pkg) # package containing the FRED dataset. Downloadable from Github at https://github.com/shaferpowell2/geol590repos/tree/master/FRED1pkg
library(tidyverse) #so that pipe function and ggplot work

#Subset FRED for simpler display
fred1subset <- fred1 %>%
  select(Belowground.part,Plant.taxonomy_Family_TPL, Accepted.genus_TPL, Accepted.species_TPL, Notes_In.situ..pot..or.hydroponic, Root.diameter, Belowground.biomass.per.ground.area, Root.length.density..RLD._Root.length.per.ground.area, Root.C.N.ratio)

#Create separate FRED subset for graph
fred1graph <- fred1 %>%
  select(Root.order,Root.diameter, Latitude_main, Root.C.N.ratio, Root.decomposition_Annual.k.constant, Root.median.lifespan_d, Belowground.biomass.per.ground.area,Root.tissue.density..RTD.)

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
         titlePanel('FRED 1 plot (X axis jittered)'),

         # Sidebar with 2 select inputs and a numeric input
         sidebarPanel(
           selectInput('xCol', 'X', c("Root.order","Root.diameter", "Latitude_main")),
           selectInput('yCol', 'Y', c("Root.C.N.ratio", "Root.tissue.density..RTD.", "Root.median.lifespan_d", "Belowground.biomass.per.ground.area"))
           ),

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

  # Create the plot
  output$plot <- renderPlot({
    ggplot(data = fred1graph) +
      aes_string(x = input$xCol, y = input$yCol) +
      geom_point(position = position_jitter(height = 0))
  })
}


shinyApp(ui, server)
