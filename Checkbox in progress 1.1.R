library(shiny)
library(FRED1pkg)  # for the diamonds dataset

ui <- fluidPage(
  title = "Examples of DataTables",
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "fred1"',
        checkboxGroupInput("show_vars", "Columns in fred1 to show:",
                           c("Below Ground Biomas per Ground Area" = "Belowground.biomass.per.ground.area",
                             "Root C/N Ratio" = "Root.C/N.ratio",
                             "Root C Content" = "Root.C.content",
                             "Root N Content" = "Root.N.content",
                             "Root Diameter"  = "Root.diameter"))
      ),

    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("fred1", DT::dataTableOutput("mytable1"))
      )
    )
  )
)

server <- function(input, output) {

  # choose columns to display
  fred2 = fred1[sample(nrow(fred1), 1000), ]
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(fred2[, input$show_vars, drop = FALSE])
  })
}







shinyApp(ui, server)
