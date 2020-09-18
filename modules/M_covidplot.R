# This module generates a plot with tag and download information,
# with a download button underneath.
#
# You use as follows:
# covidplotUI("myplot")
#
# theplot <- reactive({ggplot() etc etc})
#
# covidplotServer("myplot",
#                 plot = theplot,
#                 filename = "defaultname.png")


library(shiny)

covidplotUI <- function(id){
  ns <- NS(id)
  
  tagList(
    plotOutput(ns("theplot")),
    downloadButton(ns("plotdownload"),
                   label = "Download")
  )
}

covidplotServer <-function(id, theplot, filename = "plot.png"){
  moduleServer(
    id,
    function(input, output, session){
      
      fullplot <- reactive({
        theplot() + 
          caption +
          theme(plot.tag.position = "bottomright")
      })
      
      output$renderPlot({
        fullplot()
      })
      
      output$plotdownload <- downloadHandler(
        filename = filename,
        content = function(x){
          req(fullplot())
          p <- fullplot()
          ggsave(x, p, width = 8, height = 5)
        }
      )
    } # END server function
  ) # END moduleServer
} #END covidplotServer
  
