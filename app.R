#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("How to compare two distribution"),

    # Sidebar with a slider input for number of bins 
    fluidRow(column(6, selectInput('type_distr1', 'Type of distribution 1', choices = c('Normal', 'LogNormal')),
                                   numericInput('size_distr1', 'Size of the distribution 1', min = 10, max = 1000,
                                                value = 100),
                    actionButton('generate1', 'Generate distribution 1'),
                    plotOutput('plot_distr1', width = "500px")),
             column(6, selectInput('type_distr2', 'Type of distribution 2', choices = c('Normal', 'LogNormal')),
                    numericInput('size_distr2', 'Size of the distribution 2', min = 10, max = 1000, value = 100),
                    actionButton('generate2', 'Generate distribution 2'),
                    plotOutput('plot_distr2', width = "500px")))
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$generate1, {
    distr1 <- rnorm(input$size_distr1)
    output$plot_distr1 <- renderPlot(ggplot(as.data.frame(distr1), aes(distr1))+
                                           geom_histogram(bins = 20)+theme_bw())
  })
  observeEvent(input$generate2, {
    distr2 <- rnorm(input$size_distr2)
    output$plot_distr2 <- renderPlot(ggplot(as.data.frame(distr2), aes(distr2))+
                                       geom_histogram(bins = 20)+theme_bw())
  })
   # <- hist(distr1)
    # ggplot(as.data.frame(distr1), aes(distr1))+
    # geom_histogram(bins = 20)
}

# Run the application 
shinyApp(ui = ui, server = server)
