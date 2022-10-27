#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
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
                    plotOutput('plot_distr1', width = "500px"),
                    sliderInput('bin1', 'change bins', min = 2, max = 100, value = 20)),
             column(6, selectInput('type_distr2', 'Type of distribution 2', choices = c('Normal', 'LogNormal')),
                    numericInput('size_distr2', 'Size of the distribution 2', min = 10, max = 1000, value = 100),
                    actionButton('generate2', 'Generate distribution 2'),
                    plotOutput('plot_distr2', width = "500px"),
                    sliderInput('bin2', 'change bins', min = 2, max = 100, value = 20))),
    h3("Which test will you use to compare this distribution?", align = "center"),
    selectInput('tests_to_compare', 'Select test', choices = c('t-test', 'Mann-Whitney test',
                                                               'Monte-Carlo test', 'bootstrap')),
    actionButton('compare', 'Compare!'),
    textOutput('result')
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  distr1_data <- reactiveValues()
  observeEvent(input$generate1, {
    distr1_data$distr1 <- rnorm(input$size_distr1)
    output$plot_distr1 <- renderPlot(ggplot(as.data.frame(distr1_data$distr1), aes(distr1_data$distr1))+
                                           geom_histogram(bins = (input$bin1))+theme_bw())
  })
  distr2_data <- reactiveValues()
  observeEvent(input$generate2, {
    distr2_data$distr2 <- rnorm(input$size_distr2)
    output$plot_distr2 <- renderPlot(ggplot(as.data.frame(distr2_data$distr2), aes(distr2_data$distr2))+
                                       geom_histogram(bins = input$bin2)+theme_bw())
  })
  output$compare <- renderText('Which test will you use to compare this distribution?')
   # <- hist(distr1)
    # ggplot(as.data.frame(distr1), aes(distr1))+
    # geom_histogram(bins = 20)
  # cat(distr1_data$distr1)
  observeEvent(input$compare, {
    res <- t.test(distr1_data$distr1, distr2_data$distr2)
    output$result <- renderText(res$p.value)
    # case_when(input$tests_to_compare == 't-test' ~
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
