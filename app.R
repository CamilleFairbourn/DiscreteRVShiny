#Binomial and Poisson Distribution Applet

library(shiny)

ui <- fluidPage(title = "Discrete Probability Distributions",
                navlistPanel(
                  tabPanel(title = "Binomial distribution",
                           numericInput("binomn", label = "Number of trials", value = 5, min = 1, max = 100),
                           numericInput("binomp", label = "Probability =", value = 0.5, min = 0, max = 1, step = 0.01),
                           plotOutput("binomplot")
                  ),
                  tabPanel(title = "Poisson distribution",
                           numericInput(inputId = "lam", 
                                        label = "Choose a number for lambda", 
                                        value = 25, min = 0.1, max = 50),
                           plotOutput("poissonbar1")#,
                           # sliderInput(inputId = "lam2",
                           #             label = "Set a value for lamda",
                           #             min = 0.1, max = 100, value = 25),
                           # plotOutput("poissonbar2")
                  )
                )
  
  
        )



server <- function(input, output) {
  output$poissonbar1 <- renderPlot({
     req(input$lam)
     barplot(dpois(seq(0, 3*input$lam,1), input$lam, log=FALSE), 
             names.arg=seq(0, 3*input$lam,1), col="#F3A176", 
             cex.axis=2,cex.lab=2,cex.main=2, cex.names = 2,
             main=bquote(paste("Poisson, ", lambda, " = ", .(input$lam))))
  })
  # output$poissonbar2 <- renderPlot({
  #   barplot(dpois(seq(0, 3*input$lam2,1), input$lam2, log=FALSE), names.arg=seq(0, 3*input$lam2,1), col="#F3A176", 
  #           main=bquote(paste("Poisson, ", lambda, "= ", .(input$lam2))))
  # })
  output$binomplot <- renderPlot({
    req(input$binomn)
    req(input$binomp)
    barplot(dbinom(seq(0, input$binomn, 1), input$binomn, input$binomp), 
            names.arg = seq(0, input$binomn, 1),col = "#9999FF", 
            cex.axis=2,cex.lab=2,cex.main=2, cex.names = 2,
            main = bquote(paste("Binomial, n = ", .(input$binomn), ", p = ", .(input$binomp))))
    
  })
}


shinyApp(ui = ui, server = server)