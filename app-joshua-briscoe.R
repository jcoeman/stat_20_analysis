# ===============================================
# Fill in the following fields
# ===============================================
# Title: Project 2 Joshua Briscoe
# Description:
# Author: Joshua Briscoe
# Date:


# ===============================================
# Required packages
# ===============================================
library(tidyverse)
library(quantreg)
library(outliers)
# ...



# ===============================================
# Define UserInterface "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("Retirement Withdrawal Simulator"),
  fluidRow(
    # Inputs for initial portfolio, retirement age, and withdrawal rate
    column(3,
           numericInput(inputId = "initial", label = ("Initial Amount"), value = 1000),
           numericInput(inputId = "retirement_age", label = "Retirement Age", value = 65),
           sliderInput(inputId = "withdrawal_rate", label = ("Withdrawal Rate(%)"), min = 0, 
                       max = 0.15, value = 0.04)
           
    ),
    
    # Inputs for mean and standard deviation of annual return rates
    column(3,
           sliderInput(inputId = "mean_return_rate", label = "Mean Rate of Return(%)", min = 0, 
                       max = 0.15, value = 0.1),
           sliderInput(inputId = "mean_volatility", label = "Average return volatility(%)", min = 0, 
                       max = 0.15, value = 0.09)
    ),
    
    # Inputs for mean and standard deviation of annual inflation rates
    column(3,
           sliderInput(inputId = "mean_inflation", label = "Average mean inflation(%)", min = 0, 
                       max = 0.15, value = .06),
           sliderInput(inputId = "sd_inflation", label = "inflation volatility(%)", min = 0, 
                       max = 0.15, value = .09)
    ),
    
    # Inputs for number of simulations, and random seed
    column(3,
           numericInput(inputId = "sims", label = ("Number of Simulations"), value = 50),
           numericInput(inputId = "seed", label = ("Random Seed"), value = 12345)
           )
  ),
  fluidRow(
    column(6, h4('Simulations'), h6('Orange = 10th and 90th percentile lines, Red = Median'), plotOutput('plot'), h4('Final Year Box Plot'), plotOutput('BoxPlot')), 
    column(6, h4('Final Year: Projected Earnings'), h6('Red = mean, Orange = median'), plotOutput('plot2'), h4('Probability Histogram'), plotOutput('ProbabilityHistogram'), h4('Probability of Running out of Money'), h6('Assumes the distribution of earnings in the final year is normally distributed.'), verbatimTextOutput('table2'))),
  
  hr(),
  h2('Statistics'),
  fluidRow(
    column(6, h3('Measures of Spread'), h4('Final Year Standard Deviation'), verbatimTextOutput('SDPlot'), h4('Final Year IQR'), verbatimTextOutput('IQR'), h4('Final Year Range'), verbatimTextOutput('range')), 
    column(6,h3('Measures of Central Tendency'),
           h4(paste('Final Year Mean'), verbatimTextOutput('MeanPlot'),
           h4('Final Year Median'), verbatimTextOutput('MedianPlot')
           
           ))))



# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {

  # you may need to create reactive objects
  # (e.g. data frame to be used for graphing purposes)
  
  dat <- reactive({
    # replace the code below with your code!!!
    years <- 101 - input$retirement_age
    sims <- input$sims
    initial <- input$initial
    data_matrix <- matrix(input$initial, nrow = years, ncol = input$sims)

    for(s in 0:sims){
      initial = input$initial
      for (t in 1:(years)){
        rate <- rnorm(1, mean = input$mean_return_rate, sd = input$mean_volatility)
        inflation <- rnorm(1, mean = input$mean_inflation, sd = input$sd_inflation)
        
        ab = initial*(1+rate) - (input$withdrawal_rate*input$initial)*(1 + inflation)
        data_matrix[t,s] = ab
        initial = ab
      }
    }
    #assemble data frame
    portfolios = as.data.frame(data_matrix)
    colnames(portfolios) <- paste0("sim", 1:input$sims)
    y = 100 - input$retirement_age
    portfolios$year = 0:y
    
    pivot_longer(
      portfolios, 
      cols = starts_with("sim"),
      names_to = "simulation",
      values_to = "amount")
    })
  
  # code for graph
  # (e.g. reactive data frame used for graphing purposes)
  
  
  output$plot <- renderPlot({
    # replace the code below with your code!!!
    ggplot(data = dat(), aes(x = year, y = amount)) +
      geom_line(color = 'light blue', aes(group = simulation, colour=(amount <= 0))) +
      theme_bw() + theme(legend.position = 'none') +
      labs(x = "Years Since Retirement", y = "Balance") +
      geom_quantile(quantiles = 0.5, color = 'red', size = 1.2, method = "rqss") +
     geom_quantile(quantiles = 0.9, color = "gold", size = 1.2, method = "rqss") + 
    geom_quantile(quantiles = 0.1, color = "gold", size = 1.2, method = "rqss") + geom_hline(yintercept = 0)
  })
  
  f_dist <- reactive(data.frame(dat() %>% filter(year == 100 - input$retirement_age))$amount)
  
  output$plot2 <- renderPlot({ggplot(data = filter(dat(), year == 100 - input$retirement_age)) +
      geom_histogram(fill = 'light blue', color = "black", aes(x = amount)) +
      theme_bw() + labs(x = 'Earnings', y = 'Count') + geom_vline(xintercept = 0) + geom_vline(xintercept = mean(f_dist()), color = 'gold', size = 1.2) + geom_vline(xintercept = median(f_dist()), color = 'red', size = 1.2)
      })
  
  
  # code for statistics
  
  output$table2 <- renderPrint({
    pnorm(0, mean = mean(f_dist()), sd = sd(f_dist()))
  })
  
  output$MeanPlot <- renderPrint({
    mean(f_dist())
      })
  
  output$MedianPlot <- renderPrint({
    median(f_dist())})
    
    output$SDPlot <- renderPrint({
      sd(f_dist())})
    
    output$BoxPlot <- renderPlot({
      ggplot(data = filter(dat(), year == 100 - input$retirement_age) , aes(x = amount)) +
        geom_boxplot(fill = 'light blue', color = "black") +
        theme_bw() + labs(x = 'Earnings', y = 'Count') + geom_vline(xintercept = 0)
    })
    
    output$ProbabilityHistogram <- renderPlot({ggplot(data = filter(dat(), year == 100 - input$retirement_age)) +
        geom_histogram(fill = 'light blue', color = "black", aes(x = amount, y = ..density..)) +
        theme_bw() + labs(x = 'Earnings', y = 'Count') + geom_vline(xintercept = 0) +
        stat_function(fun = dnorm, size = 1.2, args = list(mean = mean(data.frame(filter(dat(), year == 100 - input$retirement_age))$amount), sd = sd(data.frame(filter(dat(), year == 100 - input$retirement_age))$amount)))
    })
    
    output$IQR <- renderPrint({
      IQR(f_dist())
    })
  
    output$range <- renderPrint({
      range(f_dist())
    })

}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

