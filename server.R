library(shiny)
library(SPHSUgraphs)
library(readr)
library(dplyr)
library(lubridate)

printCoefficients <- function(model){
  as_tibble(trimws(format(round(summary(model)$tTable, 3), nsmall=3))) %>%
    mutate(Coefficient = rownames(summary(model)$tTable)) %>% 
    select(Coefficient, Value, Std.Error, 'p-value') %>% 
    print()
}

testAutocorr <- function(model, data=NULL, max.lag = 10, time.points = 25) {
  # data <- eval(model$call$data)
  # par(cex = 0.7, mai = c(0.1, 0.1, 0.2, 0.1))
  # par(fig = c(0.03, 1, 0.8, 1))
  # plot(
  #   data$Year[1:time.points],
  #   residuals(model)[1:time.points],
  #   type = 'o',
  #   pch = 16,
  #   col = "red"
  # )
  
  par(fig = c(0.03, 0.5, 0.05, 0.75), new = TRUE)
  acf(residuals(model))
  
  par(fig = c(0.55, 1, 0.05, 0.75), new = TRUE)
  acf(residuals(model), type = 'partial')
}

server <- function(input, output) {
  data <- read_csv("imported data.csv")

  output$hello <- renderText("hello")
  week_start <- reactive({floor((input$int1date - dmy("01/01/2010"))/dweeks(1))})
  
  df <- reactive({
    data %>% 
      filter(Sex == tolower(input$group)) %>% 
      arrange(Year, Week) %>% 
      mutate(Time = 1:nrow(.)) %>% 
      mutate(Date = dmy("01/01/2010") + weeks(Time)) %>% 
      mutate(Int1 = ifelse(Date<=input$int1date, 0, 1),
             Trend1 = c(rep(0, week_start()), 1:(max(Time)-week_start())))
  })
  
    

    md <- reactive({
      gls(adj_rate ~ Time + Int1 + Trend1 + cos((Time-5)*pi*2/52) , data = df(), method = "ML")
    })
    
    lmd <- reactive({
      lm(adj_rate ~ Time + Int1 + Trend1 + cos((Time-5)*pi*2/52) , data = df())
    })
    
    cfac <- reactive({
      df() %>% mutate(Int1 = 0, Trend1 = 0,
                          Predict = md()$coefficients[1] +
                            md()$coefficients[2] * Time) %>% 
      filter(Date>input$int1date)
    })
    
  output$graph <- renderPlot({
    df() %>% 
      mutate(Predict = predict(md(), newdata = df()),
             lineTrend = md()$coefficients[1] +
               md()$coefficients[2] * Time +
               md()$coefficients[3] * Int1 +
               md()$coefficients[4] * Trend1
      ) %>% 
      ggplot(aes(Date, adj_rate)) +
      geom_point() +
      geom_line(data = cfac(), aes(Date, Predict, col = "Predicted"), linetype = "dashed", size = 1.5) +
      geom_line(aes(y = Predict, group = Int1, col = "Seasonal trend"), size = 1.5, alpha = 0.8) +
      geom_line(aes(y = lineTrend, group = Int1, col = "Trend"), size = 1.5, alpha = 0.8) +
      geom_vline(xintercept = input$int1date,
                 linetype = "dotted",
                 col = "#000000CC") +
      scale_colour_manual(values = c("Trend" = sphsu_cols("Thistle", names = FALSE),
                                     "Seasonal trend" = sphsu_cols("University Blue", names = FALSE),
                                     "Predicted" = sphsu_cols("Pumpkin", names = FALSE))) +
      theme_sphsu_light()
  })
  
  coefs <- reactive(printCoefficients(md()))
  
  output$coefs <- renderDataTable(coefs(), options = list(searching = FALSE, paging = FALSE))
  output$autocorr <- renderPlot({testAutocorr(lmd())})
}