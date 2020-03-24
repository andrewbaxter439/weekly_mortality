library(shiny)
library(SPHSUgraphs)
library(readr)
library(dplyr)
library(lubridate)
library(nlme)
library(plotly)
`-.gg` <- function(e1, e2) e2(e1)

printCoefficients <- function(model){
  as_tibble(trimws(format(round(summary(model)$tTable, 3), nsmall=3))) %>%
    mutate(Coefficient = rownames(summary(model)$tTable)) %>% 
    select(Coefficient, Value, Std.Error, 'p-value') %>% 
    print()
}

testAutocorr <- function(model, data=NULL, max.lag = 10, time.points = 25) {
  # data <- eval(model$call$data)
  par(cex = 0.7, mai = c(0.1, 0.1, 0.2, 0.1))
  # par(fig = c(0.03, 1, 0.8, 1))
  # plot(
  #   data$Year[1:time.points],
  #   residuals(model)[1:time.points],
  #   type = 'o',
  #   pch = 16,
  #   col = "red"
  # )
  
  par(fig = c(0.03, 0.5, 0.05, 0.9), new = TRUE)
  acf(residuals(model, type = "normalized"))
  
  par(fig = c(0.55, 1, 0.05, 0.9), new = TRUE)
  acf(residuals(model, type = "normalized"), type = 'partial')
}

data <- readRDS("data/imported_data.rds")


server <- function(input, output) {
  
  # inputs -----------------------------------------------------------------------------------------------------
  
  
  output$hello <- renderText("hello")
  week_start <- reactive({floor((input$int1date - dmy("01/01/2010"))/dweeks(1))})
  
  output$obsRange <- renderUI({
    dateRangeInput("dateRange",
                   "Dates to observe: ",
                   # start = "2010-01-01",
                   # end = "2019-08-01",
                   # min = "2010-01-01",
                   # max = "2019-08-01")
                   start = min(dfpre()$Date),
                   end = max(dfpre()$Date),
                   min = min(dfpre()$Date),
                   max = max(dfpre()$Date))
  })
  
  # dataframe --------------------------------------------------------------------------------------------------
  
  # df <- reactive({
  #   data %>% 
  #     filter(Sex == tolower(input$group)) %>% 
  #     arrange(Year, Week) %>% 
  #     mutate(Time = 1:nrow(.)) %>% 
  #     mutate(Date = dmy("01/01/2010") + weeks(Time)) %>% 
  #     mutate(Int1 = ifelse(Date<=input$int1date, 0, 1),
  #            Trend1 = c(rep(0, week_start()), 1:(max(Time)-week_start()))) %>% 
  #     # Including end/beginning markers
  #     group_by(Year) %>% 
  #     mutate(endyr = 100*exp(Week-ifelse(max(Week) == 53, 55, 54))+1,
  #            begyr = 100*exp(-Week-2)+1) %>% 
  #     ungroup() %>% 
  #     filter(Date >= input$dateRange[1], Date <= input$dateRange[2])
  # })
  
  dfpre <- reactive({
    data %>%
      filter(Sex == tolower(input$group)) %>%
      arrange(Year, Week) %>%
      mutate(Time = 1:nrow(.)) %>%
      mutate(Date = dmy("01/01/2010") + weeks(Time)) %>%
      mutate(Int1 = ifelse(Date<=input$int1date, 0, 1),
             Trend1 = c(rep(0, week_start()), 1:(max(Time)-week_start()))) %>%
      # Including end/beginning markers
      group_by(Year) %>%
      mutate(endyr = 100*exp(Week-ifelse(max(Week) == 53, 55, 54)),
             begyr = 100*exp(-Week-2)) %>%
      ungroup()
  })
  
  df <- reactive({filter(dfpre(), Date >= input$dateRange[1], Date <= input$dateRange[2])})
  
  
  md <- reactive({
    req(input$int1date, input$group, input$dateRange)
    gls(adj_rate ~ Time + Int1 + Trend1+ cos((Time-4.6)*pi*2/52.14)  + endyr + begyr, data = df(), correlation = corARMA(p=input$p, q = input$q, form = ~ Time), method = "ML")
  })

    md_null_q <- reactive({
    req(input$int1date, input$group, input$dateRange)
    gls(adj_rate ~ Time + Int1 + Trend1+ cos((Time-4.6)*pi*2/52.14)  + endyr + begyr, data = df(), correlation = corARMA(p=input$p, q = 0, form = ~ Time), method = "ML")
  })
    
    md_null_p <- reactive({
    req(input$int1date, input$group, input$dateRange)
    gls(adj_rate ~ Time + Int1 + Trend1+ cos((Time-4.6)*pi*2/52.14)  + endyr + begyr, data = df(), correlation = corARMA(p = 0, q = input$q, form = ~ Time), method = "ML")
  })

        md_p1q1 <- reactive({
    req(input$int1date, input$group, input$dateRange)
    gls(adj_rate ~ Time + Int1 + Trend1+ cos((Time-4.6)*pi*2/52.14)  + endyr + begyr, data = df(), correlation = corARMA(p = 1, q = 1, form = ~ Time), method = "ML")
  })
  
  md_null <- reactive({
    req(input$int1date, input$group, input$dateRange)
    gls(adj_rate ~ Time + Int1 + Trend1+ cos((Time-4.6)*pi*2/52.14)  + endyr + begyr, data = df(), correlation = NULL, method = "ML")
  })
  
  # lmd <- reactive({
  #   lm(adj_rate ~ Time + Int1 + Trend1 + cos((Time-4.6)*pi*2/52) + endyr + begyr, data = df())
  # })
  # 
  cfac <- reactive({
    df() %>% mutate(Predict = md()$coefficients[1] +
                      md()$coefficients[2] * Time) %>% 
      filter(Date>input$int1date)
  })
  
  output$graph <- renderPlotly({
    req(input$int1date, input$group, input$dateRange)
    plot <- df() %>% 
      mutate(
        # Predict minus seasonal corrections
        # Predict = md()$coefficients[1] +
        #   md()$coefficients[2] * Time +
        #   md()$coefficients[3] * Int1 +
        #   md()$coefficients[4] * Trend1 +
        #   md()$coefficients[5] * cos((Time-5)*pi*2/52),
        Predict = predict(md(), newdata = df()),
        lineTrend = md()$coefficients[1] +
          md()$coefficients[2] * Time +
          md()$coefficients[3] * Int1 +
          md()$coefficients[4] * Trend1
      ) %>% 
      ggplot(aes(Date, adj_rate)) +
      geom_point(aes(text = paste0("Week ending: ", Date, "<br>Week no: ", Week, "<br>Adjusted rate: ", round(adj_rate, 2)))) +
      geom_line(data = cfac(), aes(Date, Predict, col = "Predicted"), linetype = "dashed", size = 1.5) +
      geom_line(aes(y = Predict, group = Int1, col = "Seasonal trend"), size = 1.5, alpha = 0.8) +
      geom_line(aes(y = lineTrend, group = Int1, col = "Trend"), size = 1.5, alpha = 0.8) +
      geom_vline(xintercept = as.numeric(input$int1date),
                 linetype = "dotted",
                 col = "#000000CC"
      ) +
      scale_colour_manual(name = "",
                          values = c("Trend" = sphsu_cols("Thistle", names = FALSE),
                                     "Seasonal trend" = sphsu_cols("University Blue", names = FALSE),
                                     "Predicted" = sphsu_cols("Pumpkin", names = FALSE))) +
      theme_sphsu_light()+
      ylab("Age-standardised mortality rate - deaths per 100,000")
    
    ggplotly(plot, tooltip = "text")
    
  })
  
  coefs <- reactive(printCoefficients(md()))
  
  output$coefs <- renderDataTable(coefs(), options = list(searching = FALSE, paging = FALSE))
  

# autocorrelation bits ---------------------------------------------------------------------------------------

  
  output$autocorr_null <- renderPlot({req(input$autocorr_show); testAutocorr(md_null())})
  output$autocorr <- renderPlot({req(input$autocorr_show); testAutocorr(md())})
  
  output$AC1title <- renderText({req(input$autocorr_show); "Autocorrelation detection (non-corrected plots):"})
  
  output$ac_corr <- renderText({
    req(input$autocorr_show)
    paste0("Autocorrelation Correction (", modelText(), ")")
  })

  modelText <- reactive({paste0(
    ifelse(input$p!=0, paste0("AR", input$p, " "), ""),  ifelse(input$q!=0, paste0("MA", input$q, " "), ""), "autocorrelation model")
  })
    
    output$anova1_title <- renderText({req(input$autocorr_show && (input$p+input$q) != 0); paste0("ANOVA comparison: null-autocorrelation model and ", modelText())})
    
  output$anova1 <- renderDataTable(options = list(searching = FALSE, paging = FALSE), {
    req(input$autocorr_show && (input$p+input$q) != 0)
    df <- anova(md_null(), md()) %>% 
      mutate(call = c("Null autocorrelation model", modelText()))
  })

  
  null_q_text <- reactive({paste0(paste0("AR", input$p, " "), "MA0")})
    output$anova2_title <- renderText({req(input$autocorr_show && input$q!=0 && input$p!=0); paste0("ANOVA comparison: ", modelText(), " and ", null_q_text())})
  
    output$anova2 <- renderDataTable(options = list(searching = FALSE, paging = FALSE), {
      req(input$q!=0  && input$p!=0)
    req(input$autocorr_show)
    df <- anova(md_null_q(), md_null()) %>% 
      mutate(call = c(modelText(), null_q_text()))
  })
    
    null_p_text <- reactive({paste0("AR0 ", paste0("MA", input$q, " "), "")})
    output$anova3_title <- renderText({req(input$autocorr_show && input$p!=0 && input$q!=0); paste0("ANOVA comparison: ", modelText(), " and ", null_p_text())})
  
    output$anova3 <- renderDataTable(options = list(searching = FALSE, paging = FALSE), {
      req(input$p!=0 && input$q!=0)
    req(input$autocorr_show)
    df <- anova(md_null_p(), md_null()) %>% 
      mutate(call = c(modelText(), null_p_text()))
  })
  


    output$anova4_title <- renderText({req(input$autocorr_show && input$p==1 && input$q==3); paste0("ANOVA comparison: ", modelText(), " and AR1, MA1")})
  
    output$anova4 <- renderDataTable(options = list(searching = FALSE, paging = FALSE), {
      req(input$p==1 && input$q==3)
    req(input$autocorr_show)
    df <- anova(md_null_p(), md_p1q1()) %>% 
      mutate(call = c(modelText(), "AR1, MA1"))
  })
  # temp bits --------------------------------------------------------------------------------------------------
  
  output$data_table <- renderDataTable({req(input$int1date, input$group, input$dateRange)
    df()})  
  
  
}