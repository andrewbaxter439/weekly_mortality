library(shiny)
library(SPHSUgraphs)
library(readr)
library(dplyr)
library(lubridate)
library(nlme)
library(plotly)
library(rvest)
library(httr)
library(readxl)
library(tidyr)
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
  

# check for new data ------------------------------------------------------

  links <- read_html("https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales") %>% 
    html_nodes("a") %>% 
    html_attr("href")
  part_url <- links[which(grepl("englandandwales%2f2020/publishedweek", links))]
  
  weekpb <- as.numeric(gsub("^.*publishedweek(\\d{2})2020.xls", "\\1", part_url))
  
  max_2020 <- data %>% 
    filter(Year == 2020) %>% 
    summarise(maxw = max(Week)) %>% 
    pull(maxw)
  
  if (max_2020 < weekpb){
    links <- read_html("https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales") %>% 
      html_nodes("a") %>% 
      html_attr("href")
    part_url <- links[which(grepl("englandandwales%2f2020/publishedweek", links))]
    
    GET(url = paste0("https://www.ons.gov.uk", part_url), write_disk(tf <- tempfile(fileext = ".xls")))
    import_2020 <- read_xls(tf, sheet = 4, skip = 2)
    unlink(tf)
    
    colnames(import_2020)[2] <- "Age"
    
    start <- which(grepl("Males", import_2020$Age))[1] + 2
    end <- start + 6
    
    import_2020[start:end, "Sex"] <- "male"
    
    start <- which(grepl("Females", import_2020$Age))[1] + 2
    end <- start + 6
    
    import_2020[start:end, "Sex"] <- "female"
    
    start <- which(grepl("Persons", import_2020$Age))[1] + 2
    end <- start + 6
    
    import_2020[start:end, "Sex"] <- "all"
    
    import_2020[, "Year"] <- 2020
    
    dat_2020 <- import_2020 %>% 
      select(Year, Sex, Age, '1':ncol(.)) %>% 
      filter(!is.na(Sex)) %>% 
      select_if(~!all(is.na(.x))) %>% 
      mutate_at(-1:-3, ~ as.numeric(.x)) %>% 
      pivot_longer(names_to = "Week", values_to = "deaths", cols = -1:-3) %>% 
      mutate(Week = as.numeric(Week)) %>% 
      filter(!is.na(deaths),
             Week>max_2020) %>% 
      mutate_at(c(1,4,5), function(x) as.numeric(x))
    
    weights <- read_csv("european_standard_population.csv") %>% 
      group_by(Group) %>% 
      summarise(wgt = sum(EuropeanStandardPopulation)) %>% 
      mutate(Age = c("Under 1 year", "01-14", "15-44", "45-64", "65-74", "75-84", 
                     "85+"))
    
    pop_2020 <- readRDS("data/pop_estimates.rds")
    
    data <- dat_2020 %>% 
      full_join(pop_2020, by = c("Year", "Age", "Sex")) %>% 
      full_join(weights, by = c("Age")) %>% 
      mutate(rate_crude = deaths/pop, 
             expected_deaths = rate_crude * wgt) %>% 
      group_by(Sex, Year, Week) %>% 
      summarise(adj_rate = sum(expected_deaths, na.rm = TRUE)) %>% 
      ungroup() %>% 
      bind_rows(data)
    
  }
  
  
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
  

# models and arma corrected modelts ---------------------------------------

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
  

# output plot -------------------------------------------------------------

  
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
      ylab("Age-standardised mortality rate - deaths per 100,000") +
      scale_x_date(date_breaks = "2 years", date_labels = "%Y")
    
    hide("loading-content", anim = TRUE, animType = "fade")
    ggplotly(plot, tooltip = "text")
    
  })
  

    output$ggraph <- renderPlot({
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
    
    hide("loading-content", anim = TRUE, animType = "fade")
    plot
    
  })
  
  coefs <- reactive(printCoefficients(md()))
  
  output$coefs <- DT::renderDataTable(coefs(), options = list(searching = FALSE, paging = FALSE))
  

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
    
  output$anova1 <- DT::renderDataTable(options = list(searching = FALSE, paging = FALSE), {
    req(input$autocorr_show && (input$p+input$q) != 0)
    df <- anova(md_null(), md()) %>% 
      mutate(call = c("Null autocorrelation model", modelText()))
  })

  
  null_q_text <- reactive({paste0(paste0("AR", input$p, " "), "MA0")})
    output$anova2_title <- renderText({req(input$autocorr_show && input$q!=0 && input$p!=0); paste0("ANOVA comparison: ", modelText(), " and ", null_q_text())})
  
    output$anova2 <- DT::renderDataTable(options = list(searching = FALSE, paging = FALSE), {
      req(input$q!=0  && input$p!=0)
    req(input$autocorr_show)
    df <- anova(md(), md_null_q()) %>% 
      mutate(call = c(modelText(), null_q_text()))
  })
    
    null_p_text <- reactive({paste0("AR0 ", paste0("MA", input$q, " "), "")})
    output$anova3_title <- renderText({req(input$autocorr_show && input$p!=0 && input$q!=0); paste0("ANOVA comparison: ", modelText(), " and ", null_p_text())})
  
    output$anova3 <- DT::renderDataTable(options = list(searching = FALSE, paging = FALSE), {
      req(input$p!=0 && input$q!=0)
    req(input$autocorr_show)
    df <- anova(md(), md_null_p()) %>% 
      mutate(call = c(modelText(), null_p_text()))
  })
  


    output$anova4_title <- renderText({req(input$autocorr_show && input$p==1 && input$q==3); paste0("ANOVA comparison: ", modelText(), " and AR1, MA1")})
  
    output$anova4 <- DT::renderDataTable(options = list(searching = FALSE, paging = FALSE), {
      req(input$p==1 && input$q==3)
    req(input$autocorr_show)
    df <- anova(md_null_p(), md_p1q1()) %>% 
      mutate(call = c(modelText(), "AR1, MA1"))
  })
  # temp bits --------------------------------------------------------------------------------------------------
  
  output$data_table <- DT::renderDataTable({req(input$int1date, input$group, input$dateRange)
    df()})  
  
  
}