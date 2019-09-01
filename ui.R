library(plotly)
ui <- fluidPage(
   withMathJax(),  
   fluidRow(
      column(4,
             selectInput("group",
                         "Group to compare: ",
                         c("All", "Male", "Female"),
                         selected = "All"),
             align = "center"),
      column(4, dateInput("int1date",
                          "Select Exposure start date: ",
                          value = "2016-01-01"),
             align = "center"),
      column(4, uiOutput("obsRange"), align = "center")
   ),
   fluidRow(
      plotlyOutput("graph")),
   br(),
   fluidRow(column(12, align = "center", "Equation: $$Rate = \\beta_0+\\beta_1*Time+\\beta_2*Intervention_1+\\beta_3*Trend_1+\\beta_4*cos(Time)$$")),
   fluidRow(dataTableOutput("coefs")),
   br(),
   # column(4, h5("Autocorrelation detection (non-corrected plots):")),
   # column(4, numericInput("p", "AR: ", 1, min = 0)),
   # column(4, numericInput("q", "MA: ", 0, min = 0)),
   # fluidRow(plotOutput("autocorr")),
   br(),
   # fluidRow(column(12, h5(textOutput("ac_corr")))),
   # fluidRow(plotOutput("autocorr_gls")),
   # htmlOutput("corrcompare"),
   dataTableOutput("data_table"),
   p()
)
