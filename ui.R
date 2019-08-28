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
           value = "2018-01-01"),
       align = "center"),
column(4, uiOutput("obsRange"), align = "center")
 ),
 fluidRow(
 plotlyOutput("graph"),
 br(),
 p("Equation: $$Rate = \\beta_0+\\beta_1*Time+\\beta_2*Intervention_1+\\beta_3*Trend_1+\\beta_4*cos(Time)$$"),
 dataTableOutput("coefs"),
 plotOutput("autocorr")
 )
)