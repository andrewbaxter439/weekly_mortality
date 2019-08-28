ui <- fluidPage(
withMathJax(),  
 fluidRow(
   column(6,
 selectInput("group",
             "Group to compare: ",
             c("All", "Male", "Female"),
             selected = "All"),
 align = "center"),
column(6, dateInput("int1date",
           "Select Exposure start date: ",
           value = "2016-04-22"),
       align = "center")
 ),
 fluidRow(
 plotlyOutput("graph"),
 br(),
 p("Equation: $$Rate = \\beta_0+\\beta_1*Time+\\beta_2*Intervention_1+\\beta_3*Trend_1+\\beta_4*cos(Time)$$"),
 dataTableOutput("coefs"),
 plotOutput("autocorr")
 )
)