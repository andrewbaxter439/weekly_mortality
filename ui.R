ui <- fluidPage(
  
 fluidRow(
   column(6,
 selectInput("group",
             "Group to compare: ",
             c("All", "Male", "Female"),
             selected = "All"),
 align = "center"),
column(6, dateInput("int1date",
           "Select Exposure start date: ",
           value = "2012-01-01"),
       align = "center")
 ),
 fluidRow(
 plotOutput("graph"),
 br(),
 dataTableOutput("coefs"),
 plotOutput("autocorr")
 )
)