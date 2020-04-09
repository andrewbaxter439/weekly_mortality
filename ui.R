library(plotly)
library(shinyjs)

appCSS <- "
#loading-content {
  position: absolute;
  background: #5555AA;
  opacity: 0.7;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  padding: 100px;
  text-align: center;
  color: #FFFFFF;
}
"

ui <- fluidPage(
   withMathJax(),  
   useShinyjs(),
   inlineCSS(appCSS),
   fluidRow(
      div(id = "loading-content",
          h2("Checking for latest data and constructing graph"),
          h3("This will take a few seconds...")),
      column(4,
             selectInput("group",
                         "Group to compare: ",
                         c("All", "Male", "Female"),
                         selected = "All"),
             align = "center"),
      column(4, dateInput("int1date",
                          "Select Exposure start date: ",
                          value = "2019-01-01"),
             align = "center"),
      column(4, uiOutput("obsRange"), align = "center")
   ),
   fluidRow(
      # plotOutput("ggraph")),
      plotlyOutput("graph")),
   br(),
   fluidRow(column(12, align = "center", "Equation: $$Rate = \\beta_0+\\beta_1*Time+\\beta_2*Intervention_1+\\beta_3*Trend_1+\\beta_4*cos(Time)$$")),
   fluidRow(DT::dataTableOutput("coefs")),
   br(),
   column(4, checkboxInput("autocorr_show", "Autocorrelation analysis")),
   column(4, numericInput("p", "AR: ", 1, min = 0)),
   column(4, numericInput("q", "MA: ", 1, min = 0)),
   br(),
   h4(textOutput("AC1title")),
   fluidRow(plotOutput("autocorr_null")),
   br(),
   fluidRow(column(12, h4(textOutput("ac_corr")))),
   fluidRow(plotOutput("autocorr")),
   # htmlOutput("corrcompare"),
   # DT::dataTableOutput("data_table"),
   br(),
   h4(textOutput("anova1_title")),
   DT::dataTableOutput("anova1"),
   h4(textOutput("anova2_title")),
   DT::dataTableOutput("anova2"),
   h4(textOutput("anova3_title")),
   DT::dataTableOutput("anova3"),
   h4(textOutput("anova4_title")),
   DT::dataTableOutput("anova4"),
   p()
)
