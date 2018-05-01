
library(shiny)
library(shinyjs)
library(shinycssloaders)

ui<-fluidPage(
  shinyjs::useShinyjs(),

  tags$head(
    tags$style(HTML("
      @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
      
      h1 {
        font-family: 'Lobster', cursive;
        font-weight: 500;
        line-height: 1.1;
        color: #48ca3b;
      }

    "))
  ),
  headerPanel("Beatles"), 
  em("Bayesian Events and Time Landmark Estimation Software"),
  h3(""),
  h3(""),
  fluidRow(
    column(6,
           h4("Data and Model Settings"),
           wellPanel(
             strong("Example dataset can be downloaded here: "),
             downloadButton('downloadExampledata','Example dataset'),
             h4(""),
             h4(""),
             fileInput("file", label = "Choose your own dataset from PC",multiple = FALSE,accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
             fluidRow(
               column(6,
                      numericInput("npar_e", label ="Event Hazard Pieces", value = 5, min = 1, step = 1),
                      numericInput("npar_c", label ="Censored Hazard Pieces", value = 2, min = 1, step = 1),
                      numericInput("nsample", label ="Posterior Sample Size", value = 2000, min = 1, step = 100),
                      numericInput("burnin", label ="Burnin", value = 5000, min = 1, step = 100),
                      numericInput("thin", label ="Thin", value = 5, min = 1, step = 1)
               ),
               column(6,
                      radioButtons("predictiontype",label = "Prediction",choices = list("Time Prediction","Event Prediction"),selected = "Time Prediction",inline = TRUE),
                      conditionalPanel(
                        condition = "input.predictiontype == 'Time Prediction'",
                        numericInput("addevent", label ="Additional Events", value = 10, min = 1, step = 1)
                      ),
                      conditionalPanel(
                        condition = "input.predictiontype == 'Event Prediction'",
                        numericInput("addtime", label ="Additional Time", value = 10, min = 0.1, step = 0.1)
                      ),
                      radioButtons("enrollstatus",label = "Enrollment",choices = list("Completed","Not Completed"),selected = "Completed",inline = TRUE),
                      conditionalPanel(
                        condition = "input.enrollstatus == 'Not Completed'",
                        textInput("ner", label ="Future Enrollment Rate (pts/month)", value = "20,10,5"),
                        textInput("der", label ="Future Enrollment Duration (month)", value = "3,2,1"),
                        h5("* Note: all vector inputs should be comma delimited")
                      )
               )
             )
           )
    ),
    
    column(6,
           h4("Results"),
           wellPanel(
             fluidRow(
               column(4, 
                      withBusyIndicatorUI(
                        actionButton("submitButton", "Submit", icon=icon("area-chart"),class = "btn-primary"))),
               column(6,
                      h5("Algorithm Started at :"),
                      textOutput("time1"),
                      h5("Algorithm Ended at :"),
                      textOutput("time2"))),
             hr(),
             tabsetPanel(
               tabPanel("Data Summary",
                        hr(),
                        tableOutput("datasummary")
               ),
               tabPanel("Prediction Summary",
                        hr(),
                        tableOutput("prediction"),
                        withSpinner(plotOutput("predictionplot"),type=5,size=0.7)
               ),
               tabPanel("Diagnostic/Download",
                        hr(),
                        withSpinner(downloadButton('downloadDiagPlot','Download diagnostic plot as PDF file'),type=8,color=3,size=0.9), 
                        downloadButton('downloadPS','Download posterior results as CSV file'),
                        htmlOutput("diagplot")
               )
             )
           )
          )
         )
        )

