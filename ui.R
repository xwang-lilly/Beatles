
library(shiny)
library(shinyjs)

######### UI part #######  
ui<-fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Beatles"),
  h5("Bayesian Events and Time Landmark Estimation Software"),
  
  tags$head(
    tags$style(HTML("
                    .shiny-output-error-validation{
                    color:red;}
                    ")
    )
    ),
  
  fluidRow(
    
    column(6,
           h4("Data and Model Settings"),
           wellPanel(
             strong("Example dataset can be downloaded here: "),
             downloadButton('downloadExampledata','Example dataset'),
            fileInput("file", label = "Choose your own dataset from PC",multiple = FALSE,accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
            #hr(),
             fluidRow(
               column(6,
                      numericInput("npar_e", label ="Event hazard pieces", value = 5, min = 1, step = 1),
                      numericInput("npar_c", label ="Censored hazard pieces", value = 2, min = 1, step = 1),
                      numericInput("nsample", label ="Posterior sample size", value = 2000, min = 1, step = 1),
                      numericInput("burnin", label ="Burnin", value = 5000, min = 1, step = 1),
                      numericInput("thin", label ="Thin", value = 5, min = 1, step = 1)

               ),
               column(6,
                      radioButtons("predictiontype",label = "Prediction",choices = list("Time Prediction","Event Prediction"),selected = "Time Prediction",inline = TRUE),
                      conditionalPanel(
                        condition = "input.predictiontype == 'Time Prediction'",
                        numericInput("addevent", label ="Additional events", value = 10, min=1, step=1)
                      ),
                      conditionalPanel(
                        condition = "input.predictiontype == 'Event Prediction'",
                        numericInput("addtime", label ="Additional time", value = 10, min = 0.1, step = 0.1)
                      ),
                      radioButtons("enrollstatus",label = "Enrollment",choices = list("Completed","Not Completed"),selected = "Completed",inline=TRUE),
                      conditionalPanel(
                        condition = "input.enrollstatus == 'Not Completed'",
                         textInput("ner", label ="Future Enrollment Rate (pts/month)", value = "20,10,5"),
                         textInput("der", label ="Future Enrollment Duration (month)", value = "3,2,1")
                                                  #h5("* Note: all vector inputs should be comma delimited")
                      )
                      # conditionalPanel(
                      #   condition = "input.enrollstatus == 'Completed'",
                      #   textInput("ner", label ="Future Enrollment Rate (pts/month)", value = "0"),
                      #   textInput("der", label ="Future Enrollment Duration (month)", value = "0")
                      #                             #h5("* Note: all vector inputs should be comma delimited")
                      # )
                      
                
               )
             
             )
           )
    ),

    column(6,
           h4("Results"),
           wellPanel(
             fluidRow(
               column(4,
                      actionButton("submitButton", "Submit")
               ),
               column(8,
                      textOutput("time1"),
                      textOutput("time2"),
                      textOutput("message")
               )
             ),
             hr(),
             tabsetPanel(
               tabPanel("Data Summary",
                        tableOutput("datasummary")
               ),
               tabPanel("Prediction Summary",
                        tableOutput("prediction"),
                        plotOutput("predictionplot")
               ),
               tabPanel("Diagnostic/Download",
                        downloadButton('downloadDiagPlot','Download diagnostic plot as PDF file'), 
                        downloadButton('downloadPS','Download posterior results as CSV file'),
                        htmlOutput("diagplot")
                        
                        
               )
             )
             
           )
    )
  )
)


