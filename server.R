
library(shiny)
library(shinyjs)


server<-function(input,output,session){
  exampledata <- read.csv("/lrlhps/users/c253994/ShinyApps/Beatles/BeatlesTestData.csv")
  output$datasummary<-renderTable({
    if(is.null(input$file)){
      datasummary <- data.frame(c("Total Subjects","Events","Censored","At Risk"),c("","","",""),c("","","",""))
      names(datasummary) <- c("","Frequency","Time Range")
      return(datasummary)
    }else{
      infile <- input$file
      dataM <- read.csv(infile$datapath)
      nsub <- nrow(dataM)
      nevents <- sum(dataM[,2]==1)
      ncensor <- sum(dataM[,3]==1)
      natrisk <- nsub-nevents-ncensor
      tsub <- paste("(",min(dataM[,1]),", ",max(dataM[,1]),")",sep="")
      tevents <- paste0("(",min(dataM[dataM[,2]==1,1]),", ",max(dataM[dataM[,2]==1,1]),")")
      tcensor <- paste0("(",min(dataM[dataM[,3]==1,1]),", ",max(dataM[dataM[,3]==1,1]),")")
      tatrisk <- paste0("(",min(dataM[dataM[,2]==0&dataM[,3]==0,1]),", ",max(dataM[dataM[,2]==0&dataM[,3]==0,1]),")")
      datasummary <- data.frame(c("Total Subjects","Events","Censored","At Risk"),c(nsub,nevents,ncensor,natrisk),c(tsub,tevents,tcensor,tatrisk))
      names(datasummary) <- c("","Frequency","Time Range")
      return(datasummary)
    }
  },colnames = TRUE,rownames = FALSE,digits = 0)
  
  starttime<-eventReactive(input$submitButton, {
    Sys.time()
      })
  
  observeEvent(input$submitButton, {
    withBusyIndicatorServer("submitButton", {
      if (is.numeric(input$thin)) {
        stop("Please Waiting...")
      }
      })
      })
  
  output$time1<-renderText({
    paste(starttime())
      })
  
  retsamples<-eventReactive(input$submitButton, {
    req(input$file)
    infile <- input$file
    dataM <- read.csv(infile$datapath)
    npar_e <- input$npar_e
    npar_c <- input$npar_c
    nsample <- input$nsample
    burnin <- input$burnin
    thin <- input$thin
    
    if(input$enrollstatus=="Completed"){
      ner <- 0
      der <- 0
    }else{
      ner <- as.numeric(unlist(strsplit(input$ner,",")))
      der <- as.numeric(unlist(strsplit(input$der,",")))
    }
    validate(need(length(ner)==length(der),"length of enrollment rate must be equal to length of enrollment duration."))
    
    tq <- seq(0,1,length = (npar_e+1))
    tinterval_e <- c(0,quantile(dataM[dataM[,2]==1,1],tq)[-1])
    tinterval_e[(npar_e+1)] <-  10*tinterval_e[(npar_e+1)]
    ttt <- dataM[,1]
    N <- length(ttt)
    event <- dataM[,2]
    PS_lambda_event <- HazardPosterior_rjags(event=TRUE,V=event,ttt,int=tinterval_e,parts=npar_e,N, nsample, burnin, thin)
    
    tq <- seq(0,1,length = (npar_c+1))
    tinterval_c <- c(0,quantile(dataM[dataM[,3]==1,1],tq)[-1])
    tinterval_c[(npar_c+1)] <-  50*tinterval_c[(npar_c+1)]         
    censored <- dataM[,3]
    PS_lambda_censored <- HazardPosterior_rjags(event=FALSE,V=censored,ttt,int=tinterval_c,parts=npar_c,N, nsample, burnin, thin)
    
    width<-500
    height<-500
    
    outplot <- tempfile(fileext='.pdf')
    pdf(outplot)
    plot(PS_lambda_event)
    plot(PS_lambda_censored)
    dev.off()
    
    predictionplot <- tempfile(fileext='.png')
    png(predictionplot,width = width,height = height)
    
    if(input$predictiontype=="Time Prediction"){
      addevent <- input$addevent
      predicted_time<- PredTime(dataM, PS_lambda_event[[1]], PS_lambda_censored[[1]], addevent, npar_e, npar_c,ner, der, nsample, burnin, thin);
      predictionsummary <- data.frame(matrix(quantile(predicted_time,c(0,0.1,0.25,0.5,0.75,0.9,1)),nrow = 1))
      colnames(predictionsummary) <- c("MIN","10%","25%","50%","75%","90%","MAX")
      rownames(predictionsummary) <- "Predicted Time"
      posterioroutput <- cbind(predicted_time,PS_lambda_event[[1]],PS_lambda_censored[[1]])
      hist(predicted_time,main = "Density Plot of Predicted Time",freq = FALSE)
      lines(density(predicted_time),col=2)

    }else if(input$predictiontype=="Event Prediction"){
      addtime <- input$addtime
      predicted_events <- PredEvents(dataM, PS_lambda_event[[1]], PS_lambda_censored[[1]], addtime, npar_e, npar_c,ner, der, nsample, burnin, thin);
      predictionsummary <- data.frame(matrix(round(quantile(predicted_events,c(0,0.1,0.25,0.5,0.75,0.9,1)),digits = 0),nrow = 1))
      colnames(predictionsummary) <- c("MIN","10%","25%","50%","75%","90%","MAX")
      rownames(predictionsummary) <- "Predicted Events"
      colnames(PS_lambda_event[[1]]) <-gsub("lambda","e_lambda",colnames(PS_lambda_event[[1]]))        # change column names before combination
      colnames(PS_lambda_censored[[1]]) <-gsub("lambda","e_lambda",colnames(PS_lambda_censored[[1]]))
      posterioroutput <- cbind(predicted_events,PS_lambda_event[[1]],PS_lambda_censored[[1]])
      hist(predicted_events,main = "Density Plot of Predicted Events",freq = FALSE)
      lines(density(predicted_events),col=2)
    }
    dev.off()
    time.end <- Sys.time()
    return(list(predictionsummary=predictionsummary,posterioroutput=posterioroutput,outplot=outplot,predictionplot=predictionplot,time2=time.end))
    
  })
  
  output$time2<-renderText({
    paste(retsamples()$time2)
  })
  
  output$prediction<-renderTable({
      input$submitButton
      isolate({
        retsamples()$predictionsummary })
    },colnames = TRUE,rownames = TRUE,digits = 0)
  
  output$predictionplot<-renderImage({
    input$submitButton
    isolate({
      list(src = retsamples()$predictionplot,
           contentType = 'image/png',
           alt = "This is alternate text")
    })
  },deleteFile=TRUE)
  
  output$downloadDiagPlot <- downloadHandler(
    filename = c('DiagnosticPlot.pdf'),
    content = function(file) {
      file.copy(retsamples()$outplot,file,overwrite = TRUE)
    }
  )
  
  output$downloadPS <- downloadHandler(
    filename = c('PosteriorResults.csv'),
    content = function(file) {
      write.csv(retsamples()$posterioroutput, file, row.names = FALSE)
    }
  )
  
  output$downloadExampledata <- downloadHandler(
    filename = c('BeatlesTestData.csv'),
    content = function(file) {
      write.csv(exampledata, file, row.names = FALSE)
    })
    
   output$time2<-renderText({
     paste(retsamples()$time2)
   })
   
  observeEvent(input$submitButton, {
     withBusyIndicatorServer("submitButton", {
       Sys.sleep(0.1)
         stop("Done")
       })
    })
  }


  
 