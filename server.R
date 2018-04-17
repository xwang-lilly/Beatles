
############################################ Server part#################################
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
      tsub <- paste("(",min(dataM[,1]),", ",max(dataM[,1]),")")
      tevents <- paste0("(",min(dataM[dataM[,2]==1,1]),", ",max(dataM[dataM[,2]==1,1]),")")
      tcensor <- paste0("(",min(dataM[dataM[,3]==1,1]),", ",max(dataM[dataM[,3]==1,1]),")")
      tatrisk <- paste0("(",min(dataM[dataM[,2]==0&dataM[,3]==0,1]),", ",max(dataM[dataM[,2]==0&dataM[,3]==0,1]),")")
      datasummary <- data.frame(c("Total Subjects","Events","Censored","At Risk"),c(nsub,nevents,ncensor,natrisk),c(tsub,tevents,tcensor,tatrisk))
      names(datasummary) <- c("","Frequency","Time Range")
      return(datasummary)
    }
  },colnames = TRUE,rownames = FALSE,digits = 0)
  
  
  retsamples<-eventReactive(input$submitButton, {
    req(input$file)
    infile <- input$file
    dataM <- read.csv(infile$datapath)
    
    npar_e <- input$npar_e
    npar_c <- input$npar_c
    nsample <- input$nsample
    burnin <- input$burnin
    thin <- input$thin
    time.start<-Sys.time()
    
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
    PS_lambda_event <- HazardPosterior_rjags(V=event,ttt,int=tinterval_e,parts=npar_e,N, nsample, burnin, thin)
    
    tq <- seq(0,1,length = (npar_c+1))
    tinterval_c <- c(0,quantile(dataM[dataM[,3]==1,1],tq)[-1])
    tinterval_c[(npar_c+1)] <-  50*tinterval_c[(npar_c+1)] # Make upper bound large
    censored <- dataM[,3]
    PS_lambda_censored <- HazardPosterior_rjags(V=censored,ttt,int=tinterval_c,parts=npar_c,N, nsample, burnin, thin)
    
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
      posterioroutput <- cbind(predicted_events,PS_lambda_event[[1]],PS_lambda_censored[[1]])
      hist(predicted_events,main = "Density Plot of Predicted Events",freq = FALSE)
      lines(density(predicted_events),col=2)
    }
    dev.off()
    #time<-proc.time()["elapsed"]-time.start
    time.end <- Sys.time()
    return(list(predictionsummary=predictionsummary,posterioroutput=posterioroutput,outplot=outplot,predictionplot=predictionplot,time1=time.start,time2=time.end))
    
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
  
  # output$diagplot <- renderUI({
  #   #input$submitButton
  #   #style="height:600px; width:100%",
  #   tags$iframe(src=retsamples()$outplot)
  # })
  
  output$downloadDiagPlot <- downloadHandler(
    filename = c('DiagnosticPlot.pdf'),
    content = function(file) {
      file.copy(retsamples()$outplot,file,overwrite = TRUE)
    }
  )
  
  
  
  # output$diagplot <- renderImage({
  #   input$submitButton
  #   isolate({
  #     list(src = retsamples()$diagplot,
  #          contentType = 'image/png',
  #          width = "auto",
  #          height = "auto",
  #          alt = "This is alternate text")
  #   })
  # })
  
  
  
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
    }
  )
  
  output$time1<-renderText({
    input$submitButton
    paste("Algorithm started at", retsamples()$time1)
    #isolate({
    #   cat("This run took ",round(retsamples()$time/60,1)," minutes.")
    #   })
  })
   output$time2<-renderText({
     input$submitButton
     paste("Algorithm ended at", retsamples()$time2)
     #isolate({
     #   cat("This run took ",round(retsamples()$time/60,1)," minutes.")
     #   })
   })
  
  output$message<-renderPrint({
    input$submitButton
    isolate({
      cat("This run took",round(retsamples()$time2-retsamples()$time1,0),"seconds.")
    })
  })
  
}
  
 