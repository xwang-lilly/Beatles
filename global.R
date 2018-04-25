rm(list=ls())
library(msm)
library(rjags)
library(coda)
load.module("glm")
set.seed(1234)
source("helpers.R") 

###Bayesian model in JAGS###
HazardPosterior_rjags <- function(event,V,ttt,int,parts,N, nsample,burnin, thin){
  if(event==TRUE) {
  model <- "
      # V[] is the vector of event indictor
  # ttt[] is the vector of times
  # int[] is the vector of ending time of each piece
  data {
  for (i in 1:N) {
  for (k in 1:parts) {
  # indicates event-time in interval k
  d[i,k] <- (V[i])*step(ttt[i] - int[k])*step(int[k+1] - ttt[i])
  }
  }
  }
  model  {
  # Make sure that the bin (piece) boundaries are defined in the data.
  for (i in 1:N) {
  for (k in 1:parts) {
  # length of overlap of ttt[i] with interval k
  delta[i,k] <- (min(ttt[i], int[k+1]) - int[k])*step(ttt[i] - int[k])
  # the proportional hazard and piecewise exponential hazard rate lambda[k]
  theta[i,k] <- e_lambda[k]
  # define the likelihood
  d[i,k] ~ dpois(mu[i,k])
  mu[i,k] <- delta[i,k]*theta[i,k]
  }
  }
  #priors
  for (k in 1:parts) {
  e_lambda[k] ~ dgamma(0.01, 0.01)    #allowing modify prior
  }
  }"
  }else{
    model <- "
      # V[] is the vector of event indictor
    # ttt[] is the vector of times
    # int[] is the vector of ending time of each piece
    data {
    for (i in 1:N) {
    for (k in 1:parts) {
    # indicates event-time in interval k
    d[i,k] <- (V[i])*step(ttt[i] - int[k])*step(int[k+1] - ttt[i])
    }
    }
    }
    model  {
    # Make sure that the bin (piece) boundaries are defined in the data.
    for (i in 1:N) {
    for (k in 1:parts) {
    # length of overlap of ttt[i] with interval k
    delta[i,k] <- (min(ttt[i], int[k+1]) - int[k])*step(ttt[i] - int[k])
    # the proportional hazard and piecewise exponential hazard rate lambda[k]
    theta[i,k] <- c_lambda[k]
    # define the likelihood
    d[i,k] ~ dpois(mu[i,k])
    mu[i,k] <- delta[i,k]*theta[i,k]
    }
    }
    #priors
    for (k in 1:parts) {
    c_lambda[k] ~ dgamma(0.01, 0.01)    #allowing modify prior
    }
    }" 
    }
  
  jagsData <- list("V"=V,"ttt"=ttt,"int"=int,"parts"=parts,"N"=N)
  if(event==TRUE) {
    jagsInits <- list("e_lambda"=rep(0,parts))
  }else{
    jagsInits <- list("c_lambda"=rep(0,parts))
  }
  m1 <- jags.model(file=textConnection(model),data=jagsData, inits=jagsInits,n.chains = 2, n.adapt=5000, quiet=FALSE)
  update(m1, n.iter=burnin)
  samples <- coda.samples(m1, variable.names=ifelse(event==TRUE,"e_lambda","c_lambda"), n.iter=nsample*thin, thin=thin)
  samples
}

PredTime <- function(dataM, PS_lambda_event, PS_lambda_censored, nevent, npar_e, npar_c, ner, der, nsample, burnin, thin){
  tq <- seq(0,1,length = (npar_e+1))
  tinterval_e <- c(0,quantile(dataM[dataM[,2]==1,1],tq)[-1])
  tinterval_e[(npar_e+1)] <-  10*tinterval_e[(npar_e+1)]
  
  tq <- seq(0,1,length = (npar_c+1))
  tinterval_c <- c(0,quantile(dataM[dataM[,3]==1,1],tq)[-1])
  tinterval_c[(npar_c+1)] <-  50*tinterval_c[(npar_c+1)] # Make upper bound large
  
  # event and censored vector
  
  dataRisk <- dataM[(dataM[,2]==0) & (dataM[,3]==0),1];
  nstar <- length(dataRisk);
  
  #at risk population and unknown population;
  risknewt1=risknewt1_e=risknewt1_c=riskevent1=rep(0,nstar)
  n2er=sum(ner*der)
  n3er=rep(ner,der)
  nper=length(n3er)
  tenroll=risknewt2=risknewt2_e=risknewt2_c=riskevent2=rep(0,n2er)
  predtime=rep(0,nsample);
  
  for (i in 1:nsample){
    rate_e=PS_lambda_event[i,]
    rate_c=PS_lambda_censored[i,]
    
    #at risk population
    for (j in 1:nstar){
      repeat{
        risknewt1_e[j]=rpexp(1,rate=rate_e,as.numeric(tinterval_e[1:npar_e]))
        if(risknewt1_e[j]> dataRisk[j]){break}
      }
      repeat{
        risknewt1_c[j]=rpexp(1,rate_c,as.numeric(tinterval_c[1:npar_c]))
        if(risknewt1_c[j]> dataRisk[j]){break}
      }
      if (risknewt1_e[j]<risknewt1_c[j]) {
        risknewt1[j]=risknewt1_e[j]; riskevent1[j]=1
      }else {risknewt1[j]=risknewt1_c[j]; riskevent1[j]=0}
    }
    newtime1=risknewt1-dataRisk;
    
    #unknown population
    # if nep is piece of enrollment for unknown population, the enrollment rate is ner;
    #  and the duration of each enrollment piece is der; 
    # eg, ner=c(40,30,20) and der=c(5,3,1) means that expect to enroll 40 patients/month in the next 5 months ;
    # then 30 pts/mth in the following 3 months; and 20 pts in the last month;
    
    #patients to be enrolled;
    
    if (n2er>0){
      for (k in 1:nper){
        for (h in 1:n3er[k]){
          if (length(n3er)>1){
            erindex=sum(n3er[1:(k-1)])+h 
            tenroll[erindex]=30.25*(k-1+h/n3er[k])
          }
          else {
            erindex=h 
            tenroll[erindex]=30.25*(k-1+h/n3er[k])
          }
        }
      }
      
      for (j in 1:n2er){            
        risknewt2_e[j]=rpexp(1,rate=rate_e,as.numeric(tinterval_e[1:npar_e]))
        risknewt2_c[j]=rpexp(1,rate_c,as.numeric(tinterval_c[1:npar_c]))
        if (risknewt2_e[j]<risknewt2_c[j]) {risknewt2[j]=risknewt2_e[j]; riskevent2[j]=1}
        else {risknewt2[j]=risknewt2_c[j]; riskevent2[j]=0}
      }
      
      newtime2=tenroll+risknewt2;
      newtime=c(newtime1, newtime2);
      riskevent=c(riskevent1, riskevent2)
    }else {
      newtime=newtime1; riskevent=riskevent1;
    }
    
    temp=cbind(newtime,riskevent);
    temp2=temp[order(temp[,1]),];
    temp3=cbind(temp2,cumsum(temp2[,2]));
    if (max(temp3[,3]) < nevent){
      predtime[i] <- Inf;
    } else {
      predtime[i]<- min(temp3[temp3[,3]==nevent,1]);}
  }
  predtime
}



PredEvents <- function(dataM, PS_lambda_event, PS_lambda_censored, interval, npar_e, npar_c, ner, der, nsample, burnin, thin){
  
  tq <- seq(0,1,length = (npar_e+1))
  tinterval_e <- c(0,quantile(dataM[dataM[,2]==1,1],tq)[-1])
  tinterval_e[(npar_e+1)] <-  10*tinterval_e[(npar_e+1)]
  
  tq <- seq(0,1,length = (npar_c+1))
  tinterval_c <- c(0,quantile(dataM[dataM[,3]==1,1],tq)[-1])
  tinterval_c[(npar_c+1)] <-  50*tinterval_c[(npar_c+1)] # Make upper bound large
  
  totaleventsV <- rep(0,nrow=nsample);
  currentevents <- sum(dataM[,2]);
  nmcint <- 100;
  dataRisk <- dataM[(dataM[,2]==0) & (dataM[,3]==0),1];
  nstar <- length(dataRisk);
  
  STx<- function(Tx , tinterval, Gammatv){
    Indx <- rep(0,2);
    Indx[1] <- sum(tinterval < Tx)-1;
    Indx[2] <- (Tx-tinterval[Indx[1]+1])/(tinterval[Indx[1]+2]-tinterval[Indx[1]+1]);
    Value  <- exp(-sum(Gammatv[1:Indx[1]])-Indx[2]*Gammatv[Indx[1]+1]);
    Value;
  }
  lambdaX <- function(X,lambda,tinterval){
    v <-  lambda[sum(tinterval < X)];
  }
  
  for(nsample_iter in 1:nsample){
    preEvents_exp <- 0;
    preEvents_pre <- 0;
    preEvents_pre2 <- 0;
    lambda1 <- PS_lambda_event[nsample_iter,];
    lambda0 <- PS_lambda_censored[nsample_iter,];
    
    Gammatv1 <- lambda1*diff(tinterval_e);
    Gammatv0 <- lambda0*diff(tinterval_c);
    
    # at risk population;
    for(nstar_iter in 1:nstar){
      t0 <- dataRisk[nstar_iter];
      X <- runif(nmcint,min= t0, max = t0+interval);
      numerator <- interval*mean(apply(array(X),1, lambdaX,lambda1,tinterval_e)*apply(array(X),1,STx,tinterval_e,Gammatv1)*apply(array(X),1,STx,tinterval_c,Gammatv0));
      denominator <- STx(t0,tinterval_e,Gammatv1)*STx(t0,tinterval_c,Gammatv0);
      preEvents_pre <- preEvents_pre + rbinom(1, 1, numerator/denominator);
    }
    
    #unknown population;
    n2er=sum(ner*der)
    n3er=rep(ner,der)
    nper=length(n3er)
    tenroll=risknewt2=risknewt2_e=risknewt2_c=riskevent2=rep(0,n2er)
    
    if (n2er>0){
      for (k in 1:nper){
        for (h in 1:n3er[k]){
          if (length(n3er)>1){
            erindex=sum(n3er[1:(k-1)])+h 
            tenroll[erindex]=30.25*(k-1+h/n3er[k])
          } else {
            erindex=h 
            tenroll[erindex]=30.25*(k-1+h/n3er[k])
          }
        }
      }
      
      for(nstar_iter in 1:n2er){
        terl <- tenroll[nstar_iter];
        if (terl<interval){
          X <- runif(nmcint,min= 0, max = interval-terl);
          numerator2 <- interval*mean(apply(array(X),1, lambdaX,lambda1,tinterval_e)*apply(array(X),1,STx,tinterval_e,Gammatv1)*apply(array(X),1,STx,tinterval_c,Gammatv0));
          preEvents_pre2 <- preEvents_pre2 + rbinom(1, 1, numerator2);
        }else {preEvents_pre2 <- preEvents_pre2}
      }
      totaleventsV[nsample_iter] <- currentevents + preEvents_pre + preEvents_pre2;
    }else {
      totaleventsV[nsample_iter] <- currentevents + preEvents_pre;
    }
  }
  totaleventsV;
}
