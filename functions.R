LogLoss <- function(y, fitted.results1){
  N=length(y)
  ll=(-1/N)*sum(y*log(fitted.results1)+(1-y)*log(1-fitted.results1))
  return(ll)
}
AUC<-function(y,p){
  pr <- prediction(p, y)
  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  return(auc)
}

ROC<-function(y,p){
  pr <- prediction(p, y)
  prf2 <- performance(pr, measure = "tpr", x.measure = "fpr")
  plot(prf2,col="blue")
  abline(a=0,b=1)
}

Classification<-function(y,fitted.results1){
  fitted.results <- ifelse(fitted.results1 > 0.5,1,0)
  misClasificError <- mean(fitted.results != y)
  return(misClasificError)
}

Analys<-function(model,set){
  y=set$Res
  p <- predict(model,set,type="response")
  print(paste('Accuracy',round(1-Classification(y,p),digits=4)))
  print(paste('Logloss',round(LogLoss(y,p),digits=4)))
  print(paste('AUC',round(AUC(y,p),digits=4)))
  op=par(mfrow=c(1,2))
  hist(p,col="lightgreen",main=NULL,xlab = "Anpassade sannolikheter",ylab="Frekvens")
  qqnorm(p,col="blue")
  qqline(p)
  plim=seq(0.4,0.6,0.005)
  accur<-rep(0,length(plim))
  for(i in 1:length(plim)){
    preds=ifelse(p >plim[i],1,0)
    accur[i]= 1-mean(preds != y)
  }
  op=par(mfrow=c(1,2))
  plot(plim,accur,xlab="Threshold",ylab="Accuracy",type="p",pch=19,col="magenta")
  abline(a=accur[which(plim==0.5)],b=0, type="l", lty=2)
  ROC(y,p)
  return(p)
}

Lagvis<-function(set,teams,p){
  y=set$Res
  n=30
  f=ifelse(p > 0.5,1,0)
  tot=numeric(n)
  home=numeric(n)
  for ( i in 1:n){
    x=teams[i]
    a=which(set$Tm==x)
    b=which(set$Opp==x)
    c=c(a,b)
    tot[i]=1-mean(f[c] !=y[c])
    home[i]=1-mean(f[a]!=y[a])
  }
  return(rbind(tot,home))#rbind(home,road,tot)
}

TeamWinP<-function(set,teams){
  wp<-wph<-numeric(30)
  for(i in 1:30){
    x=teams[i]
    s<-set%>%
      filter(Tm==x|Opp==x)%>%
      mutate(Res2=ifelse(Tm==x,Res,1-Res))
    wp[i]=mean(s$Res2)
    wph[i]=mean(set$Res[which(set$Tm==x)])
  }
  return(rbind(wp,wph))
}

Confusion<-function(set,p){
  y=set$Res
  f <- ifelse(p > 0.5,1,0)
  TP=sum(y==1 & f==1)
  TN=sum(y==0 & f==0)
  FP=sum(y==0 & f==1)
  FN=sum(y==1 & f==0)
  print(paste('FalsePositiveRate',FP/(TN+FP)))
  print(paste('FalseNegativeRate',FN/(TP+FN)))
  print(paste('PositivePred',mean(f)))
  print(paste('WinProc', mean(y) ))
}

d<-function(valid1,teams){
  val1=c(which(valid1==min(valid1)),which(valid1==max(valid1)))
  M<-data.frame(Team=teams,Value=round(valid1,digits=4))
  m<-M[val1,]
  return(m)
}