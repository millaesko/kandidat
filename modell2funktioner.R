pg<-function(sprev,snow){
  n=dim(snow)[1]
  s1a<-sprev%>%
    mutate(Res1=1-Res)%>%
    group_by(Tm)%>%
    summarise_each(funs(sum),Res,Res1,G,GA,S,SA,PIM,OppPIM)
  s1b<-sprev%>%
    mutate(Res1=1-Res)%>%
    group_by(Opp)%>%
    summarise_each(funs(sum),Res1,Res,GA,G,SA,S,OppPIM,PIM)
  s1<-cbind(s1a[,1],s1a[,c(2:9)]+s1b[,c(2:9)])
  s1<-s1%>%
    mutate(GP=Res+Res1,SP=S/(S+SA),PimP=PIM-OppPIM)#SP=S/(S+SA),PimP=PIM/(PIM+OppPIM)
  s2<-snow%>%
    mutate(WinP.Tm=numeric(n),WinP.Opp=numeric(n),GF.Tm=numeric(n),GF.Opp=numeric(n),GA.Tm=numeric(n),GA.Opp=numeric(n),Pims.Tm=numeric(n),Pims.Opp=numeric(n),
           S.Tm=numeric(n),S.Opp=numeric(n))
  for(i in 1:30){
    x=s1$Tm[i]
    stest<-s2%>%
      filter(Tm==x|Opp==x)%>%
      mutate(Res1=1-Res,Wins=ifelse(Tm==x,Res,Res1),GFS=ifelse(Tm==x,G,GA),GAS=ifelse(Tm==x,GA,G),GF=cumsum(GFS)/seq_along(GFS),GA=cumsum(GAS)/seq_along(GAS),
             S.=ifelse(Tm==x,S,SA)/(ifelse(Tm==x,S,SA)+ifelse(Tm==x,SA,S)),Pim=ifelse(Tm==x,PIM,OppPIM)-ifelse(Tm==x,OppPIM,PIM),WinP=cumsum(Wins)/seq_along(Wins),
             SP=cumsum(S.)/seq_along(S.),Pims=cumsum(Pim)/seq_along(Pim)) #Pim=ifelse(Tm==x,PIM,OppPIM)/(ifelse(Tm==x,PIM,OppPIM)+ifelse(Tm==x,OppPIM,PIM))
    nn=dim(stest)[1]-1
    t<-seq(0,nn)
    #m=100*nn
    m=5 #dim(stest)[1]
    lambda<-ifelse(t<=m,1,0)*t/m+1*ifelse(t>m,1,0)
    # lambda<-t/m
    #lambda<-exp(-t)
    a=30
    lambda2<-1-exp(-t/a)  #*ifelse(t<=15,1,0)
    nn=1
    lambda3<-1-exp(-t*3)#ifelse(t<=nn,1,0)*t/nn+ifelse(t>nn,1,0)
    lambda4<-1-exp(-t*2)
    WinPLast=s1$Res[i]/s1$GP[i]
    GFGLast=s1$G[i]/s1$GP[i]
    GAGLast=s1$GA[i]/s1$GP[i]
    PimsLast=s1$PimP[i]
    ShotsLast=s1$SP[i]
    s<-stest%>%
      #mutate(Win=WinPLast*(1-lambda)+append(0,head(WinP,-1))*lambda, GFP=c(GFGLast,head(GF,-1)),GAP=c(GAGLast,head(GA,-1)),PimP=c(PimsLast,head(Pims,-1)),
      #ShotP=c(ShotsLast,head(SP,-1))) PimP=PimsLast*(1-lambda)+append(0,head(Pims,-1))*lambda
      mutate(Win=WinPLast*(1-lambda2)+append(0,head(WinP,-1))*lambda2, Win2=WinPLast*(1-lambda)+append(0,head(WinP,-1))*lambda,
             GFP=GFGLast*(1-lambda)+append(0,head(GF,-1))*lambda,
             GAP=GAGLast*(1-lambda4)+append(0,head(GA,-1))*lambda4,PimP=c(PimsLast,head(Pims,-1)),
             ShotP=ShotsLast*(1-lambda)+append(0,head(SP,-1))*lambda)#GFP=GFGLast*(1-lambda3)+append(0,head(GF,-1))*lambda3,
    # GAP=GAGLast*(1-lambda3)+append(0,head(GA,-1))*lambda3
    shome<-s%>%
      filter(Tm==x)
    sroad<-s%>%
      filter(Opp==x)
    s2$WinP.Tm[which(s2$Tm==x)]<-shome$Win
    s2$WinP.Opp[which(s2$Opp==x)]<-sroad$Win2
    s2$GF.Tm[which(s2$Tm==x)]<-shome$GFP
    s2$GF.Opp[which(s2$Opp==x)]<-sroad$GFP
    s2$GA.Tm[which(s2$Tm==x)]<-shome$GAP
    s2$GA.Opp[which(s2$Opp==x)]<-sroad$GAP
    s2$Pims.Tm[which(s2$Tm==x)]<-shome$PimP
    s2$Pims.Opp[which(s2$Opp==x)]<-sroad$PimP
    s2$S.Tm[which(s2$Tm==x)]<-shome$ShotP
    s2$S.Opp[which(s2$Opp==x)]<-sroad$ShotP
    #    s2$LG.Tm[which(s2$Tm==x)]<-c(0,head(stest$Wins,-1))[which(stest$Tm==x)]
    #  s2$LG.Opp[which(s2$Opp==x)]<c(0,head(stest$Wins,-1))[which(stest$Tm==x)]
  }
  s2<-s2%>%
    mutate(WPD=WinP.Tm-WinP.Opp,GFD=GF.Tm-GF.Opp,GAD=GA.Tm-GA.Opp,SD=S.Tm-S.Opp)
  return(s2)
}

hth<-function(s1,s2){
  nb=dim(s1)[1]+1
  s=rbind(s1,s2)
  nbe=dim(s)[1]
  m=length(s$Tm)
  s$Rk<-c(1:m)
  teams=unique(s$Tm)
  s$HthWP<-s$Lhth<-s$DiffHth<-c(rep(0,m))#numeric(m)
  for(i in 1:30){
    x=teams[i]
    t=teams[-i]
    h<-s%>%
      filter(Tm==x|Opp==x)%>%
      mutate(XGames=ifelse(Tm==x,Res,1-Res),Shot=ifelse(Tm==x,S,SA),GF=ifelse(Tm==x,G,GA),
             Ga=ifelse(Tm==x,GA,G),Sa=ifelse(Tm==x,SA,S),diff=GF-Ga)
    for(j in 1:length(t)){
      y=t[j]
      h1<-h%>%
        filter(Tm==y|Opp==y)
      home=which(h1$Tm==x)
      hem=which(s$Rk %in% h1$Rk[which(h1$Tm==x)])
      l=which(h1$Ovr!="SO")
      so=which(h1$Ovr=="")
      xg=h1$XGames[so]
      #s$HthWP1[hem]<-append(0,head(cumsum(xg)/seq_along(xg),-1))[home]
      s$HthWP[hem]<-append(0,head(cumsum(h1$XGames)/seq_along(h1$XGames),-1))[home]
      s$Lhth[hem]<-append(0,head(h1$XGames,-1))[home]
      s$DiffHth[hem]<-append(0,head(cumsum(h1$diff)/seq_along(h1$diff),-1))[home]#append(0,head(h1$diff,-1))[home]#
    }
  }
  return(s[c(nb:nbe),])
}

playoff<-function(snow,playoff){
  p1<-playoff%>%
    group_by(Tm)%>%
    tally()
  p2<-playoff%>%
    group_by(Opp)%>%
    tally()
  p<-cbind(p1[,1],p1[,2]+p2[,2])
  m=dim(snow)[1]
  s2<-snow%>%
    mutate(GPP.Tm=numeric(m),GPP.Opp=numeric(m))
  k=dim(p)[1]
  tm=unique(snow$Tm)
  tms=p$Tm
  tms <- factor(tms, levels=levels(tm))
  for(i in 1:16){
    x=tms[i]
    s2$GPP.Tm[which(s2$Tm==x)]<-s2$GPP.Opp[which(s2$Opp==x)]<-p$n[i]
  }
  return(s2)
}





last2<-function(sprev,po,snow){
  sh<-hth(sprev,snow)
  s<-pg(sprev,sh)
  s.out<-playoff(s,po)
  return(s.out)
}


