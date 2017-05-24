thisseason<-function(s,teams){
  n=dim(s)[1]
  s$GF.Tm<-s$GF.Opp<-s$GA.Tm<-s$GA.Opp<-s$S.Tm<-s$S.Opp<-s$GFD<-s$DiffLastGame.Tm<-s$DiffLastGame.Opp<-s$GAD<-s$SD<-s$WP.Tm<-s$WP.Opp<-
    s$LastGame.Tm<- s$LastGame.Opp<- s$OvertimeP.Tm<-s$OvertimeP.Opp<-s$Pims.Opp<-s$Pims.Tm<-s$Pims1<-s$Last3Games.Tm<-s$Last3Games.Opp<-s$LastGame2.Tm<-s$LastGame2.Opp<-s$GALastGame.Tm<-s$GALastGame.Opp<-s$GFLastGame.Tm<-s$GFLastGame.Opp<-numeric(n)
  #s$Last3Games.Tm<-s$Last3Games.Opp<-numeric(n)
  for (i in 1:length(teams)){
    x=teams[i]
    home=which(s$Tm==x)
    opp=which(s$Opp==x)
    sub<-s%>%
      filter(Tm==x|Opp==x)%>%
      mutate(Res1=1-Res,ResT=ifelse(Tm==x,Res,Res1),WP=c(0,head(cumsum(ResT)/seq_along(ResT),-1)),lg1=c(0,head(ResT,-1)),lg2=c(0,0,head(ResT,-2)),lg3=c(0,0,0,head(ResT,-3)),
             wins=lg1+lg2+lg3,
             Ovt=ifelse(Ovr=="",0,1),OvPr=append(0,head(cumsum(Ovt)/seq_along(Ovt),-1)),
             GF=ifelse(Tm==x,G,GA),GA=ifelse(Tm==x,GA,G),Diff=append(0,head(GF-GA,-1)), 
             gf=append(0,head(cumsum(GF)/seq_along(GF),-1)),ga=append(0,head(cumsum(GA)/seq_along(GA),-1)),
             SD=ifelse(Tm==x,S,SA)-ifelse(Tm==x,SA,S),S=append(0,head(cumsum(SD)/seq_along(SD),-1)),
             PIMS=ifelse(Tm==x,PIM,OppPIM)-ifelse(Tm==x,OppPIM,PIM),
             PimP=append(0,head(cumsum(PIMS)/seq_along(PIMS),-1)))
    r=which(sub$Opp==x)
    h=which(sub$Tm==x)
    s$GA.Tm[home]<-sub$ga[h]
    s$GA.Opp[opp]<-sub$ga[r]
    s$GF.Tm[home]<-sub$gf[h]
    s$GF.Opp[opp]<-sub$gf[r]
    s$S.Tm[home]<-sub$S[h]
    s$S.Opp[opp]<-sub$S[r]
    s$GALastGame.Tm[home]<-c(0,head(sub$GA,-1))[h]#sub$Diff[h]
    s$GALastGame.Opp[opp]<-c(0,head(sub$GA,-1))[r]#sub$Diff[r]
    s$GFLastGame.Tm[home]<-c(0,head(sub$GF,-1))[h]#sub$Diff[h]
    s$GFLastGame.Opp[opp]<-c(0,head(sub$GF,-1))[r]#sub$Diff[r]
    s$WP.Tm[home]<-sub$WP[h]
    s$WP.Opp[opp]<-sub$WP[r]
    s$LastGame.Tm[home]<-sub$lg1[h]
    s$LastGame.Opp[opp]<-sub$lg1[r]
    s$LastGame2.Tm[home]<-sub$lg2[h]
    s$LastGame2.Opp[opp]<-sub$lg2[r]
    s$Last3Games.Tm[home]<-sub$wins[h]
    s$Last3Games.Opp[opp]<-sub$wins[r]
    s$OvertimeP.Tm[home]<-sub$OvPr[h]
    s$OvertimeP.Opp[opp]<-sub$OvPr[r]
    s$Pims.Tm[home]<-sub$PimP[h]
    s$Pims.Opp[opp]<-sub$PimP[r]
  }
  s$GFD<-s$GF.Tm - s$GF.Opp
  s$GAD<-s$GA.Tm - s$GA.Opp
  s$SD<-s$S.Tm-s$S.Opp
  s$PimsD<-s$Pims.Tm-s$Pims.Opp
  return(s)
}

tabell<-function(s1,p1,s2){
  sp<-s1%>%
    mutate(Res1=1-Res)%>%
    group_by(Tm)%>%
    summarise_each(funs(sum),Res,Res1,G,GA)
  st<-s1%>%
    group_by(Opp)%>%
    mutate(Res1=1-Res)%>%
    summarise_each(funs(sum),Res1,Res,GA,G)
  s<-cbind(sp[,1],sp[,c(2:5)]+st[,c(2:5)])
  p<-p1%>%
    mutate(Res1=1-Res)%>%
    group_by(Tm)%>%
    summarise_each(funs(sum),Res,Res1)
  p2<-p1%>%
    mutate(Res1=1-Res)%>%
    group_by(Opp)%>%
    summarise_each(funs(sum),Res1,Res)
  p$Wins<-p$Res+p2$Res1
  p$Losses<-p$Res1+p2$Res
  p$Games<-p$Wins+p$Losses
  n=dim(s2)[1]
  s2$WP.Tm.ls<-s2$WP.Opp.ls<-s2$HWP<-s2$RWP<-s2$GF.Tm.ls<-s2$GF.Opp.ls<-s2$GA.Tm.ls<-s2$GA.Opp.ls<-s2$PlayoffGP.Tm<-s2$PlayoffGP.Opp<-s2$PlayoffWins.Tm<-s2$PlayoffWins.Opp<-numeric(n)
  for (i in 1:length(s$Tm)){
    x=s$Tm[i]
    gp=s$Res[i]+s$Res1[i]
    h=which(s2$Tm==x)
    r=which(s2$Opp==x)
    p$Tm <- factor(p$Tm, levels=levels(s$Tm))
    s2$HWP[h]<-sp$Res[i]/gp
    s2$RWP[r]<-st$Res1[i]/gp
    s2$WP.Tm.ls[h]<-s2$WP.Opp.ls[r]<-s$Res[i]/gp
    s2$GF.Tm.ls[h]<-s2$GF.Opp.ls[r]<-s$G[i]/gp
    s2$GA.Tm.ls[h]<-s2$GA.Opp.ls[r]<-s$GA[i]/gp
    s2$PlayoffGP.Tm[h]<-s2$PlayoffGP.Opp[r]<-ifelse(x %in% p$Tm,p$Games[which(p$Tm==x)],0)
    s2$PlayoffWins.Tm[h]<-s2$PlayoffWins.Opp[r]<-ifelse(x %in% p$Tm,p$Wins[which(p$Tm==x)],0)
  }
  #A<-cbind(s2,d)
  return(s2)
}

tabell2<-function(s1,snu){
  t=unique(snu$Tm)
  for(i in 1:30){
    sub<-s1%>%
      filter(Tm==x|Opp==x)%>%
      mutate(Res2=ifelse(Tm==x,Res,1-Res))
    so=which(sub$Ovr=="SO")
    ot=which(sub$Ovr=="OT")
    reg=which(sub$Ovr=="")
  }
}

hth<-function(s1,s2){
  s=rbind(s1,s2)
  m=length(s$Tm)
  s$Rk<-c(1:m)
  teams=unique(s$Tm)
  s$HthWP<-s$LastHth<-s$DiffHth<-c(rep(0,m))#numeric(m)
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
      s$HthWP[hem]<-append(0,head(cumsum(h1$XGames)/seq_along(h1$XGames),-1))[home]
      s$LastHth[hem]<-append(0,head(h1$XGames,-1))[home]
      s$DiffHth[hem]<-append(0,head(cumsum(h1$diff)/seq_along(h1$diff),-1))[home]
    }
  }
  S<-s[which(s$season==s2$season[1]),]
  S$Rk<-c(1:length(S$Rk))
  return(S)
}

tabort<-function(h,n){
  m<-c(1,2)
  tm=unique(h$Tm)
  for (i in 1:30){
    x=tm[i]
    nr<-which(h$Tm==x|h$Opp==x)[1:n]
    m<-append(nr,m)
  }
  q=unique(m)
  data<-h[-q,]
  return(data)
}

last<-function(s1,p1,s2,n){
  t=unique(s2$Tm)
  a<-hth(s1,s2)
  c<-thisseason(a,t)
  d<-tabell(s1,p1,c)
  e<-tabort(d,n)
  e<-e%>%
    mutate(WPD=WP.Tm-WP.Opp,WPD.ls=WP.Tm.ls-WP.Opp.ls,GPD=PlayoffGP.Tm-PlayoffGP.Opp,OvtD=OvertimeP.Tm-OvertimeP.Opp,
           LG=LastGame.Tm-LastGame.Opp,PWD=PlayoffWins.Tm-PlayoffWins.Opp,Pims=Pims.Tm-Pims.Opp,LG3=Last3Games.Tm-Last3Games.Opp)
  return(e)
}
