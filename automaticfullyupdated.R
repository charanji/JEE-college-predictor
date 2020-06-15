setwd("C:/Users/Mahe/Desktop/R/RIIT")
year2017<-read.csv("2017whole1.csv")
year2018<-read.csv("2018whole1.csv")

collegelist<-function(rank,category){
  if (category== 'OBC-NCL'){
    obcncl<-year2017[year2017$Category=='OBC-NCL',]
    rankbetween<- obcncl[obcncl$Opening.Rank<rank & obcncl$Closing.Rank>rank,]
    rankabove<- obcncl[obcncl$Opening.Rank>rank,]
    rowbind2017<-rbind(rankbetween,rankabove)
    preferenceorderRankwise<-rowbind2017[order(rowbind2017$Opening.Rank),]
    preferenceorderBranchwise<-rowbind2017[order(rowbind2017$Academic.Program.Name,
                                                 rowbind2017$Opening.Rank),]
    write.csv(preferenceorderRankwise,'2017preferencerank.csv')
    write.csv(preferenceorderBranchwise,'2017preferencebranch.csv')
    
    obcncl<-year2018[year2018$Category=='OBC-NCL',]
    rankbetween<- obcncl[obcncl$Opening.Rank<rank & obcncl$Closing.Rank>rank,]
    rankabove<- obcncl[obcncl$Opening.Rank>rank,]
    rowbind2018<-rbind(rankbetween,rankabove)
    preferenceorderRankwise<-rowbind2018[order(rowbind2018$Opening.Rank),]
    preferenceorderBranchwise<-rowbind2018[order(rowbind2018$Academic.Program.Name,
                                                 rowbind2018$Opening.Rank),]
    write.csv(preferenceorderRankwise,'2018preferencerank.csv')
    write.csv(preferenceorderBranchwise,'2018preferencebranch.csv')
  }
  
  if (category== 'SC'){
    sc<-year2017[year2017$Category=='SC',]
    rankbetween<- sc[sc$Opening.Rank<rank & sc$Closing.Rank>rank,]
    rankabove<- sc[sc$Opening.Rank>rank,]
    rowbind2017<-rbind(rankbetween,rankabove)
    preferenceorderRankwise<-rowbind2017[order(rowbind2017$Opening.Rank),]
    preferenceorderBranchwise<-rowbind2017[order(rowbind2017$Academic.Program.Name,
                                                 rowbind2017$Opening.Rank),]
    write.csv(preferenceorderRankwise,'2017preferencerank.csv')
    write.csv(preferenceorderBranchwise,'2017preferencebranch.csv')
    
    sc<-year2018[year2018$Category=='SC',]
    rankbetween<- sc[sc$Opening.Rank<rank & sc$Closing.Rank>rank,]
    rankabove<- sc[sc$Opening.Rank>rank,]
    rowbind2018<-rbind(rankbetween,rankabove)
    preferenceorderRankwise<-rowbind2018[order(rowbind2018$Opening.Rank),]
    preferenceorderBranchwise<-rowbind2018[order(rowbind2018$Academic.Program.Name,
                                                 rowbind2018$Opening.Rank),]
    write.csv(preferenceorderRankwise,'2018preferencerank.csv')
    write.csv(preferenceorderBranchwise,'2018preferencebranch.csv')
  }
  
  if (category== 'ST'){
    st<-year2017[year2017$Category=='ST',]
    rankbetween<- st[st$Opening.Rank<rank & st$Closing.Rank>rank,]
    rankabove<- st[st$Opening.Rank>rank,]
    rowbind2017<-rbind(rankbetween,rankabove)
    preferenceorderRankwise<-rowbind2017[order(rowbind2017$Opening.Rank),]
    preferenceorderBranchwise<-rowbind2017[order(rowbind2017$Academic.Program.Name,
                                                 rowbind2017$Opening.Rank),]
    write.csv(preferenceorderRankwise,'2017preferencerank.csv')
    write.csv(preferenceorderBranchwise,'2017preferencebranch.csv')
    
    st<-year2018[year2018$Category=='ST',]
    rankbetween<- st[st$Opening.Rank<rank & st$Closing.Rank>rank,]
    rankabove<- st[st$Opening.Rank>rank,]
    rowbind2018<-rbind(rankbetween,rankabove)
    preferenceorderRankwise<-rowbind2018[order(rowbind2018$Opening.Rank),]
    preferenceorderBranchwise<-rowbind2018[order(rowbind2018$Academic.Program.Name,
                                                 rowbind2018$Opening.Rank),]
    write.csv(preferenceorderRankwise,'2018preferencerank.csv')
    write.csv(preferenceorderBranchwise,'2018preferencebranch.csv')
  }
  
  if (category== 'General'){
    general<-year2017[year2017$Category=='General',]
    rankbetween<- general[general$Opening.Rank<rank & general$Closing.Rank>rank,]
    rankabove<- general[general$Opening.Rank>rank,]
    rowbind2017<-rbind(rankbetween,rankabove)
    preferenceorderRankwise<-rowbind2017[order(rowbind2017$Opening.Rank),]
    preferenceorderBranchwise<-rowbind2017[order(rowbind2017$Academic.Program.Name,
                                                 rowbind2017$Opening.Rank),]
    write.csv(preferenceorderRankwise,'2017preferencerank.csv')
    write.csv(preferenceorderBranchwise,'2017preferencebranch.csv')
    
    general<-year2018[year2018$Category=='General',]
    rankbetween<- general[general$Opening.Rank<rank & general$Closing.Rank>rank,]
    rankabove<- general[general$Opening.Rank>rank,]
    rowbind2018<-rbind(rankbetween,rankabove)
    preferenceorderRankwise<-rowbind2018[order(rowbind2018$Opening.Rank),]
    preferenceorderBranchwise<-rowbind2018[order(rowbind2018$Academic.Program.Name,
                                                 rowbind2018$Opening.Rank),]
    write.csv(preferenceorderRankwise,'2018preferencerank.csv')
    write.csv(preferenceorderBranchwise,'2018preferencebranch.csv')
  }
  
  if (category== 'OBC-NCL(PwD)'){
    obcpwd<-year2017[year2017$Category=='OBC-NCL(PwD)',]
    rankbetween<- obcpwd[obcpwd$Opening.Rank<rank & obcpwd$Closing.Rank>rank,]
    rankabove<- obcpwd[obcpwd$Opening.Rank>rank,]
    rowbind2017<-rbind(rankbetween,rankabove)
    preferenceorderRankwise<-rowbind2017[order(rowbind2017$Opening.Rank),]
    preferenceorderBranchwise<-rowbind2017[order(rowbind2017$Academic.Program.Name,
                                                 rowbind2017$Opening.Rank),]
    write.csv(preferenceorderRankwise,'2017preferencerank.csv')
    write.csv(preferenceorderBranchwise,'2017preferencebranch.csv')
    
    obcpwd<-year2018[year2018$Category=='OBC-NCL(PwD)',]
    rankbetween<- obcpwd[obcpwd$Opening.Rank<rank & obcpwd$Closing.Rank>rank,]
    rankabove<- obcpwd[obcpwd$Opening.Rank>rank,]
    rowbind2018<-rbind(rankbetween,rankabove)
    preferenceorderRankwise<-rowbind2018[order(rowbind2018$Opening.Rank),]
    preferenceorderBranchwise<-rowbind2018[order(rowbind2018$Academic.Program.Name,
                                                 rowbind2018$Opening.Rank),]
    write.csv(preferenceorderRankwise,'2018preferencerank.csv')
    write.csv(preferenceorderBranchwise,'2018preferencebranch.csv')
  }
  
  if (category== 'SC(PwD)'){
    scpwd<-year2017[year2017$Category=='SC PwD',]
    rankbetween<- scpwd[scpwd$Opening.Rank<rank & scpwd$Closing.Rank>rank,]
    rankabove<- scpwd[scpwd$Opening.Rank>rank,]
    rowbind2017<-rbind(rankbetween,rankabove)
    preferenceorderRankwise<-rowbind2017[order(rowbind2017$Opening.Rank),]
    preferenceorderBranchwise<-rowbind2017[order(rowbind2017$Academic.Program.Name,
                                                 rowbind2017$Opening.Rank),]
    write.csv(preferenceorderRankwise,'2017preferencerank.csv')
    write.csv(preferenceorderBranchwise,'2017preferencebranch.csv')
    
    scpwd<-year2018[year2018$Category=='SC PwD',]
    rankbetween<- scpwd[scpwd$Opening.Rank<rank & scpwd$Closing.Rank>rank,]
    rankabove<- scpwd[scpwd$Opening.Rank>rank,]
    rowbind2018<-rbind(rankbetween,rankabove)
    preferenceorderRankwise<-rowbind2018[order(rowbind2018$Opening.Rank),]
    preferenceorderBranchwise<-rowbind2018[order(rowbind2018$Academic.Program.Name,
                                                 rowbind2018$Opening.Rank),]
    write.csv(preferenceorderRankwise,'2018preferencerank.csv')
    write.csv(preferenceorderBranchwise,'2018preferencebranch.csv')
  }
  
  if (category== 'ST(PwD)'){
    stpwd<-year2017[year2017$Category=='ST PwD',]
    rankbetween<- stpwd[stpwd$Opening.Rank<rank & stpwd$Closing.Rank>rank,]
    rankabove<- stpwd[stpwd$Opening.Rank>rank,]
    rowbind2017<-rbind(rankbetween,rankabove)
    preferenceorderRankwise<-rowbind2017[order(rowbind2017$Opening.Rank),]
    preferenceorderBranchwise<-rowbind2017[order(rowbind2017$Academic.Program.Name,
                                                 rowbind2017$Opening.Rank),]
    write.csv(preferenceorderRankwise,'2017preferencerank.csv')
    write.csv(preferenceorderBranchwise,'2017preferencebranch.csv')
    
    stpwd<-year2018[year2018$Category=='ST PwD',]
    rankbetween<- stpwd[stpwd$Opening.Rank<rank & stpwd$Closing.Rank>rank,]
    rankabove<- stpwd[stpwd$Opening.Rank>rank,]
    rowbind2018<-rbind(rankbetween,rankabove)
    preferenceorderRankwise<-rowbind2018[order(rowbind2018$Opening.Rank),]
    preferenceorderBranchwise<-rowbind2018[order(rowbind2018$Academic.Program.Name,
                                                 rowbind2018$Opening.Rank),]
    write.csv(preferenceorderRankwise,'2018preferencerank.csv')
    write.csv(preferenceorderBranchwise,'2018preferencebranch.csv')
  }
  
  if (category== 'General(PwD)'){
    generalpwd<-year2017[year2017$Category=='General PwD',]
    rankbetween<- generalpwd[generalpwd$Opening.Rank<rank & generalpwd$Closing.Rank>rank,]
    rankabove<- generalpwd[generalpwd$Opening.Rank>rank,]
    rowbind2017<-rbind(rankbetween,rankabove)
    preferenceorderRankwise<-rowbind2017[order(rowbind2017$Opening.Rank),]
    preferenceorderBranchwise<-rowbind2017[order(rowbind2017$Academic.Program.Name,
                                                 rowbind2017$Opening.Rank),]
    write.csv(preferenceorderRankwise,'2017preferencerank.csv')
    write.csv(preferenceorderBranchwise,'2017preferencebranch.csv')
    
    generalpwd<-year2018[year2018$Category=='General PwD',]
    rankbetween<- generalpwd[generalpwd$Opening.Rank<rank & generalpwd$Closing.Rank>rank,]
    rankabove<- generalpwd[generalpwd$Opening.Rank>rank,]
    rowbind2018<-rbind(rankbetween,rankabove)
    preferenceorderRankwise<-rowbind2018[order(rowbind2018$Opening.Rank),]
    preferenceorderBranchwise<-rowbind2018[order(rowbind2018$Academic.Program.Name,
                                                 rowbind2018$Opening.Rank),]
    write.csv(preferenceorderRankwise,'2018preferencerank.csv')
    write.csv(preferenceorderBranchwise,'2018preferencebranch.csv')
  }
}
