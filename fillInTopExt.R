fillInTopExt<-function(util,x){
  len<-dim(x)[1];
  i<-1;
  while (i<=len)
  {
    proposalId<-x[i,1];
    util[which(util$ProposalID==proposalId),'TopUp']<-x[i,2];    
    i<-i+1;
    print(i);
  }  
}