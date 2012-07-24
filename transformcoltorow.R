transformcoltorow<-function(x)
{
  uniuser<-unique(x$CustomerID);
  number<-6;
  diffmat<-matrix(rep(0,length(uniuser)*number),nrow=length(uniuser));
  for(i in 1:length(uniuser))
  {
    for(j in 1:number)
    {
      diffmat[i,j]<-x$DayDiff[which(x$CustomerID==uniuser[i]&x$LoanNumber==j)];
      diffmat[i,j]<-ifelse(diffmat[i,j]==0,0.5,diffmat[i,j]);
    }
  }
  return(diffmat);
}