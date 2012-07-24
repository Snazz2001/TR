IdentifyBAD4MaxUtil<-function(x){
  len<-dim(x)[1];
  isBADBe4<- 0 ;
  BadUserList<-c();
  for (i in 1:len)
  {
    if(i > 1)
    {
      userid<-x[i,'UserID'];
      if(userid!=preuserid)
      {
        if(isBADBe4<2)
        {
          if(prevData==1)
          {
            BadUserList<-c(BadUserList,preuserid);
          }
        }
        isBADBe4 = 0;
      }
      else
      {
        if(x[i,'BAD45']==1)
         isBADBe4<-isBADBe4 + 1;
      }
    }
    else
    {
      if(x[i,'BAD45']==1)
        isBADBe4<-isBADBe4 + 1;      
    }
    preuserid<-x[i,'UserID'];
    prevData<-x[i,'BAD45'];
  }
  return(BadUserList)
}