calculateDaySinceLastPayOff<-function(x)
{
  len<-dim(x)[1];
  x$DayDiff<-0;
  precustomer<-0;
  isfirst<-0;
  for(i in 1:len)
  {
    curcustomer<-x[i,"CustomerID"];
    if(i==1)
    {
      x[i,"DayDiff"]<-0;
      print("enter here once");
    }
    else
    {
        if(curcustomer==precustomer)
        {
          if(isfirst==0)
          {
          x[i,"DayDiff"]<-x[i,"ApplicationDate"] - x[i-1,"PayBackDate"];
          print(x[i,"DayDiff"]);
          isfirs<-1;
          }
        }
        else if(curcustomer!=precustomer)
        {
          
          x[i,"DayDiff"]<-0;
          print("enter here a few times");
          print(precustomer);
          print(curcustomer);
          isfirst<-0;
        }
    }
    print(x[i,"DayDiff"]);
    precustomer<-curcustomer;
  }
#  print(x$DayDiff)
  return(x)
}