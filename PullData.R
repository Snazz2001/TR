setwd('C:/My Projects/Zheng - Trust Rating Logic - RAD - 272/')
maxutil<-read.csv("maxutilizationCustomerID.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);#conver the date info to proper format.
maxutil$PayBackDate<-as.Date(maxutil$PayBackDate,"%d/%m/%Y");
maxutil$DueDate<-as.Date(maxutil$DueDate,"%d/%m/%Y");
maxutil$NextDueDate<-as.Date(maxutil$NextDueDate,"%d/%m/%Y");
maxutil$ApplicationDate<-as.Date(maxutil$ApplicationDate,"%d/%m/%Y");
usertimes<-table(maxutil$UserID);
b<-dimnames(usertimes);
b<-unlist(b);
selected<-usertimes[]>5;#select the users who has at least 5 loan and each loan max out.
b[selected];#select all the users.
redmax<-maxutil[maxutil$UserID %in% b[selected],]

rem<-redmax[is.na(redmax$PayBackDate)&redmax$NextDueDate>"2012-01-30",]#those data has missing value on PayBackDate and has not been classified as default.

c<-setdiff(redmax$ProposalID,rem$ProposalID);
fdata<-redmax[redmax$ProposalID %in%c,];
fdata$diff<-fdata$PayBackDate-fdata$NextDueDate;
fdata$diff[is.na(fdata$diff)]<-46;
fdata$BAD45<-ifelse(fdata$diff>45,1,0);#define the bad45 for prediction
baduserid<-fdata$UserID[which(fdata$BAD==1)];#find all users who are bad
fdata[fdata$UserID %in% baduserid,];
baduserinfo<-fdata[fdata$UserID %in% baduserid,];
write.csv(baduserinfo,file="baduserinfo.csv");

uids<-IdentifyBAD4MaxUtil(baduserinfo);
badcases<-maxutil[maxutil$UserID %in% uids,];

badcasefreq<-table(badcases$CustomerID);
badcustomerid<-dimnames(badcasefreq);
head(badcustomerid);
write.csv(badcustomerid,file="badcustomerid.csv");
---------------------------------------------------------------------------------------------------------
utilbad<-read.csv("utilizationBad.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);#conver the date info to proper format.  
bad45<-utilbad[which(utilbad$Bad==1),]
bad45$util<-bad45$LoanAmount/bad45$MaxLoanAmount;
bad45$util<-ifelse(bad45$util>=1,1,bad45$util);
good45<-utilbad[which(utilbad$Bad==0),]
good45$util<-good45$LoanAmount/good45$MaxLoanAmount;
good45$util<-ifelse(good45$util>=1,1,good45$util);
bad45$TRBin<-ifelse(bad45$TrustRating<=600,1,ifelse(bad45$TrustRating<=800,2,ifelse(bad45$TrustRating<=1000,3,-1)));
bad45$TRBinf<-factor(bad45$TRBin,labels=c("TrustRate<=600","600<TrustRate<=800","800<TrustRate<=1000"));
histogram(~util|TRBinf,data=bad45)

good45$TRBin<-ifelse(good45$TrustRating<=600,1,ifelse(good45$TrustRating<=800,2,ifelse(good45$TrustRating<=1000,3,-1)));
good45$TRBinf<-factor(good45$TRBin,labels=c("TrustRate<=600","600<TrustRate<=800","800<TrustRate<=1000"));
histogram(~util|TRBinf,data=good45);

badindex<-1:15702;
bind<-sample(badindex,1000);
goodindex<-1:1398595;
gind<-sample(goodindex,1000);
bdata<-bad45[bind,];
gdata<-good45[gind,];
mdata<-rbind(bdata,gdata);
library(party)
tree<-ctree(TrustRating~DaysSinceLastProposal+LoanAmountAgreed+LoanTerm+Delinquency+Bad+util,data=mdata);
plot(tree);

tree<-ctree(TrustRating~AllArrears+AllDelinquency+Term+BAD45+DaysSinceLastProposal+DaysSincePayOff+EarlyPayOff_num+FULL_INDEBT_TMB+LastArrears+LoanNumber+LoanToIncome+MaxDelinquency+NumberOfStringLoans3+PrevDel20+PrevD760+PrevDelinquency+PreviousCash+PrevTerm+RegNumberPresent+SEARCH_ALLCREDIT_BFB+TenDel15+AvgArrears+AvgCash+AvgDelinquency+AvgTimesExtended+Trend_Cash+Delinquency+Util,data=trdatabad)
------------------------------------------------------------------------------------------------
util<-read.csv("utilizationTrustRating.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);#conver the date info to proper format.
colnames(util)[1]<-'ProposalID';
util$Bad[which(is.na(util$PayBackDate))]<-1;#note that when the paybackdate is null, which can be the user did not pay back or the paybackdate is not due.
bad45<-util[which(util$Bad==1),];
bad45$util<-bad45$LoanAmountAgreed/bad45$MaxLoanAmount;
bad45$util<-ifelse(bad45$util>=1,1,bad45$util);
good45<-util[which(util$Bad==0),];
good45$util<-good45$LoanAmountAgreed/good45$MaxLoanAmount;
good45$util<-ifelse(good45$util>=1,1,good45$util);
bad45$TRBin<-ifelse(bad45$TrustRating<=600,1,ifelse(bad45$TrustRating<=800,2,ifelse(bad45$TrustRating<=1000,3,-1)));
bad45$TRBinf<-factor(bad45$TRBin,labels=c("TrustRate<=600","600<TrustRate<=800","800<TrustRate<=1000"));
good45$TRBin<-ifelse(good45$TrustRating<=600,1,ifelse(good45$TrustRating<=800,2,ifelse(good45$TrustRating<=1000,3,-1)));
good45$TRBinf<-factor(good45$TRBin,labels=c("TrustRate<=600","600<TrustRate<=800","800<TrustRate<=1000"));
histogram(~util,data=bad45);
histogram(~util,data=good45);
histogram(~util|TRBinf,data=bad45);
histogram(~util|TRBinf,data=good45);
good45$TRBin<-ifelse(good45$TrustRating<=600,1,ifelse(good45$TrustRating<=800,2,ifelse(good45$TrustRating<=1000,3,-1)));
good45$TRBinf<-factor(good45$TRBin,labels=c("TrustRate<=600","600<TrustRate<=800","800<TrustRate<=1000"));
histogram(~util|TRBinf,data=good45);
#util$TopUp<-0;
#util$Ext<-0;

topup<-read.csv("TotalTopUp.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);#conver the date info to proper format.
colnames(topup)<-c('ProposalID','TopUp');
extend<-read.csv("TotalExt.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);#conver the date info to proper format.
colnames(extend)<-c('ProposalID','Ext');
util<-merge(util,topup,by='ProposalID',all.x=TRUE);
util<-merge(util,extend,by='ProposalID',all.x=TRUE);
util[is.na(util$TopUp),'TopUp']<-0;
util[is.na(util$Ext),'Ext']<-0;
sum(util$Ext==0)/dim(util)[1];
sum(util$TopUp==0)/dim(util)[1];

sum(util$Bad[which(util$LoanNumber==1)])/length(util$Bad[which(util$LoanNumber==1)]);#the arrear rate for ln

topupuser<-util$CustomerID[which(util$TopUp>0)];
extuser<-util$CustomerID[which(util$Ext>0)];
length(unique(topupuser));
length(unique(extuser));
topfreq<-table(topupuser)
extfreq<-table(extuser);
topfredf<-as.data.frame(topfreq);
extfredf<-as.data.frame(extfreq);

topfreq<-as.data.frame(table(topfredf$Freq));
extfreq<-as.data.frame(table(extfredf$Freq));
topfreq$proportion<-topfreq$Freq/sum(topfreq$Freq);
extfreq$proportion<-extfreq$Freq/sum(extfreq$Freq);
colnames(topfreq)[1]<-"Top Up Frequency";
colnames(extfreq)[1]<-"Extension Frequency";

barplot(topfreq[,3],names.arg=c(1,2,3,4,5,6,7,8,9,10,11),xlab="Top Up Frequency",ylab="Relative Frequency",main="Top Up Frequency")
barplot(extfreq[,3],xlab="Extension Frequency",ylab="Relative Frequency",names.arg=c(1,2,3,4,5,6,7),main="Extension Frequency")

a1<-util[which(util$TopUp==1),];
dim(a1[which(a1$Bad==1),])[1]/dim(a1)[1];
a2<-util[which(util$TopUp==2),];
dim(a2[which(a2$Bad==1),])[1]/dim(a2)[1];

TopUpFreq<-c(0,1,2,3,4,5,6);
BadRate<-c(0.0916,0.1377,0.1437,0.1617,0.1887,0.2131,0.3333);
plot(TopUpFreq,BadRate,type="b",pch=20,lty=0);
top<-data.frame(TopUpFreq,BadRate);
toplm<-lm(BadRate~TopUpFreq,data=top);
abline(toplm);

e1<-util[which(util$Ext==1),];
dim(e1[which(e1$Bad==1),])[1]/dim(e1)[1];
BadRateE<-c(0.0833,0.2503,0.4721,0.3949);
barplot(BadRateE,main="Bad Rate vs #Extension",xlab="# Extension", ylab="Bad Rate",names.arg=c(0,1,2,3),col=rainbow(4))

util$Badf<-ifelse(util$Bad==0,"Good","Bad");
util$Extf<-""
util$Extf[which(util$Ext==1)]<-"Ext 1";
util$Extf[which(util$Ext==2)]<-"Ext 2";
util$Extf[which(util$Ext==3)]<-"Ext 3";
util$Extf[which(util$Ext==0)]<-"Ext 0";
histogram(~TrustRating|Extf+Badf,data=util);

util$TopUpf<-""
util$TopUpf[which(util$TopUp==0)]<-"TopUp 0";
util$TopUpf[which(util$TopUp==1)]<-"TopUp 1";
util$TopUpf[which(util$TopUp==2)]<-"TopUp 2";
util$TopUpf[which(util$TopUp==3)]<-"TopUp 3";
util$TopUpf[which(util$TopUp==4)]<-"TopUp 4";
util$TopUpf[which(util$TopUp==5)]<-"TopUp 5";
util$TopUpf[which(util$TopUp==6)]<-"TopUp 6";
histogram(~TrustRating|TopUpf+Badf,data=util);

mixscore<-read.csv("mixscore.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);#conver the date info to proper format.
colnames(mixscore)[1]<-'ProposalID';
mixscore<-data.frame(mixscore$ProposalID,mixscore$MixedScore)
colnames(mixscore)[1]<-'ProposalID';
colnames(mixscore)[2]<-'MixedScore';
#util$MixedScore<-sapply(mixscore$MixedScore,identity)
util<-merge(util,mixscore,by='ProposalID',all.x=TRUE)
library(lattice);
bwplot(Badf~MixedScore,data=util,ylab='Loan Outcome',xlab="MixedScore");

badloanNo<-util$LoanNumber[which(util$Bad==1)];
blntab<-table(badloanNo);
blntab<-as.data.frame(blntab);
colnames(blntab)<-c("LoanNumber","Freq");
plot(blntab$LoanNumber,log(blntab$Freq),type="b",pch=3,lty=2,xlab="LoanNumber",ylab="log(Freq)");


library('reldist');
top2bad<-hist(util$TrustRating[which(util$Bad==1&util$TopUp==2)]);
gini(top2bad$counts);
top2good<-hist(util$TrustRating[which(util$Bad==0&util$TopUp==2)]);
gini(top2good$counts);

----------------------------------------------------------------------
tran<-read.csv("transaction.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);#conver the date info to proper format.
tranbad<-tran[which(tran$Bad==1),];
colnames(tranbad)[1]<-'ProposalID';
baduser<-unique(tranbad$CustomerID);
tran<-tran[which(tran$CustomerID %in% baduser),];
colnames(tran)[1]<-'ProposalID';
remuser<-unique(tran$CustomerID[which(tran$Bad==1&tran$LoanNumber==0)]);
myvars<-tran$CustomerID %in% remuser;
tran[!myvars,];

-----------------------------------------------------------------------
sv<-read.csv("samplevelocity.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);#conver the date info to proper format.
colnames(sv)[1]<-'ProposalID';
sv$ApplicationDate<-as.Date(as.character(sv$ApplicationDate),"%Y%m%d");
sv$PayBackDate<-as.Date(as.character(sv$PayBackDate),"%Y%m%d");
sv$NextDueDate<-as.Date(as.character(sv$NextDueDate),"%Y%m%d");

sv1<-calculateDaySinceLastPayOff(sv);
sv1$Bad[which(is.na(sv1$PayBackDate))]<-1;
sv1mat<-transformcoltorow(sv1);
vel<-1/sv1mat;
acc<-matrix(rep(0,50),nrow=10);
acc[,1]<-vel[,2]-vel[,1];
acc[,2]<-vel[,3]-vel[,2];
acc[,3]<-vel[,4]-vel[,3];
acc[,4]<-vel[,5]-vel[,4];
acc[,5]<-vel[,6]-vel[,5];
vv<-apply(acc,1,sum);
mean(vv);
sd(vv);

v<-apply(vel,2,mean);
vel<-rbind(vel,v);
write.csv(vel,file="vel.csv");

mv<-matrix(rep(0,60),nrow=10);
mv[,6]<-(vel[1:10,6]+vel[1:10,5])/2;
mv[,5]<-(vel[1:10,5]+vel[1:10,4])/2;
mv[,4]<-(vel[1:10,4]+vel[1:10,3])/2;
mv[,3]<-(vel[1:10,3]+vel[1:10,2])/2;
mv[,2]<-(vel[1:10,2]+vel[1:10,1])/2;
mv[,1]<-(vel[1:10,1])/2;

mve[,1]<-(vel[1:10,1])*a;
mve[,2]<-(a*vel[1:10,2]+(1-a)*mve[,1]);
mve[,3]<-(a*vel[1:10,3]+(1-a)*mve[,2]);
mve[,4]<-(a*vel[1:10,4]+(1-a)*mve[,3]);
mve[,5]<-(a*vel[1:10,5]+(1-a)*mve[,4]);
mve[,6]<-(a*vel[1:10,6]+(1-a)*mve[,5]);

---------------------------------------------------------------------
newcustomer<-util$CustomerID[which(util$LoanNumber==0)];
newcustomer<-unique(newcustomer);
newitems<-util[util$CustomerID %in% newcustomer,];
l6users<-newitems$CustomerID[which(newitems$LoanNumber>5)];#filer out the customer who commit less than 6 loans during this period.
l6users<-unique(l6users);
l6util<-util[util$CustomerID %in% l6users,];
l6util<-l6util[order(l6util$CustomerID,l6util$ApplicationDate),];
l6util$ApplicationDate<-as.Date(l6util$ApplicationDate,format="%Y-%m-%d");
l6util$PayBackDate<-as.Date(l6util$PayBackDate,format="%Y-%m-%d");
l6util$NextDueDate<-as.Date(l6util$NextDueDate,format="%Y-%m-%d");

l6util<-calculateDaySinceLastPayOff(l6util)
l6util$util<-l6util$LoanAmountAgreed/l6util$MaxLoanAmount;

baddata<-l6util$CustomerID %in% c(1345738);
l6utilg<-l6util[!baddata];

bb<-table(l6utilg$CustomerID,l6utilg$Bad);
bbm<-as.matrix(bb);
bbm[which(bbm[,2]==1),];
onearrear<-bbm[which(bbm[,2]==1),];
zeroarrear<-bbm[which(bbm[,2]==0),];
l6utilg$DayDiffTermRatio<-l6utilg$DayDiff/l6utilg$LoanTerm;
l6utilg$TRBin<-ifelse(l6utilg$TrustRating<=600,1,ifelse(l6utilg$TrustRating<=800,2,ifelse(l6utilg$TrustRating<=1000,3,-1)));
l6utilg$TRBinf<-factor(l6utilg$TRBin,labels=c("TrustRate<=600","600<TrustRate<=800","800<TrustRate<=1000"));

badcustomerid<-as.numeric(rownames(oneareardf));
l0data<-l6utilg[which((l6utilg$CustomerID %in% badcustomerid)&l6utilg$LoanNumber==0),];
lnbaddata<-l6utilg[which((l6utilg$CustomerID %in% badcustomerid)&l6utilg$Bad==1),];
colcomdata<-merge(l0data,lnbaddata,by="CustomerID");
colcomdata$span<-colcomdata$ApplicationDate.y-colcomdata$ApplicationDate.x;
span<-as.numeric(colcomdata$span);
hist(span,main="Time span for default after first application date");

write.csv(colcomdata$ProposalID.x,file="id.csv");
colnames(colcomdata)[2]<-"ProposalID";
colnames(features)[1]<-"ProposalID"
colcomdata2<-merge(colcomdata,features,by="ProposalID");
train<-sample(colcomdata2$ProposalID,size=400,replace=FALSE);
traindata<-colcomdata2[which(colcomdata2$ProposalID %in% train),];
testdata<-colcomdata2[which(!colcomdata2$ProposalID %in% train),];

utils<-utilizaionAfterfrozen(l6utilg[-31465,]);
utils$diff<-utils$curutils-utils$preutils;
utils$bin<-ifelse(utils$diff>0,1,ifelse(utils$diff==0,0,-1));
table(utils$bin);
plotdata<-data.frame(colcomdata$MixedScore.y,span,colcomdata$Bad.y);
colnames(plotdata)<-c("MixedScore","Span","Arrear45");
h2d<-hist2d(plotdata$MixedScore,plotdata$Span,nbins=c(10,9));
h2d$counts;
write.csv(h2d$counts,file="counts.csv");
plotdata2<-data.frame(colcomdata$LoanNumber.y,span,colcomdata$Bad.y);
colnames(plotdata2)<-c("LoanNumber","Span","Arrear45");
h2d2<-hist2d(plotdata2$LoanNumber,plotdata2$Span,nbins=c(10,9));


traindata$spanbin<-ifelse(traindata$span<=40,0,ifelse(traindata$span<=80,1,ifelse(traindata$span<=120,2,ifelse(traindata$span<=160,3,4))));
testdata$spanbin<-ifelse(testdata$span<=40,0,ifelse(testdata$span<=80,1,ifelse(testdata$span<=120,2,ifelse(testdata$span<=160,3,4))));
traindata$labels<-class.ind(traindata$spanbin);
testdata$labels<-class.ind(testdata$spanbin);




