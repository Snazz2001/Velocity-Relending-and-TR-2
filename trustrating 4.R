setwd('C:/My Projects/Zheng - Velocity or Re-Lending and Trust Rating 2/');

weka: C:\Program Files\Weka-3-6>java -Xmx4g -jar weka.jar
-----------SQL-------------------------------
SELECT A.ProposalID,
A.CustomerID,
convert(char(10),A.ApplicationDate,112) as ApplicationDate,
convert(char(10),A.PayBackDate,112) as PaybackDate,
convert(char(10),A.NextDueDate,112) as NextDueDate,
R.LoanNumber,
pp.DaysSinceLastProposal,
TotalGoodsCashPrice as LoanAmountAgreed,
P.OriginalLoanAmount as LoanAmountOriginal,
a.Term as LoanTerm,
TrustRating,
R.MixedScore,
em.AnnualIncome,
A.AgreementReference,
isnull(DATEDIFF(dd,A.NextDueDate,A.PayBackDate), 500) as Delinquency,
case when isnull(DATEDIFF(dd,A.NextDueDate,A.PayBackDate),500)>45 then 1 else 0 end as Bad into #temp
  FROM [WongaWholeStagingv2].[redline].[Agreements] A
  inner join WongaWholeStagingv2.wongalook.Result R on R.ProposalID=A.ProposalID
  inner join WongaWholeStagingv2.greyface.application ap on ap.agreement_reference = A.AgreementReference
  inner join WongaWholeStagingv2.greyface.EmploymentDetails em on em.EmploymentDetailId = ap.EmploymentDetailId
  --inner join WongaWholeStagingv2.redline.Transactions t on t.AgreementReference = A.AgreementReference
  --inner join WongaLook.data.Application AP on AP.ProposalID=A.ProposalID
  inner join WongaWholeStagingv2.redline.Proposals P on P.ProposalID = A.ProposalID
  left join WongaWholeStagingv2.dbo.PrevProposalData pp on R.ProposalID = pp.ProposalId
  where  A.ApplicationDate BETWEEN '2011-07-01' AND '2011-12-31' 
  
    select t.AgreementReference, COUNT(*) as Total_Extentions into #ext
from WongaWholeStagingV2.redline.Transactions t 
inner join #temp te on te.AgreementReference = t.AgreementReference
where TransactionType = 100 group by t.AgreementReference

select 
t.agreementreference,
COUNT(*)Total_TopUps into #top
from WongaWholeStagingV2.redline.Transactions t 
inner join #temp te on te.AgreementReference = t.AgreementReference
where TransactionType = 20 and (Reference like '%top up%' or isnumeric(reference)> 0) 
group by t.agreementreference

select ProposalID,CustomerID,ApplicationDate,PaybackDate,NextDueDate,LoanNumber,DaysSinceLastProposal,LoanAmountAgreed,LoanAmountOriginal,LoanTerm,TrustRating,
MixedScore,AnnualIncome,Delinquency,Bad,Total_Extentions,Total_TopUps into #mydata from  #temp te
left join #ext ex on te.agreementreference = ex.agreementreference 
left join #top topup on te.agreementreference = topup.agreementreference

select * from #mydata

select a.ProposalID,a.CustomerID,a.ApplicationDate as FirstApplicationDate,a.PaybackDate as FirstPaybackDate,a.NextDueDate as FirstNextDueDate,a.LoanNumber as FirstLoanNumber,a.DaysSinceLastProposal as FirstDaysSinceLastProposal,a.LoanAmountAgreed as FirstLoanAmountAgreed,a.LoanAmountOriginal as FirstLoanAmountOriginal,a.LoanTerm as FirstLoanTerm,a.TrustRating as FirstTrustRating,a.
MixedScore as FirstMixedScore,a.AnnualIncome as FirstAnnualIncome,a.Delinquency as FirstDelinquency,a.Bad as FirstBad,a.Total_Extentions as FirstTotalExt,a.Total_TopUps as FirstTotalTop,b.ProposalID as SecondProposalID,b.ApplicationDate as SecondApplicationDate,b.PaybackDate as SecondPaybackDate,b.NextDueDate as SecondNextDueDate,b.LoanNumber as SecondLoanNumber,b.DaysSinceLastProposal as SecondDaysSinceLastProposal,b.LoanAmountAgreed as SecondLoanAmountAgreed,b.LoanAmountOriginal as SecondLoanAmountOriginal,b.LoanTerm as SecondLoanTerm,b.TrustRating as SecondTrustRating,b.
MixedScore as SecondMixedScore,b.AnnualIncome as SecondAnnualIncome,b.Delinquency as SecondDelinquency,b.Bad as SecondBad,b.Total_Extentions as SecondTotalExt,b.Total_TopUps as SecondTotalTop,c.LoanNumber as ThirdLoanNumber,c.CustomerID as ThirdCustomerID, c.Delinquency as ThirdDelinquency,c.Bad as ThirdBad,c.TrustRating as ThirdTrustRating from #mydata as a,#mydata as b,#mydata as c where a.CustomerID = b.CustomerID and a.CustomerID = c.CustomerID and b.LoanNumber - a.LoanNumber = 1 and
c.LoanNumber - b.LoanNumber = 1 order by a.CustomerID, a.ApplicationDate 
------------------------------------End of SQL------------------------------------------
nxloan<-read.csv("prednextloan.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
colnames(nxloan)[1]<-"ProposalID";
nxloan<-nxloan[order(nxloan$CustomerID,nxloan$SecondApplicationDate,decreasing = TRUE),];

nxloannr<-nxloan[!duplicated(nxloan$CustomerID),];
nxloannr$FirstTotalExt<-ifelse(is.na(nxloannr$FirstTotalExt),0,nxloannr$FirstTotalExt);
nxloannr$SecondTotalExt<-ifelse(is.na(nxloannr$SecondTotalExt),0,nxloannr$SecondTotalExt);
nxloannr$FirstTotalTop<-ifelse(is.na(nxloannr$FirstTotalTop),0,nxloannr$FirstTotalTop);
nxloannr$SecondTotalTop<-ifelse(is.na(nxloannr$SecondTotalTop),0,nxloannr$SecondTotalTop);

--derive new features-----------
nxloannr$FirstApplicationDate<-as.Date(as.character(nxloannr$FirstApplicationDate),format="%Y%m%d");
nxloannr$SecondApplicationDate<-as.Date(as.character(nxloannr$SecondApplicationDate),format="%Y%m%d");
nxloannr$FirstPaybackDate<-as.Date(as.character(nxloannr$FirstPaybackDate),format="%Y%m%d");
nxloannr$SecondPaybackDate<-as.Date(as.character(nxloannr$SecondPaybackDate),format="%Y%m%d");

nxloannr$SecondDaysSinceLastPayOff<-nxloannr$SecondApplicationDate-nxloannr$FirstPaybackDate;

nxloannr$AmountOrigDiff<-nxloannr$SecondLoanAmountOriginal - nxloannr$FirstLoanAmountOriginal;
nxloannr$AmountAgreeDiff<-nxloannr$SecondLoanAmountAgreed - nxloannr$FirstLoanAmountAgreed;
nxloannr<-nxloannr[!is.na(nxloannr$FirstMixedScore),]; #missing value, fill in the mean value.
nxloannr$ScoreDiff<-nxloannr$SecondMixedScore - nxloannr$FirstMixedScore;
nxloannr$utilization1<-nxloannr$FirstLoanAmountAgreed/nxloannr$FirstTrustRating;
nxloannr$utilization2<-nxloannr$SecondLoanAmountAgreed/nxloannr$SecondTrustRating;
nxloannr$utilization1<-ifelse(nxloannr$utilization1>1,1,nxloannr$utilization1)
nxloannr$utilization2<-ifelse(nxloannr$utilization2>1,1,nxloannr$utilization2)
nxloannr$utilRatio<-nxloannr$utilization2/nxloannr$utilization1;
nxloannr$utilDiff<-nxloannr$utilization2 - nxloannr$utilization1;
nxloannr$IncomeDiff<-nxloannr$SecondAnnualIncome - nxloannr$FirstAnnualIncome;
nxloannr<-nxloannr[!is.na(nxloannr$utilRatio),];
nxloannr<-nxloannr[!is.na(nxloannr$SecondMixedScore),];
nxloannr$trratio<-nxloannr$SecondTrustRating/nxloannr$FirstTrustRating;
nxloannr$utitrratio<-nxloannr$utilRatio/nxloannr$trratio;
nxloannr$FirstLoanIncome<-nxloannr$FirstLoanAmountOriginal/(nxloannr$FirstAnnualIncome/12);
nxloannr$SecondLoanIncome<-nxloannr$SecondLoanAmountOriginal/(nxloannr$SecondAnnualIncome/12);

nxloander<-data.frame(nxloannr$FirstLoanNumber,nxloannr$FirstDaysSinceLastProposal,nxloannr$FirstLoanAmountAgreed,nxloannr$FirstLoanAmountOriginal,nxloannr$FirstLoanTerm,nxloannr$FirstTrustRating,nxloannr$FirstMixedScore,nxloannr$FirstAnnualIncome,nxloannr$FirstDelinquency,nxloannr$FirstTotalExt,nxloannr$FirstTotalTop,nxloannr$SecondLoanNumber,nxloannr$SecondDaysSinceLastProposal,nxloannr$SecondLoanAmountAgreed,nxloannr$SecondLoanAmountOriginal,nxloannr$SecondLoanTerm,nxloannr$SecondTrustRating,nxloannr$SecondMixedScore,nxloannr$SecondAnnualIncome,nxloannr$SecondDelinquency,nxloannr$SecondTotalExt,nxloannr$SecondTotalTop,nxloannr$AmountOrigDiff,nxloannr$AmountAgreeDiff,nxloannr$ScoreDiff,nxloannr$utilization1,nxloannr$utilization2,nxloannr$utilRatio,nxloannr$utilDiff,nxloannr$IncomeDiff,nxloannr$trratio,nxloannr$utitrratio,nxloannr$logutitrratio,nxloannr$SecondDaysSinceLastPayOff,nxloannr$FirstLoanIncome,nxloannr$SecondLoanIncome,nxloannr$ThirdBad);

write.csv(nxloander,file="nxloander.csv");


library(ggplot2);
qplot(logutitrratio,SecondMixedScore,data=nxloannr,shape=as.factor(Bad),colour=as.factor(Bad))
qplot(as.numeric(SecondDaysSinceLastPayOff),ScoreDiff,data=nxloannr,shape=as.factor(Bad),colour=as.factor(Bad),facets=Bad~.);

library(car);
scatterplot.matrix(~ScoreDiff+logutitrratio+FirstMixedScore+SecondMixedScore+SecondDelinquency|Bad,data=nxloannr);



nxloannr<-data.frame(nxloannr,nxloannr$ThirdBad);
colnames(nxloannr)[46]<-"Bad";

plot(utilDiff[which(SecondBad==1)],ScoreDiff[which(SecondBad==1)],pch=SecondBad[which(SecondBad==1)]+20,col=SecondBad[which(SecondBad==1)]+2)
legend("topleft",title="Arrear type",c("Good","Arrear45"),pch=c(20,21),col=c("red","green"));

plot(AmountAgreeDiff,ScoreDiff,pch=Bad+20,col=Bad+2);
legend("topleft",title="Arrear type",c("Good","Arrear45"),pch=c(20,21),col=c("red","green"));

-----------------------------------------Trust Rating Velocity---------------------------------------------------------------

--------SQL------------------------
SELECT A.ProposalID,
A.CustomerID,
convert(char(10),A.ApplicationDate,112) as ApplicationDate,
convert(char(10),A.PayBackDate,112) as PaybackDate,
convert(char(10),A.NextDueDate,112) as NextDueDate,
R.LoanNumber,
pp.DaysSinceLastProposal,
TotalGoodsCashPrice as LoanAmountAgreed,
P.OriginalLoanAmount as LoanAmountOriginal,
a.Term as LoanTerm,
TrustRating,
R.MixedScore,
em.AnnualIncome,
A.AgreementReference,
isnull(DATEDIFF(dd,A.NextDueDate,A.PayBackDate), 500) as Delinquency,
case when isnull(DATEDIFF(dd,A.NextDueDate,A.PayBackDate),500)>45 then 1 else 0 end as Bad into #temp
  FROM [WongaWholeStagingv2].[redline].[Agreements] A
  inner join WongaWholeStagingv2.wongalook.Result R on R.ProposalID=A.ProposalID
  inner join WongaWholeStagingv2.greyface.application ap on ap.agreement_reference = A.AgreementReference
  inner join WongaWholeStagingv2.greyface.EmploymentDetails em on em.EmploymentDetailId = ap.EmploymentDetailId
  --inner join WongaWholeStagingv2.redline.Transactions t on t.AgreementReference = A.AgreementReference
  --inner join WongaLook.data.Application AP on AP.ProposalID=A.ProposalID
  inner join WongaWholeStagingv2.redline.Proposals P on P.ProposalID = A.ProposalID
  left join WongaWholeStagingv2.dbo.PrevProposalData pp on R.ProposalID = pp.ProposalId
  where  A.ApplicationDate BETWEEN '2011-01-01' AND '2011-07-01' and R.LoanNumber = 0 and TrustRating = 400 order by A.ApplicationDate;
select CustomerID into #custid from #temp;
SELECT A.ProposalID,
A.CustomerID,
convert(char(10),A.ApplicationDate,112) as ApplicationDate,
convert(char(10),A.PayBackDate,112) as PaybackDate,
convert(char(10),A.NextDueDate,112) as NextDueDate,
R.LoanNumber,
pp.DaysSinceLastProposal,
TotalGoodsCashPrice as LoanAmountAgreed,
P.OriginalLoanAmount as LoanAmountOriginal,
a.Term as LoanTerm,
TrustRating,
R.MixedScore,
em.AnnualIncome,
A.AgreementReference,
isnull(DATEDIFF(dd,A.NextDueDate,A.PayBackDate), 500) as Delinquency,
case when isnull(DATEDIFF(dd,A.NextDueDate,A.PayBackDate),500)>45 then 1 else 0 end as Bad
  FROM [WongaWholeStagingv2].[redline].[Agreements] A
  inner join WongaWholeStagingv2.wongalook.Result R on R.ProposalID=A.ProposalID
  inner join WongaWholeStagingv2.greyface.application ap on ap.agreement_reference = A.AgreementReference
  inner join WongaWholeStagingv2.greyface.EmploymentDetails em on em.EmploymentDetailId = ap.EmploymentDetailId
  --inner join WongaWholeStagingv2.redline.Transactions t on t.AgreementReference = A.AgreementReference
  --inner join WongaLook.data.Application AP on AP.ProposalID=A.ProposalID
  inner join WongaWholeStagingv2.redline.Proposals P on P.ProposalID = A.ProposalID
  left join WongaWholeStagingv2.dbo.PrevProposalData pp on R.ProposalID = pp.ProposalId
  where  A.CustomerID in (select CustomerID from #cids) and R.LoanNumber < 31 order by A.CustomerID, R.LoanNumber;

-------End of SQL------------------

trv<-read.csv("trvelocity.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);#30 loans for the customers
colnames(trv)[1]<-"ProposalID";
library(lattice);
densityplot(~TrustRating|as.factor(LoanNumber),data = trv);
histogram(~TrustRating|as.factor(LoanNumber),data = trv);
xyplot(TrustRating~LoanNumber|CustomerID,data=trv[1:300,],type="o");
xyplot(TrustRating~LoanNumber|as.character(CustomerID),data=trv[1:650,],type="o");
xyplot(TrustRating~LoanNumber|as.character(CustomerID),data=trv[10981:11441,],type="o");

ln<-seq(0:30);

meanval<-function(x,df)
{
	len<-length(x);
	vals<-c();
	for(i in 1:len)
	{
		vals<-c(vals,mean(df$TrustRating[which(df$LoanNumber==(i-1))]));
	}
	return(data.frame(x,vals))
}
meantr<-meanval(ln,trv);
qplot(x,vals,data=meantr,geom=c("path","point"),xlab="LoanNumber",ylab="TrustRating",main="Mean of Trust Rating vs LoanNumber");

medianval<-function(x,df)
{
	len<-length(x);
	vals<-c();
	for(i in 1:len)
	{
		vals<-c(vals,median(df$TrustRating[which(df$LoanNumber==(i-1))]));
	}
	return(data.frame(x,vals))
}
mediantr<-medianval(ln,trv);
qplot(x,vals,data=mediantr,geom=c("path","point"),xlab="LoanNumber",ylab="TrustRating",main="Median of Trust Rating vs LoanNumber");

trv$utilization<-trv$LoanAmountOriginal/trv$TrustRating;
xyplot(utilization~LoanNumber|as.character(CustomerID),data=trv[10981:11441,],type="o");
xyplot(TrustRating~LoanNumber|as.character(CustomerID),data=trv[10981:11441,],type="o",col="red");


---------------------------------------SQL--------------------------
SELECT A.ProposalID,
A.CustomerID,
convert(char(10),A.ApplicationDate,112) as ApplicationDate,
convert(char(10),A.PayBackDate,112) as PaybackDate,
convert(char(10),A.NextDueDate,112) as NextDueDate,
R.LoanNumber,
pp.DaysSinceLastProposal,
TotalGoodsCashPrice as LoanAmountAgreed,
P.OriginalLoanAmount as LoanAmountOriginal,
a.Term as LoanTerm,
TrustRating,
R.MixedScore,
em.AnnualIncome,
A.AgreementReference,
isnull(DATEDIFF(dd,A.NextDueDate,A.PayBackDate), 500) as Delinquency,
case when isnull(DATEDIFF(dd,A.NextDueDate,A.PayBackDate),500)>45 then 1 else 0 end as Bad into #temp
  FROM [WongaWholeStagingv2].[redline].[Agreements] A
  inner join WongaWholeStagingv2.wongalook.Result R on R.ProposalID=A.ProposalID
  inner join WongaWholeStagingv2.greyface.application ap on ap.agreement_reference = A.AgreementReference
  inner join WongaWholeStagingv2.greyface.EmploymentDetails em on em.EmploymentDetailId = ap.EmploymentDetailId
  --inner join WongaWholeStagingv2.redline.Transactions t on t.AgreementReference = A.AgreementReference
  --inner join WongaLook.data.Application AP on AP.ProposalID=A.ProposalID
  inner join WongaWholeStagingv2.redline.Proposals P on P.ProposalID = A.ProposalID
  left join WongaWholeStagingv2.dbo.PrevProposalData pp on R.ProposalID = pp.ProposalId
  where  A.ApplicationDate BETWEEN '2011-01-01' AND '2011-07-01' order by A.ApplicationDate;

select b.LoanNumber, a.TrustRating as TR1,b.TrustRating as TR2,b.ProposalID, b.CustomerID  from #temp a, #temp b where a.TrustRating < 1000 and b.TrustRating = 1000 and b.LoanNumber - a.LoanNumber = 1 and a.CustomerID = b.CustomerID
---------------------------------------End of SQL-------------------

last2loan<-read.csv("last2loanto10001st6month2011.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
colnames(last2loan)[1]<-"LoanNumber";
last2loan$gap2max<-last2loan$TR2-last2loan$TR1;
mean(last2loan$LoanNumber);
sd(last2loan$LoanNumber);
table(last2loan$LoanNumber);
max(last2loan$LoanNumber);
min(last2loan$LoanNumber);
ggplot(last2loan,aes(LoanNumber,gap2max,colour=Bad))+geom_point();
ggplot(last2loan,aes(LoanNumber,gap2max,colour=as.factor(Bad)))+geom_point(position="jitter");

mycust<-unique(trv$CustomerID[which(trv$TrustRating==1000)]);
mycustdata<-trv$CustomerID %in% mycust;
xyplot(utilization~LoanNumber|as.character(CustomerID),data=trv[mycustdata,],type="o");
xyplot(TrustRating~LoanNumber|as.character(CustomerID),data=trv[mycustdata,],type="o");

----------------------------------------SQL---------------------------------------------------
SELECT R.LoanNumber,
AVG(a.Term) as Term,
AVG(TrustRating) as TrustRating
  FROM [WongaWholeStagingv2].[redline].[Agreements] A
  inner join WongaWholeStagingv2.wongalook.Result R on R.ProposalID=A.ProposalID
  inner join WongaWholeStagingv2.redline.Proposals P on P.ProposalID = A.ProposalID
  left join WongaWholeStagingv2.dbo.PrevProposalData pp on R.ProposalID = pp.ProposalId
  where  A.ApplicationDate BETWEEN '2011-01-01' AND '2011-07-01' group by R.LoanNumber; 
  
  --------------------------------------End of SQL---------------------------------------------
  trlnwhole<-read.csv("trvsln1st6m2011.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);#trust rating update with loan number
  colnames(trlnwhole)[1]<-"LoanNumber";
qplot(LoanNumber,TrustRating,data=trlnwhole,geom=c("path","point"),xlab="LoanNumber",ylab="TrustRating",main="Mean of Trust Rating vs LoanNumber for All Loans in 01/01 - 01/07 2011");
qplot(LoanNumber,Counter,data=trlnwhole,geom=c("path","point"),xlab="LoanNumber",ylab="The number of Application",main="# of Loan Applied on each LoanNumber for All Loans in 01/01 - 01/07 2011");
qplot(LoanNumber,log(Counter,10),data=trlnwhole,geom=c("path","point"),xlab="LoanNumber",ylab="Log(The number of Application)",main="# of Loan Applied on each LoanNumber for All Loans in 01/01 - 01/07 2011");

trquick<-read.csv("trustratingquick1000.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);#trust rating for customer reach 1000.
customermisstr<-unique(trquick$CustomerID[which(is.na(trquick$TrustRating))]);
colnames(trquick)[1]<-"ProposalID";
mycust<-trquick$CustomerID %in% customermisstr;
customermisstr2<-unique(trquick$CustomerID[which((trquick$TrustRating[trquick$LoanNumber==0]<300))]);
mycust2<-trquick$CustomerID %in% customermisstr2;

trquick2<-trquick[!mycust,];
trquick2<-trquick2[!mycust2,];
xyplot(TrustRating~LoanNumber|as.factor(CustomerID),data=trquick2[307:724,],type="o");

ggplot(trv,aes(as.factor(LoanNumber),TrustRating))+geom_boxplot();


nxloander<-read.csv("nxloander.csv",header=TRUE,sep=",",na.strings="NULL",stringsAsFactors=FALSE);
nxloander<-read.csv("nxloander.csv",header=TRUE,sep=",",na.strings="NA",stringsAsFactors=FALSE);
trainind<-sample(1:209039,146327,replace=FALSE);
train<-nxloander[trainind,];
test<-nxloander[-trainind,];

nlpre.logr<-glm(ThirdBad~FirstLoanNumber+as.numeric(FirstDaysSinceLastProposal)+FirstLoanAmountAgreed+FirstLoanAmountOriginal+FirstLoanTerm+FirstTrustRating+FirstMixedScore+FirstAnnualIncome+FirstDelinquency+FirstTotalExt+FirstTotalTop+SecondLoanNumber+SecondDaysSinceLastProposal+SecondLoanAmountAgreed+SecondLoanAmountOriginal+SecondLoanTerm+SecondTrustRating+SecondMixedScore+SecondAnnualIncome+SecondDelinquency+SecondTotalExt+SecondTotalTop+AmountOrigDiff+AmountAgreeDiff+ScoreDiff+utilization1+utilization2+utilRatio+utilDiff+IncomeDiff+trratio+utitrratio+logutitrratio+SecondDaysSinceLastPayOff+FirstLoanIncome+SecondLoanIncome,family=binomial("logit"))


nn<-nnet(ThirdBad~FirstLoanNumber+FirstLoanAmountAgreed+FirstLoanAmountOriginal+FirstLoanTerm+FirstTrustRating+FirstMixedScore+FirstAnnualIncome+FirstDelinquency+SecondDaysSinceLastProposal+SecondLoanTerm+SecondTrustRating+SecondMixedScore+SecondAnnualIncome+SecondDelinquency+SecondTotalExt+SecondTotalTop+ScoreDiff+utilization1+utilization2+utilRatio+utilDiff+trratio+utitrratio+logutitrratio+SecondDaysSinceLastPayOff+FirstLoanIncome+SecondLoanIncome,data=train,size=20,linout=FALSE,decay=0.025,maxit=5000);#auc 0.7943
nn.pred<-predict(nn,test);
pred<-prediction(nn.pred,test$ThirdBad);
auc<-performance(pred,"auc");
auc;

nn<-nnet(ThirdBad~FirstLoanNumber+FirstLoanAmountAgreed+FirstLoanAmountOriginal+FirstLoanTerm+FirstTrustRating+FirstMixedScore+FirstAnnualIncome+FirstDelinquency+SecondDaysSinceLastProposal+SecondLoanTerm+SecondTrustRating+SecondMixedScore+SecondAnnualIncome+SecondDelinquency+SecondTotalExt+SecondTotalTop+ScoreDiff+utilization1+utilization2+utilRatio+utilDiff+trratio+utitrratio+logutitrratio+SecondDaysSinceLastPayOff+FirstLoanIncome+SecondLoanIncome,data=train,size=20,linout=FALSE,decay=0.025,maxit=5000);
  
set.seed(303)  
nn<-nnet(ThirdBad~FirstLoanNumber+FirstLoanAmountAgreed+FirstLoanAmountOriginal+FirstLoanTerm+FirstTrustRating+FirstMixedScore+FirstAnnualIncome+FirstDelinquency+SecondDaysSinceLastProposal+SecondLoanTerm+SecondTrustRating+SecondMixedScore+SecondAnnualIncome+SecondDelinquency+SecondTotalExt+SecondTotalTop+ScoreDiff+utilization1+utilization2+utilRatio+utilDiff+trratio+utitrratio+logutitrratio+SecondDaysSinceLastPayOff+FirstLoanIncome+SecondLoanIncome,data=train,size=20,linout=FALSE,decay=0.025,maxit=5000) #0.7921

set.seed(303)  
nn<-nnet(ThirdBad~FirstLoanNumber+FirstLoanAmountAgreed+FirstLoanAmountOriginal+FirstLoanTerm+FirstTrustRating+FirstMixedScore+FirstAnnualIncome+FirstDelinquency+SecondDaysSinceLastProposal+SecondLoanTerm+SecondTrustRating+SecondMixedScore+SecondAnnualIncome+SecondDelinquency+SecondTotalExt+SecondTotalTop+ScoreDiff+utilization1+utilization2+utilRatio+trratio+logutitrratio+SecondDaysSinceLastPayOff+FirstLoanIncome+SecondLoanIncome,data=train,size=20,linout=FALSE,decay=0.025,maxit=5000) #0.7959

set.seed(303)  #preferred
nn<-nnet(ThirdBad~FirstLoanNumber+FirstMixedScore+FirstDelinquency+FirstTotalTop+SecondLoanNumber+SecondDaysSinceLastProposal+SecondLoanTerm+SecondMixedScore+SecondDelinquency+SecondTotalExt+SecondTotalTop+utilization2+SecondDaysSinceLastPayOff+SecondLoanIncome,data=train,size=20,linout=FALSE,decay=0.025,maxit=5000) 
###############################################################
# 0.7949 - Evaluator:    weka.attributeSelection.CfsSubsetEval#  19318 training
#Search:weka.attributeSelection.BestFirst -D 1 -N 5 14features# 
###############################################################


set.seed(303)  
nn<-nnet(ThirdBad~FirstLoanNumber+FirstDaysSinceLastProposal+FirstLoanAmountOriginal+FirstLoanTerm+FirstTrustRating+FirstMixedScore+FirstAnnualIncome+FirstDelinquency+FirstTotalExt+FirstTotalTop+SecondLoanNumber+SecondDaysSinceLastProposal+SecondLoanAmountOriginal+SecondLoanTerm+SecondTrustRating+SecondMixedScore+SecondAnnualIncome+SecondDelinquency+SecondTotalTop+AmountOrigDiff+AmountAgreeDiff+ScoreDiff+utilization1+utilDiff+utitrratio+SecondDaysSinceLastPayOff+FirstLoanIncome+SecondLoanIncome,data=train,size=20,linout=FALSE,decay=0.025,maxit=5000) 
#############################################################################################
# 0.7949 - Evaluator:    ClassifierSubsetEval -B weka.classifier.tree.J48 -T -H -C 0.25 -M 2# 
# GreedyStepwise -T -1.797693 -N -1       ROC 0.7933                                        # 
#############################################################################################