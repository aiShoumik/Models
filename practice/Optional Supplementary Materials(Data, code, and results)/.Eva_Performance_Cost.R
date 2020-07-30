# consider trading cost and slipage
backtest=function(x){
    winpct=length(x[x>0])/length(x[x!=0])
    annRet=PerformanceAnalytics::Return.annualized(x,scale = 250,geometric = F)
    sharpe=PerformanceAnalytics::SharpeRatio.annualized(x,scale = 250,geometric = F)
    maxDD=PerformanceAnalytics::maxDrawdown(x)
    perfo=c(winpct,annRet,sharpe,maxDD)
    return(perfo)
}
#-------------------------Performance Function---------------------
eva_func=function(sec_names,alg,c1,c2){
  idNumber=read.csv(sec_names,header=F)$V1
  idNumber=substr(as.character(idNumber),start = 1,stop = 6)
  SignalFile=paste(idNumber,"_TradeSignal_",alg,".csv",sep = "")
  Rawfile=paste(idNumber,"_New",".csv",sep = "")
  N=length(SignalFile)
  #c1=0.02
  #c2=0.004
  WinTrade=NULL
  AnnRetTrade=NULL
  AnnSharpeTrade=NULL
  MaxDDTrade=NULL
  for(i in 1:N){
    TradeSignal0=read.csv(SignalFile[i],header = T)
    TradeSignal=xts::xts(TradeSignal0[,2],order.by = as.Date(TradeSignal0[,1]))
    TradeSignal_1=lag(TradeSignal,1)
    TradeSignal_1[1]=0
    TradeSignal_2=lag(TradeSignal,2)
    TradeSignal_3=lag(TradeSignal,3)
    TradeSignal_3[1]=0
    TradeSignal_3[2]=0
    TradeSignal_3[3]=0
    TradeSignal_2[1]=0
    TradeSignal_2[2]=0
    RawPrice=read.csv(Rawfile[i],header = T)[,c("X","Close")]
    RawPrice=(xts::xts(RawPrice[,-1],order.by = as.Date(RawPrice[,1])))
    Price1=RawPrice-c1*abs(TradeSignal_1-TradeSignal_2)-RawPrice*c2*abs(TradeSignal_1-TradeSignal_2)
    Price1_1=lag(RawPrice)
    Price1_1[1]=Price1_1[2]
    Price2=Price1_1+c1*abs(TradeSignal_2-TradeSignal_3)+Price1_1*c2*abs(TradeSignal_2-TradeSignal_3)
    Ret_Cost=((Price1-Price2)/Price2)[zoo::index(TradeSignal)]
    ReturnTrade=TradeSignal_1*Ret_Cost
    EvalIndi=backtest(ReturnTrade)
    WinTrade0=EvalIndi[1]
    AnnRetTrade0=EvalIndi[2]
    AnnSharpeTrade0=EvalIndi[3]
    MaxDDTrade0=EvalIndi[4]
    WinTrade=c(WinTrade,WinTrade0)
    AnnRetTrade=c(AnnRetTrade, AnnRetTrade0)
    AnnSharpeTrade=c(AnnSharpeTrade,AnnSharpeTrade0)
    MaxDDTrade=c(MaxDDTrade,MaxDDTrade0)
  }
  performance=cbind(WinTrade,AnnRetTrade,AnnSharpeTrade,MaxDDTrade)
  rownames(performance)=idNumber
  colnames(performance)=c("WinTrade","AnnRetTrade","AnnSharpeTrade","MaxDDTrade")
  write.csv(as.data.frame(performance),file=paste(".218NewHS300_Performance_",alg,"_Cost_",c1,"_",c2,".csv",sep = ""))
}
eva_func(".NewSP500.csv")
#-----------------------
alg1=c("MLP","DBN","SAE","RNN","LSTM","GRU","LR","SVM","CART","XGB","NB","RF")
alg=c("NB","RF")
c11=c(0,0.01,0.02,0.03,0.04)
c1=c(0,0.01,0.02,0.03,0.04)
c22=c(0,0.001,0.002,0.003,0.004,0.005)
c2=c(0,0.001,0.002,0.003,0.004,0.005)
for (i in alg) {
  for (j in c1) {
    for (k in c2) {
      eva_func(".NewHS300.csv",alg = i,c1=j,c2=k)
    }
  }
}
#----------------significant test-----------------------------
a=read.csv(".218ARR_Cost.csv")[,-1]
round((a[1,]-a[30,])/a[1,],4)
round((a[1,]-a[16,])/a[1,],4)
mean(a$SAE[2:6])
mean(a$SAE[c(7,13,19,25)])
#----------------significant test----------
c1=c(0,0.01,0.02,0.03,0.04)
c2=c(0,0.001,0.002,0.003,0.004,0.005)
name=NULL
  for (j in c1) {
    name1=NULL
    for (k in c2) {
      name0=paste(j,"_",k,sep = "")
      name1=c(name1,name0)
    }
    name=c(name,name1)
  }
alg="NB"
c1=c(0,0.01,0.02,0.03,0.04)
c2=c(0,0.001,0.002,0.003,0.004,0.005)
wr=NULL
for (i in alg) {
  wr1=NULL
  for (j in c1) {
    wr2=NULL
    for (k in c2) {
      wr0=read.csv(paste(".218NewSP500_Performance_",alg,"_Cost_",j,"_",k,".csv",sep = ""),header = T)[,5]
      wr2=cbind(wr2,wr0)
    }
    wr1=cbind(wr1,wr2)
  }
  wr=cbind(wr,wr1)
} 
colnames(wr)=name
round(apply(wr,MARGIN = 2,mean),4)
#mwr=reshape2::melt(wr[,c(1,2,3,7,8,9,13)])
mwr=reshape2::melt(wr)
#bartlett.test(mwr$value~mwr$Var2,data =  mwr)
#pgirmess::kruskalmc(mwr$value~mwr$Var2,data =  mwr)
a=DescTools::NemenyiTest(mwr$value~mwr$Var2,data =  mwr)
#which(a[[1]][,2]>0.05)
a[[1]][which(a[[1]][,2]>0.05),][1:30,]
#write.csv(a[[1]][which(a[[1]][,2]>0.05),],file=".MLP_MDD_com.csv")
