#results
TC_Analysis=function(alg,c1,c2){
  #alg=c("MLP","DBN","SAE","RNN","LSTM","GRU","CART","NB","RF","LR","SVM","XGB")
  #c1=c(0,0.01,0.02,0.03)
  #c2=c(0,0.001,0.002,0.003,0.004)
  WR2=NULL
  ARR2=NULL
  ASR2=NULL
  MDD2=NULL
  for (i in alg){
    WR1=NULL
    ARR1=NULL
    ASR1=NULL
    MDD1=NULL
    for (j in c1){
      WR=NULL
      ARR=NULL
      ASR=NULL
      MDD=NULL
      for (k in c2) {
        a= read.csv(paste(".218NewHS300_Performance_",i,"_Cost_",j,"_",k,".csv",sep = ""),header = T)     
        WR0=mean(a$WinTrade)
        ARR0=mean(a$AnnRetTrade)
        ASR0=mean(a$AnnSharpeTrade)
        MDD0=mean(a$MaxDDTrade)
        WR=c(WR,WR0)
        ARR=c(ARR,ARR0)
        ASR=c(ASR,ASR0)
        MDD=c(MDD,MDD0)
      }
      WR1=c(WR1,WR)
      ARR1=c(ARR1,ARR)
      ASR1=c(ASR1,ASR)
      MDD1=c(MDD1,MDD)
    }
    WR2=round(cbind(WR2,WR1),4)
    ARR2=round(cbind(ARR2,ARR1),4)
    ASR2=round(cbind(ASR2,ASR1),4)
    MDD2=round(cbind(MDD2,MDD1),4)
  }
  colnames(WR2)=alg
  colnames(ARR2)=alg
  colnames(ASR2)=alg
  colnames(MDD2)=alg
  write.csv(as.data.frame(WR2),file = ".218WR_Cost.csv")
  write.csv(as.data.frame(ARR2),file = ".218ARR_Cost.csv")
  write.csv(as.data.frame(ASR2),file = ".218ASR_Cost.csv")
  write.csv(as.data.frame(MDD2),file = ".218MDD_Cost.csv")
}

alg=c("MLP","DBN","SAE","RNN","LSTM","GRU","CART","NB","RF","LR","SVM","XGBoost")
c1=c(0,0.01, 0.02,0.03,0.04)
c2=c(0,0.001,0.002,0.003,0.004,0.005)
TC_Analysis(alg=alg,c1=c1,c2=c2)
