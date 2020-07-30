#compute ACC,RCC,PCC, AUC and F1.
eva_func=function(sec_names){
  idNumber=read.csv(sec_names,header=F)$V1
  Stock_Name=read.csv(sec_names,header=F)$V2
  idNumber=substr(as.character(idNumber),start = 1,stop = 6)
  Label_File=paste(idNumber,"_DatasetNew",".csv",sep = "")
  SignalFile=paste(idNumber,"_TradeSignal_GRU",".csv",sep = "")
  N=length(SignalFile)
  Accuracy=NULL
  Precision=NULL
  Recall=NULL
  F1=NULL
  AUC=NULL
  for(i in 1:N){
    TradeSignal0=read.csv(SignalFile[i],header = T)
    Label=read.csv(Label_File[i],header = T)$Label[-c(1:250)]
    Signal_Label=TradeSignal0[,2][-c(1:250)]
    result=table(Signal_Label,Label)
    pred=ROCR::prediction(Signal_Label,Label)
    perf=ROCR::performance(pred,measure = "auc")
    AUC0=(perf@y.values)[[1]]
    if(dim(result)[1]==2){
      Accuracy0=sum(diag(result))/sum(result)
      Precision0=result[2,2]/sum(result[,2])
      Recall0=result[2,2]/sum(result[2,])
      F10=2*Precision0*Recall0/(Precision0+Recall0)
    }else{
      Accuracy0=1
      Precision0=1
      Recall0=1
      F10=2*Precision0*Recall0/(Precision0+Recall0)
    }
    Accuracy=c(Accuracy,Accuracy0)
    Precision=c(Precision,Precision0)
    Recall=c(Recall,Recall0)
    F1=c(F1,F10)
    AUC=c(AUC,AUC0)
  }
  performance=cbind(Accuracy,Precision,Recall,F1,AUC)
  rownames(performance)=idNumber
  colnames(performance)=c("Accuracy","Precision","Recall","F1","AUC")
  write.csv(as.data.frame(performance),file=".NewHS300_Performance_ML_GRU.csv")
}
eva_func(".Newhs300.csv")