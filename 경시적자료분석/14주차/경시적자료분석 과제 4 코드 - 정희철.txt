setwd("~/Google Drive/My Drive/skku class files/2022년도 2학기/경시적자료분석/13주차")
library (gee)

ctq = read.table("ctqII.data",col.names=c("ID","TP","Trt","BinVar"), header = FALSE)
ctq = ctq[2:length(ctq$ID),]
ctq <- data.frame(ID = ctq$ID, TP = ctq$TP, Trt = ctq$Trt, BinVar = ctq$BinVar)
ctq$TP = as.integer(ctq$TP)
ctq$Trt = as.integer(ctq$Trt)
ctq$BinVar = as.integer(ctq$BinVar)


ctq_mod <- gee(BinVar ~ TP + Trt,
           cor="exchangeable",
           id=ID,data=ctq,family="binomial")
summary(ctq_mod)
