#######ARCENE
library(deepnet)

setwd("/home/timedreamer/Dropbox/Course/STA5635/BInaryClassificationDatasets/arcene/")


arcene_tr <- read.delim("arcene_train.data",sep=" ",header=F)

tr_result <- read.delim("arcene_train.labels",header=F)
colnames(tr_result) <- "result"
tr_result$result[tr_result$result == -1] <- 0

arcene_ts <- read.delim("arcene_valid.data",sep=" ",header=F)
ts_result <- read.delim("arcene_valid.labels",sep=" ",header=F)
colnames(ts_result) <- "result"
ts_result$result[ts_result$result == -1] <- 0


arcene_tr[,10001] <- NULL
arcene_ts[,10001] <- NULL
colnames(arcene_tr)[1:10000] <- paste("var",c(1:10000),sep="") 
colnames(arcene_tr)[1:5]

colnames(arcene_ts)[1:10000] <- paste("var",c(1:10000),sep="") 
colnames(arcene_ts)[1:5]

tr <- numeric()
ts <- numeric()

for (i in c(128,256,512,1024)){
    nn_arcene <- nn.train(x=as.matrix(arcene_tr),y=tr_result$result,hidden = c(128,i))
    err.dnn <- nn.test(nn_arcene,x=as.matrix(arcene_tr),y=tr_result$result)
    tr <- append(tr,err.dnn)
    
    err.dnn <- nn.test(nn_arcene,x=as.matrix(arcene_ts),y=ts_result$result)
    ts <- append(ts,err.dnn)
    
}

tr
ts

###DEXTER
setwd("/home/timedreamer/Dropbox/Course/STA5635/BInaryClassificationDatasets/dexter/")

load("word_matrix.RData")

result_tr <- read.delim("dexter_train.labels",header=F)
colnames(result_tr) <- "result"
result_tr$result[result_tr$result==-1] <- 0
result_ts <- read.delim("dexter_valid.labels",header=F)
colnames(result_ts) <- "result"
result_ts$result[result_ts$result==-1] <- 0

tr <- numeric()
ts <- numeric()

for (i in c(128,256,512,1024)){
    nn_arcene <- nn.train(x=as.matrix(word_matrix_tr),y=result_tr$result,hidden = c(128,i))
    err.dnn <- nn.test(nn_arcene,x=as.matrix(word_matrix_tr),y=result_tr$result)
    tr <- append(tr,err.dnn)
    
    err.dnn <- nn.test(nn_arcene,x=as.matrix(word_matrix_ts),y=result_ts$result)
    ts <- append(ts,err.dnn)
}

tr
ts


####MADELON
#Madelon
setwd("/home/timedreamer/Dropbox/Course/STA5635/BInaryClassificationDatasets/MADELON/")

madelon_tr <- read.delim("madelon_train.data",sep=" ",header=F)
madelon_tr[501]<- NULL
result_tr <- read.delim("madelon_train.labels",header=F)
result_tr$V1[result_tr$V1==-1] <- 0

madelon_ts <- read.delim("madelon_valid.data",sep=" ",header=F)
madelon_ts[501]<- NULL
result_ts <- read.delim("madelon_valid.labels",header=F)
result_ts$V1[result_ts$V1==-1] <- 0

tr <- numeric();ts <- numeric()

for (i in c(128,256,512,1024)){
    nn_arcene <- nn.train(x=as.matrix(madelon_tr),y=result_tr$V1,hidden = c(128,i))
    err.dnn <- nn.test(nn_arcene,x=as.matrix(madelon_tr),y=result_tr$V1)
    tr <- append(tr,err.dnn)
    
    err.dnn <- nn.test(nn_arcene,x=as.matrix(madelon_ts),y=result_ts$V1)
    ts <- append(ts,err.dnn)
}

tr
ts


#######Gissette

#Gissette
setwd("/home/timedreamer/Dropbox/Course/STA5635/BInaryClassificationDatasets/Gisette/")

gisette_tr <- read.delim("gisette_train.data",sep=" ",header=F)
gisette_tr$V5001 <- NULL
tr_result <- read.delim("gisette_train.labels",header=F)
colnames(tr_result) <- "result"
tr_result$result[tr_result$result == -1] <- 0

gisette_vl <- read.delim("gisette_valid.data",sep=" ",header=F)
gisette_vl$V5001 <- NULL
vl_result <- read.delim("gisette_valid.labels",header=F)
colnames(vl_result) <- "result"
vl_result$result[vl_result$result == -1] <- 0


tr <- numeric()
ts <- numeric()

for (i in c(128,256,512,1024)){
    nn_arcene <- nn.train(x=as.matrix(gisette_tr),y=tr_result$result,hidden = c(i))
    err.dnn <- nn.test(nn_arcene,x=as.matrix(gisette_tr),y=tr_result$result)
    tr <- append(tr,err.dnn)
    
    err.dnn <- nn.test(nn_arcene,x=as.matrix(gisette_vl),y=vl_result$result)
    ts <- append(ts,err.dnn)
    
}

tr
ts



