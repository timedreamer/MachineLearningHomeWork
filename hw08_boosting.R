#######################3
#HIll VAlley. load Data

load("~/Dropbox/Course/STA5635/BInaryClassificationDatasets/hill-valley/hill_rankAvg.RData")

tr <- numeric()
ts <- numeric()
library(caTools)

for (k in c(10,30,100,300)) {
    fit <- LogitBoost(xlearn = rank_avg_tr,ylearn = result_tr$result,nIter = k)
    fit.val <- predict(fit,xtest = rank_avg_tr,type = "class")
    cm <- table(actual = result_tr$result,fitted=fit.val)
    mce <- 1 - (sum(diag(cm))/sum(cm))
    tr <- append(tr,mce)
    
    fit.val <- predict(fit,xtest = rank_avg_ts,type = "class")
    cm <- table(actual = result_ts$result,fitted=fit.val)
    mce <- 1 - (sum(diag(cm))/sum(cm))
    ts <- append(ts,mce)
    
}

tr
ts

tr_tree <- numeric()
ts_tree <- numeric()

rank_avg_tr <- cbind(rank_avg_tr,result_tr$result)
rank_avg_tr <- data.frame(rank_avg_tr)

rank_avg_ts <- cbind(rank_avg_ts,result_ts$result)
rank_avg_ts <- data.frame(rank_avg_ts)

library(deepboost)

for (k in c(10,30,100,300)) {
    
    fit <- deepboost(rank_avg_tr[,11] ~ .,data=rank_avg_tr[,-11],tree_depth = 100,num_iter = k,verbose = F )
    fit.val <- predict(fit,newdata=rank_avg_tr[,-11])
    cm <- table(actual = result_tr$result,fitted=fit.val)
    mce <- 1 - (sum(diag(cm))/sum(cm))
    tr_tree <- append(tr_tree,mce)
    

    fit.val <- predict(fit,newdata=rank_avg_ts[,-11])
    cm <- table(actual = result_ts$result,fitted=fit.val)
    mce <- 1 - (sum(diag(cm))/sum(cm))
    ts_tree <- append(ts_tree,mce)
    
}

tr_tree;ts_tree

plot(tr,type="b",col="blue",xaxt="n",ylab="miscalculationRate",xlab="number of iterations",ylim=c(0.006,0.240))
axis(1, at=1:4, labels=c(10,30,100,300))
lines(ts,col="red",type="b")
lines(tr_tree,col="black",type="b")
lines(ts_tree,col="green",type="b")
title(main="Hill_Valley")
legend(0.12,c("train","test","train_tree","test_tree"),lty=c(1,1),col=c("blue","red","black","green"))

################
#Madelon
setwd("/home/timedreamer/Dropbox/Course/STA5635/BInaryClassificationDatasets/MADELON/")

madelon_tr <- read.delim("madelon_train.data",sep=" ",header=F)
madelon_tr[501]<- NULL
result_tr <- read.delim("madelon_train.labels",header=F)


madelon_ts <- read.delim("madelon_valid.data",sep=" ",header=F)
madelon_ts[501]<- NULL
result_ts <- read.delim("madelon_valid.labels",header=F)

tr <- numeric();ts <- numeric()

for (k in c(10,30,100,300)) {
    fit <- LogitBoost(xlearn = madelon_tr,ylearn = result_tr$V1,nIter = k)
    fit.val <- predict(fit,xtest = madelon_tr,type = "class")
    cm <- table(actual = result_tr$V1,fitted=fit.val)
    mce <- 1 - (sum(diag(cm))/sum(cm))
    tr <- append(tr,mce)
    
    fit.val <- predict(fit,xtest = madelon_ts,type = "class")
    cm <- table(actual = result_ts$V1,fitted=fit.val)
    mce <- 1 - (sum(diag(cm))/sum(cm))
    ts <- append(ts,mce)
}

plot(tr,type="b",col="blue",xaxt="n",ylab="miscalculationRate",xlab="number of iterations",ylim=c(0.275,0.493))
axis(1, at=1:4, labels=c(10,30,100,300))
lines(ts,col="red",type="b")
title(main="Madelon")
legend("right",c("train","test"),lty=c(1,1),col=c("blue","red"))

#########
#Dexter

setwd("/home/timedreamer/Dropbox/Course/STA5635/BInaryClassificationDatasets/dexter")
load("word_matrix.RData")

result_tr <- read.delim("dexter_train.labels",header=F)
colnames(result_tr) <- "result"
result_ts <- read.delim("dexter_valid.labels",header=F)
colnames(result_ts) <- "result"

tr <- numeric()
ts <- numeric()

for (k in c(10,30,100,300)) {
    fit <- LogitBoost(xlearn = word_matrix_tr,ylearn = result_tr$result,nIter = k)
    fit.val <- predict(fit,xtest = word_matrix_tr,type = "class")
    cm <- table(actual = result_tr$result,fitted=fit.val)
    mce <- 1 - (sum(diag(cm))/sum(cm))
    tr <- append(tr,mce)
    
    fit.val <- predict(fit,xtest = word_matrix_ts,type = "class")
    cm <- table(actual = result_ts$result,fitted=fit.val)
    mce <- 1 - (sum(diag(cm))/sum(cm))
    ts <- append(ts,mce)
    
}



plot(tr,type="b",col="blue",xaxt="n",ylab="miscalculationRate",xlab="number of iterations",ylim=c(0,0.125))
axis(1, at=1:4, labels=c(10,30,100,300))
lines(ts,col="red",type="b")
title(main="Dexter")
legend("right",c("train","test"),lty=c(1,1),col=c("blue","red"))



#################
#Gissette
setwd("/home/timedreamer/Dropbox/Course/STA5635/BInaryClassificationDatasets/Gisette/")

gisette_tr <- read.delim("gisette_train.data",sep=" ",header=F)
gisette_tr$V5001 <- NULL
tr_result <- read.delim("gisette_train.labels",header=F)
colnames(tr_result) <- "result"

gisette_vl <- read.delim("gisette_valid.data",sep=" ",header=F)
gisette_vl$V5001 <- NULL
vl_result <- read.delim("gisette_valid.labels",header=F)
colnames(vl_result) <- "result"


tr <- numeric()
ts <- numeric()

for (k in c(10,30,100,300)) {
    fit <- LogitBoost(xlearn = gisette_tr,ylearn = tr_result$result,nIter = k)
    fit.val <- predict(fit,xtest = gisette_tr,type = "class")
    cm <- table(actual = tr_result$result,fitted=fit.val)
    mce <- 1 - (sum(diag(cm))/sum(cm))
    tr <- append(tr,mce)
    
    fit.val <- predict(fit,xtest = gisette_vl,type = "class")
    cm <- table(actual = vl_result$result,fitted=fit.val)
    mce <- 1 - (sum(diag(cm))/sum(cm))
    ts <- append(ts,mce)
}

plot(tr,type="b",col="blue",xaxt="n",ylab="miscalculationRate",xlab="number of iterations",ylim=c(0,0.058))
axis(1, at=1:4, labels=c(10,30,100,300))
lines(ts,col="red",type="b")
title(main="Gisette")
legend("right",c("train","test"),lty=c(1,1),col=c("blue","red"))


##########ARCENE

setwd("/home/timedreamer/Dropbox/Course/STA5635/BInaryClassificationDatasets/arcene/")

arcene_tr <- read.delim("arcene_train.data",sep=" ",header=F)
tr_result <- read.delim("arcene_train.labels",header=F)
colnames(tr_result) <- "result"

arcene_ts <- read.delim("arcene_valid.data",sep=" ",header=F)
ts_result <- read.delim("arcene_valid.labels",sep=" ",header=F)
colnames(ts_result) <- "result"

arcene_tr[,10001] <- NULL
arcene_ts[,10001] <- NULL
colnames(arcene_tr)[1:10000] <- paste("var",c(1:10000),sep="") 
colnames(arcene_tr)[1:5]

colnames(arcene_ts)[1:10000] <- paste("var",c(1:10000),sep="") 
colnames(arcene_ts)[1:5]


tr <- numeric()
ts <- numeric()

for (k in c(3,10,30,100,300)) {
    fit <- LogitBoost(xlearn = arcene_tr,ylearn = tr_result$result,nIter = k)
    fit.val <- predict(fit,xtest = arcene_tr,type = "class")
    cm <- table(actual = tr_result$result,fitted=fit.val)
    mce <- 1 - (sum(diag(cm))/sum(cm))
    tr <- append(tr,mce)
    
    fit.val <- predict(fit,xtest = arcene_ts,type = "class")
    cm <- table(actual = ts_result$result,fitted=fit.val)
    mce <- 1 - (sum(diag(cm))/sum(cm))
    ts <- append(ts,mce)
}

tr;ts

plot(tr,type="b",col="blue",xaxt="n",ylab="miscalculationRate",xlab="number of iterations",ylim=c(0,0.44))
axis(1, at=1:5, labels=c(3,10,30,100,300))
lines(ts,col="red",type="b")
title(main="Arcene")
legend("right",c("train","test"),lty=c(1,1),col=c("blue","red"))
