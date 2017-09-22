################Gissete################333
getwd()
setwd("/home/timedreamer/Dropbox/Course/STA5635/BInaryClassificationDatasets/Gisette/")

gisette_tr <- read.delim("gisette_train.data",sep=" ",header=F)
gisette_tr$V5001 <- NULL
tr_result <- read.delim("gisette_train.labels",header=F)
gisette_tr <- cbind(gisette_tr,tr_result)
colnames(gisette_tr)[5001] <- "result"
gisette_tr[1:5,5001]
colnames(gisette_tr)[1:5]
colnames(gisette_tr)[1:5000] <- paste("var",c(1:5000),sep="")
colnames(gisette_tr)[1:5]

gisette_vl <- read.delim("gisette_valid.data",sep=" ",header=F)
vl_result <- read.delim("gisette_valid.labels",header=F)
gisette_vl <- cbind(gisette_vl,vl_result)
colnames(gisette_vl)[5002] <- "result"
colnames(gisette_vl)[5002]
colnames(gisette_vl)[1:5001] <- paste("var",c(1:5001),sep="")
colnames(gisette_vl)[1:5]

library(randomForest)
fit <- randomForest(gisette_tr$result ~ .,data=gisette_tr,ntree = 10)
is.na(gisette_vl)


#######################Hill-valey###########################
library(randomForest)
setwd("/home/timedreamer/Dropbox/Course/STA5635/BInaryClassificationDatasets/hill-valley")
setwd("D:\\Dropbox\\Course\\STA5635\\BInaryClassificationDatasets\\hill-valley")

hill_tr <- read.delim("X.dat",sep=" ",header=F)
result_tr <- read.delim("Y.dat",header=F)
colnames(result_tr) <- "result"

avg1 <- apply(hill_tr[1:10],1,mean)
avg2 <- apply(hill_tr[11:20],1,mean)
avg3 <- apply(hill_tr[21:30],1,mean)
avg4 <- apply(hill_tr[31:40],1,mean)
avg5 <- apply(hill_tr[41:50],1,mean)
avg6 <- apply(hill_tr[51:60],1,mean)
avg7 <- apply(hill_tr[61:70],1,mean)
avg8 <- apply(hill_tr[71:80],1,mean)
avg9 <- apply(hill_tr[81:90],1,mean)
avg10 <- apply(hill_tr[91:100],1,mean)


avg_total_tr <- cbind(avg1,avg2,avg3,avg4,avg5,avg6,avg7,avg8,avg9,avg10)
rank_avg_tr <- apply(avg_total_tr,1,rank)
rank_avg_tr <- t(rank_avg_tr)
colnames(rank_avg_tr)

hill_ts <- read.delim("Xtest.dat",sep=" ",header=F)
result_ts <- read.delim("Ytest.dat",header=F)
colnames(result_ts) <- "result"

avg1 <- apply(hill_ts[1:10],1,mean)
avg2 <- apply(hill_ts[11:20],1,mean)
avg3 <- apply(hill_ts[21:30],1,mean)
avg4 <- apply(hill_ts[31:40],1,mean)
avg5 <- apply(hill_ts[41:50],1,mean)
avg6 <- apply(hill_ts[51:60],1,mean)
avg7 <- apply(hill_ts[61:70],1,mean)
avg8 <- apply(hill_ts[71:80],1,mean)
avg9 <- apply(hill_ts[81:90],1,mean)
avg10 <- apply(hill_ts[91:100],1,mean)

avg_total_ts <- cbind(avg1,avg2,avg3,avg4,avg5,avg6,avg7,avg8,avg9,avg10)
rank_avg_ts <- apply(avg_total_ts,1,rank)
rank_avg_ts <- t(rank_avg_ts)
colnames(rank_avg_ts)

print(fit)
plot(fit)
varImpPlot(fit)
fit$err.rate
fit$

tr <- numeric()
ts <- numeric()

for (k in c(3,10,30,100,300)) {
  fit <- randomForest(x=rank_avg_tr, y=as.factor(result_tr$result),data=rank_avg_tr,ntree=k)
  error <- tail(fit$err.rate[,1],n=1)
  tr <- append(tr,error)
  
  fit.val <- predict(fit, newdata =rank_avg_ts,type = "class" )
  cm <- table(actual = result_ts$result,fitted=fit.val)
  mce <- 1 - (sum(diag(cm))/sum(cm))
  ts <- append(ts,mce)

}

plot(tr,type="b",col="blue",ylim=c(0.105,0.235),ylab="miscalculationRate",xlab="maxDepth")
lines(ts,col="red",type="b")
title(main="hill_valley")
legend("topright",c("train","test"),lty=c(1,1),col=c("blue","red"))


cm
mce <- 1 - (sum(diag(cm))/sum(cm))
ts <- append(ts,mce)
mce

##############arcene#####################
setwd("D:\\Dropbox\\Course\\STA5635\\BInaryClassificationDatasets\\arcene/")
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
  set.seed(k)
  fit <- randomForest(x=arcene_tr, y=as.factor(tr_result$result),data=arcene_tr,ntree=k)
  error <- tail(fit$err.rate[,1],n=1)
  tr <- append(tr,error)
  
  fit.val <- predict(fit, newdata =arcene_ts,type = "class" )
  cm <- table(actual = ts_result$result,fitted=fit.val)
  mce <- 1 - (sum(diag(cm))/sum(cm))
  ts <- append(ts,mce)
  
}

plot(tr,type="b",col="blue",ylim=c(0.18,0.39),xaxt="n",ylab="miscalculationRate",xlab="number of trees")
axis(1, at=1:5, labels=c(3,10,30,100,300))
lines(ts,col="red",type="b")
title(main="arcene")
legend("topright",c("train","test"),lty=c(1,1),col=c("blue","red"))

tr
ts
cm

#######################Madelon#################333
setwd("/home/timedreamer/Dropbox/Course/STA5635/BInaryClassificationDatasets/MADELON")
library(randomForest)
madelon_tr <- read.delim("madelon_train.data",sep=" ",header=F)
result_tr <- read.delim("madelon_train.labels",header=F)
madelon_tr[,501] <- NULL
colnames(result_tr) <- "result"
colnames(madelon_tr)[1:500] <- paste("var",c(1:500),sep="")
colnames(madelon_tr)[1:5]


madelon_ts <- read.delim("madelon_valid.data",sep=" ",header=F)
result_ts <- read.delim("madelon_valid.labels",header=F)
madelon_ts[,501] <- NULL
colnames(result_ts) <- "result"
colnames(madelon_ts)[1:500] <- paste("var",c(1:500),sep="")
colnames(madelon_ts)[1:5]

tr <- numeric()
ts <- numeric()

for (k in c(3,10,30,100,300)) {
    set.seed(k)
    fit <- randomForest(x=madelon_tr, y=as.factor(result_tr$result),data=madelon_tr,ntree=k)
    error <- tail(fit$err.rate[,1],n=1)
    tr <- append(tr,error)
    
    fit.val <- predict(fit, newdata =madelon_ts,type = "class" )
    cm <- table(actual = result_ts$result,fitted=fit.val)
    mce <- 1 - (sum(diag(cm))/sum(cm))
    ts <- append(ts,mce)
    
}

plot(tr,type="b",col="blue",ylim=c(0.26,0.440),xaxt="n",ylab="miscalculationRate",xlab="number of trees")
axis(1, at=1:5, labels=c(3,10,30,100,300))
lines(ts,col="red",type="b")
title(main="Madelon")
legend("topright",c("train","test"),lty=c(1,1),col=c("blue","red"))

tr
ts

#################Satimage###############
############reuslt for train data############################
setwd("/home/timedreamer/Dropbox/Course/STA5635/Multi-ClassDatasets/satimage")
sat_trn <- read.delim("X.dat",sep=" ",header=F)
cn <- paste("var",c(1:36),sep="")
colnames(sat_trn) <- cn
result_tr <- read.delim("Y.dat",header = F)
colnames(result_tr) <- "result"

sat_tst <- read.delim("Xtest.dat",sep=" ",header=F)
cn <- paste("var",c(1:36),sep="")
colnames(sat_tst) <- cn
result_ts <- read.delim("Ytest.dat",header=F)
colnames(result_ts) <-"result"

tr <-numeric()
ts <- numeric()

for (k in c(3,10,30,100,300)) {
    set.seed(k)
    fit <- randomForest(x=sat_trn, y=as.factor(result_tr$result),data=sat_trn,ntree=k)
    error <- tail(fit$err.rate[,1],n=1)
    tr <- append(tr,error)
    
    fit.val <- predict(fit, newdata =sat_tst,type = "class" )
    cm <- table(actual = result_ts$result,fitted=fit.val)
    mce <- 1 - (sum(diag(cm))/sum(cm))
    ts <- append(ts,mce)
    
}

plot(tr,type="b",col="blue",xaxt="n",ylab="miscalculationRate",xlab="number of trees")
axis(1, at=1:5, labels=c(3,10,30,100,300))
lines(ts,col="red",type="b")
title(main="Satimage")
legend("topright",c("train","test"),lty=c(1,1),col=c("blue","red"))

tr
ts

###########Poker#########################3
setwd("/home/timedreamer/Dropbox/Course/STA5635/Multi-ClassDatasets/poker")

#read training data. only keep size info.
poker_trx <- read.delim("X.dat",sep=" ",header=F)
poker_trx <- poker_trx[,c(2,4,6,8,10)]

poker_try <- read.delim("Y.dat",sep=" ",header=F)
colnames(poker_try) <- "hand"

# sort traiing poker hand from big to small.
tr_sort <- t(apply(poker_trx,1,sort,decreasing=T))
tr_sort[1:5,1:5]
colnames(tr_sort) <- paste("var",c(1:5),sep="")

#read test data.
poker_tsx <- read.delim("Xtest.dat",sep=" ",header=F)
poker_tsx <- poker_tsx[,c(2,4,6,8,10)]

poker_tsy <- read.delim("Ytest.dat",sep=" ",header=F)
colnames(poker_tsy) <- "hand"

#sort test data from big to small
ts_sort <- t(apply(poker_tsx,1,sort,decreasing=T))
ts_sort[1:5,1:5]
colnames(ts_sort) <- paste("var",c(1:5),sep="")

tr <-numeric()
ts <- numeric()

for (k in c(3,10,30,100,300)) {
    set.seed(k)
    fit <- randomForest(x=tr_sort, y=as.factor(poker_try$hand),data=tr_sort,ntree=k)
    error <- tail(fit$err.rate[,1],n=1)
    tr <- append(tr,error)
    
    fit.val <- predict(fit, newdata =ts_sort,type = "class" )
    cm <- table(actual = poker_tsy$hand,fitted=fit.val)
    mce <- 1 - (sum(diag(cm))/sum(cm))
    ts <- append(ts,mce)
    
}

plot(tr,type="b",col="blue",ylim=c(0.049,0.125),xaxt="n",ylab="miscalculationRate",xlab="number of trees")
axis(1, at=1:5, labels=c(3,10,30,100,300))
lines(ts,col="red",type="b")
title(main="poker")
legend("topright",c("train","test"),lty=c(1,1),col=c("blue","red"))

tr
ts
cm

#############covtype################
setwd("D:\\Dropbox\\Course\\STA5635\\Multi-ClassDatasets\\covtype//")
setwd("/home/timedreamer/Dropbox/Course/STA5635/Multi-ClassDatasets/covtype")
covtype <- read.delim("covtype.data",sep=",",header=F)
nrow(covtype)
cov_tr <- covtype[1:15120,]
nrow(cov_tr)
cov_ts <- covtype[15121:581012,]
rm(covtype)

result_tr <- cov_tr[,55]
result_ts <- cov_ts[,55]

cov_tr[,55] <- NULL
cov_ts[,55] <- NULL
cn <- paste("var",c(1:54),sep="")
colnames(cov_tr) <- cn
colnames(cov_ts) <- cn


tr <- numeric()
ts <- numeric()

for (k in c(3,10,30,100,300)) {
    set.seed(k)
    fit <- randomForest(x=cov_tr, y=as.factor(result_tr),data=cov_tr,ntree=k)
    error <- tail(fit$err.rate[,1],n=1)
    tr <- append(tr,error)
    
    fit.val <- predict(fit, newdata =cov_ts,type = "class" )
    cm <- table(actual = result_ts,fitted=fit.val)
    mce <- 1 - (sum(diag(cm))/sum(cm))
    ts <- append(ts,mce)
    
}

plot(tr,type="b",col="blue",ylim=c(0.168,0.394),xaxt="n",ylab="miscalculationRate",xlab="number of trees")
axis(1, at=1:5, labels=c(3,10,30,100,300))
lines(ts,col="red",type="b")
title(main="cov_type")
legend("topright",c("train","test"),lty=c(1,1),col=c("blue","red"))

tr
ts

##############miniboone
setwd("/home/timedreamer/Dropbox/Course/STA5635/BInaryClassificationDatasets/Miniboone")
library(randomForest)
mini <- read.delim('miniboone_noFirstline.txt',sep="\t",header=F)

result <- c(rep(1,times=36499),rep(0,times=93565))

colnames(mini)[1:50] <- paste("var",c(1:50),sep="")

tr <- numeric()

for (k in c(3,10,30,100,300)) {
    set.seed(k)
    fit <- randomForest(x=mini, y=as.factor(result),data=mini,ntree=k)
    error <- tail(fit$err.rate[,1],n=1)
    tr <- append(tr,error)
    
    
}

plot(tr,type="b",col="blue",xaxt="n",ylab="miscalculationRate",xlab="number of trees")
axis(1, at=1:5, labels=c(3,10,30,100,300))
title(main="miniboone")
legend("topright",c("train"),lty=c(1,0),col="blue")

ptm <- proc.time()
r <- rfcv(mini,as.factor(result),cv.fold = 4) # did on pauper
proc.time()  -ptm

with(r, plot(n.var, error.cv, log="x", type="o", lwd=2))
title(main="miniboone_4_fold_cv_predictors")
save(r,file = "rfcv_result.RData")
rm(r)

##############Dexter
setwd("/home/timedreamer/Dropbox/Course/STA5635/BInaryClassificationDatasets/dexter/")
setwd("D:\\Dropbox\\Course\\STA5635\\BInaryClassificationDatasets\\dexter")

tr_d <- read.delim("dexter_train.data",sep="",header=F,col.names = paste("var",c(1:329),sep=""),colClasses = "character")
result_tr <- read.delim("dexter_train.labels",header=F)
colnames(result_tr) <- "result"


ts_d <- read.delim("dexter_valid.data",sep="",header=F,col.names=paste("var",c(1:329),sep=""),colClasses = "character")
result_ts <- read.delim("dexter_valid.labels",header=F)
colnames(result_ts) <- "result"


# make a 300*20000 matrix
300*20000
word_matrix_tr <- matrix(rep(0,6e+6),nrow = 300,ncol = 20000)
word_matrix_ts <- matrix(rep(0,6e+6),nrow = 300,ncol = 20000)

# change value by nested loops.
for (i in 1:300) {
  t <- as.character(tr_d[i,])
  t1 <- t[nchar(t)!= 0]
  t2 <- strsplit(t1,split=":")
  for (j in 1:length(t2)){
    word_matrix_tr[i,as.numeric(t2[[j]][1])] <- as.numeric(t2[[j]][2])  
  }
}

for (i in 1:300) {
  t <- as.character(ts_d[i,])
  t1 <- t[nchar(t)!= 0]
  t2 <- strsplit(t1,split=":")
  for (j in 1:length(t2)){
    word_matrix_ts[i,as.numeric(t2[[j]][1])] <- as.numeric(t2[[j]][2])  
  }
}

save(word_matrix_tr,word_matrix_ts,file="word_matrix.RData")

word_matrix[1:10,1:10]

colnames(word_matrix_tr) <- paste("word",c(1:20000),sep="")
colnames(word_matrix_ts) <- paste("word",c(1:20000),sep="")

tr <- numeric()
ts <- numeric()

for (k in c(3,10,30,100,300)) {
  set.seed(k)
  fit <- randomForest(x=word_matrix_tr, y=as.factor(result_tr$result),data=word_matrix_tr,ntree=k)
  error <- tail(fit$err.rate[,1],n=1)
  tr <- append(tr,error)
  
  fit.val <- predict(fit, newdata =word_matrix_ts,type = "class" )
  cm <- table(actual = result_ts$result,fitted=fit.val)
  mce <- 1 - (sum(diag(cm))/sum(cm))
  ts <- append(ts,mce)
  
}

str(result_tr)

plot(tr,type="b",col="blue",xaxt="n",ylab="miscalculationRate",xlab="number of trees")
axis(1, at=1:5, labels=c(3,10,30,100,300))
lines(ts,col="red",type="b")
title(main="dexter")
legend("topright",c("train","test"),lty=c(1,1),col=c("blue","red"))

tr
ts