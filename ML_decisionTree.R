getwd()
setwd("/home/timedreamer/Dropbox/Course/STA5635/Multi-ClassDatasets/satimage")
setwd("D:\\Dropbox\\Course\\STA5635\\Multi-ClassDatasets\\satimage")
library(rpart)

sat <- read.delim("sat.trn",sep=" ",header=F)

cn <- paste("var",c(1:37),sep="")
colnames(sat) <- cn
sat$var37 <- as.factor(sat$var37)
sat[1:5,1:5]
sat[1:5,37]
table(sat[37])

?rpart
fit <- rpart(sat$var37 ~ .,method = "class",data=sat,maxdepth=2)
?printcp
?plotcp
printcp(fit)
plotcp(fit)

plot(fit,uniform=TRUE,main="classification tree")
text(fit,use.n=T,all=T,cex=0.8)


fit.val <- predict(fit,type="class")

length(fit.val[fit.val=="1"])/length(sat$var37)

cm <- table(actual = sat$var37,fitted=fit.val)

mce <- 1 - (sum(diag(cm))/sum(cm))
mce

#####Satimage#######################
############reuslt for train data############################
sat_trn <- read.delim("sat.trn",sep=" ",header=F)
cn <- paste("var",c(1:37),sep="")
colnames(sat_trn) <- cn
sat_trn$var37 <- as.factor(sat_trn$var37)
table(sat_trn[37])

tr <-numeric()
for (i in 1:12) {
  fit <- rpart(sat_trn$var37 ~ .,method = "class",data=sat_trn,maxdepth=i)
  fit.val <- predict(fit,type="class")
  cm <- table(actual = sat_trn$var37,fitted=fit.val)
  mce <- 1 - (sum(diag(cm))/sum(cm))
  tr <- append(tr,mce)
}
plot(tr)

sat_tst <- read.delim("sat.tst",sep=" ",header=F)
cn <- paste("var",c(1:37),sep="")
colnames(sat_tst) <- cn
sat_tst$var37 <- as.factor(sat_tst$var37)
table(sat_tst[37])

ts <-numeric()
for (i in 1:12) {
  fit <- rpart(sat_tst$var37 ~ .,method = "class",data=sat_tst,maxdepth=i)
  fit.val <- predict(fit,type="class")
  cm <- table(actual = sat_tst$var37,fitted=fit.val)
  mce <- 1 - (sum(diag(cm))/sum(cm))
  ts <- append(ts,mce)
}
plot(tr,type="b",col="blue",ylim=c(0.15,0.6),ylab="miscalculationRate",xlab="maxDepth")
lines(ts,col="red",type="b")
title(main="Satimage")

################################################################

#########PokerHand###########################
setwd("D:\\Dropbox\\Course\\STA5635\\Multi-ClassDatasets\\poker/")
setwd("/home/timedreamer/Dropbox/Course/STA5635/Multi-ClassDatasets/poker")

poker_trx <- read.delim("X.dat",sep=" ",header=F)
poker_try <- read.delim("Y.dat",sep=" ",header=F)
colnames(poker_try) <- "hand"

tr_size <- poker_trx[,c(2,4,6,8,10)]
colnames(tr_size) <- paste("hand",c(1:5),sep="")

tr_color <- poker_trx[,c(1,3,5,7,9)]
colnames(tr_color) <- paste("hand",c(1:5),sep="")

tr_size[1:10,]
tr_color[1:10,]

poker_tsx <- read.delim("Xtest.dat",sep=" ",header=F)
poker_tsy <- read.delim("Ytest.dat",sep=" ",header=F)
colnames(poker_tsy) <- "hand"

ts_size <- poker_tsx[,c(2,4,6,8,10)]
colnames(ts_size) <- paste("hand",c(1:5),sep="")

ts_color <- poker_tsx[,c(1,3,5,7,9)]
colnames(ts_color) <- paste("hand",c(1:5),sep="")

ts_size[1:10,]
ts_color[1:10,]


# sort training poker data.
tr_size_sort <- data.frame()
tr_color_sort <- data.frame()

tr <- numeric()
for (i in 1:25010){
    t1 <- sort(tr_size[i,],decreasing = T)
    c1 <- colnames(t1)
    t1 <- as.numeric(t1[1,])
    tr_size_sort <- rbind(tr_size_sort,t1)
    
    t2 <- tr_color[i,]
    t2 <- t2[,c1]
    t2 <- as.numeric(t2[1,])
    tr_color_sort <- rbind(tr_color_sort,t2)
}

tr_final <- cbind(tr_size_sort,tr_color_sort)
colnames(tr_final) <- paste("var",c(1:10),sep="")
tr_final[1:5,]
tr_final <- cbind(tr_final,poker_try)

# sort test data.
ptm <- proc.time()
ts_size_sort <- data.frame()
ts_color_sort <- data.frame()

ts <- numeric()
for (i in 1:100000){
    t1 <- sort(ts_size[i,],decreasing = T)
    c1 <- colnames(t1)
    t1 <- as.numeric(t1[1,])
    ts_size_sort <- rbind(ts_size_sort,t1)
    
    t2 <- ts_color[i,]
    t2 <- t2[,c1]
    t2 <- as.numeric(t2[1,])
    ts_color_sort <- rbind(ts_color_sort,t2)
}

proc.time() -ptm
sortFun <- function(x){
    t1 <- sort(x,decreasing = T)
    c1 <- colnames(t1)
    #t1 <- as.numeric(t1[1,])
    return(t1)
    
}

tts <- ts_size[1:10,]
pp <- apply(tts,1,sortFun)

t1 <- sort(tts,decreasing = T)
c1 <- colnames(t1)
t1 <- as.numeric(t1[1,])

ts_final1 <- cbind(ts_size_sort,ts_color_sort)

colnames(ts_final) <- paste("var",c(1:10),sep="")
ts_final[1:5,]
ts_final <- cbind(ts_final,poker_tsy[1:100000,])



tr <-numeric()
library(rpart)
for (i in 1:12) {
  fit <- rpart(tr_final$hand ~ .,method = "class",data=tr_final,maxdepth=i)
  fit.val <- predict(fit,type="class")
  cm <- table(actual = tr_final$hand,fitted=fit.val)
  mce <- 1 - (sum(diag(cm))/sum(cm))
  tr <- append(tr,mce)
  
  fit.val <- predict(fit,type="class",newdata=ts_final)
  cm <- table(actual = ts_final$hand,fitted=fit.val)
  mce <- 1 - (sum(diag(cm))/sum(cm))
  ts <- append(ts,mce)
}
plot(tr)
printcp(fit)

plot(tr,type="b",col="blue",ylab="miscalculationRate",xlab="maxDepth")
lines(ts,col="red",type="b")
title(main="poker_hand")
legend("topright",c("train","test"),lty=c(1,1),col=c("blue","red"))




fit <- rpart(hand ~ .,method = "class",data=poker_tr,minsplit=2, minbucket=1)
fit.val
#########################################################



####covType#################
setwd("D:\\Dropbox\\Course\\STA5635\\Multi-ClassDatasets\\covtype//")
covtype <- read.delim("covtype.data",sep=",",header=F)
nrow(covtype)
cov_tr <- covtype[1:11340,]
nrow(cov_tr)
cov_vl <- covtype[11341:15120,]
cov_ts <- covtype[15121:581012,]
rm(covtype)

cn <- paste("var",c(1:55),sep="")
colnames(cov_tr) <- cn
colnames(cov_vl) <- cn
colnames(cov_ts) <- cn


fit <- rpart(var55 ~ .,method = "class",data=cov_tr)
fit.val <- predict(fit,type="class")
cm <- table(actual = cov_tr$var55,fitted=fit.val)
mce <- 1 - (sum(diag(cm))/sum(cm))
tr <- append(tr,mce)

t <- as.vector(cov_tr[,1])
hist(t)


tr <- numeric()
vl <- numeric()
ts <- numeric()
for (i in 1:12) {
  fit <- rpart(cov_tr$var55 ~ .,method = "class",data=cov_tr,maxdepth=i)
  fit.val <- predict(pfit,type="class")
  cm <- table(actual = cov_tr$var55,fitted=fit.val)
  mce <- 1 - (sum(diag(cm))/sum(cm))
  tr <- append(tr,mce)
  
  fit.val <- predict(fit,type="class",newdata= cov_vl)
  cm <- table(actual = cov_vl$var55,fitted=fit.val)
  mce <- 1 - (sum(diag(cm))/sum(cm))
  vl <- append(vl,mce)
  
  fit.val <- predict(fit,type="class",newdata= cov_ts)
  cm <- table(actual = cov_ts$var55,fitted=fit.val)
  mce <- 1 - (sum(diag(cm))/sum(cm))
  ts <- append(ts,mce)
}

plot(tr,type="b",col="blue",ylim=c(0.35,1),ylab="miscalculationRate",xlab="maxDepth")
lines(ts,col="red",type="b")
title(main="cov_type")

############Dexter##############################################################
getwd()
setwd("/home/timedreamer/Dropbox/Course/STA5635/BInaryClassificationDatasets/dexter")

dexter_tr <- read.delim("dexter_train.data",sep=" ",header=F)
head(dexter_tr)

tr_result <- read.delim("dexter_train.labels",header=F)

dexter_tr <- cbind(dexter_tr,tr_result)

##########gisette##########################
getwd()
setwd("/home/timedreamer/Dropbox/Course/STA5635/BInaryClassificationDatasets/Gisette/")

gisette_tr <- read.delim("gisette_train.data",sep=" ",header=F)

tr_result <- read.delim("gisette_train.labels",header=F)
gisette_tr <- cbind(gisette_tr,tr_result)
colnames(gisette_tr)[5002] <- "result"
colnames(gisette_tr)[5002]

colnames(gisette_tr)[1:5001] <- paste("var",c(1:5001),sep="")
colnames(gisette_tr)[1:5]

gisette_vl <- read.delim("gisette_valid.data",sep=" ",header=F)
vl_result <- read.delim("gisette_valid.labels",header=F)
gisette_vl <- cbind(gisette_vl,vl_result)
colnames(gisette_vl)[5002] <- "result"
colnames(gisette_vl)[5002]
colnames(gisette_vl)[1:5001] <- paste("var",c(1:5001),sep="")
colnames(gisette_vl)[1:5]

it <- rpart(result ~ .,method = "class",data=gisette_tr)
printcp(it)
plotcp(it)

plot(it,uniform=TRUE,main="classification tree")
text(it,use.n=T,all=T,cex=0.8)

fit <- rpart(cov_tr$var55 ~ .,method = "class",data=cov_tr,maxdepth=i)
fit.val <- predict(it,type="class")
cm <- table(actual = gisette_tr$result,fitted=fit.val)
mce <- 1 - (sum(diag(cm))/sum(cm))
tr <- append(tr,mce)
mce
cm

tr <- numeric()
vl <- numeric()

for (i in 1:12) {
    fit <- rpart(gisette_tr$result ~ .,method = "class",data=gisette_tr,maxdepth=i)
    fit.val <- predict(fit,type="class")
    cm <- table(actual = gisette_tr$result,fitted=fit.val)
    mce <- 1 - (sum(diag(cm))/sum(cm))
    tr <- append(tr,mce)
    
    
    fit.val <- predict(fit,type="class",newdata=gisette_vl )
    cm <- table(actual = gisette_vl$result,fitted=fit.val)
    mce <- 1 - (sum(diag(cm))/sum(cm))
    vl <- append(vl,mce)
}
plot(tr,type="b",col="blue",ylim=c(0.07,0.17),ylab="miscalculationRate",xlab="maxDepth")
lines(vl,col="red",type="b")
title(main="gisette")

# no difference for a prune tree.
for (i in 1:12) {
  fit <- rpart(gisette_tr$result ~ .,method = "class",data=gisette_tr,maxdepth=i)
  pfit<- prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
  fit.val <- predict(pfit,type="class")
  cm <- table(actual = gisette_tr$result,fitted=fit.val)
  mce <- 1 - (sum(diag(cm))/sum(cm))
  tr <- append(tr,mce)
  
  
  fit.val <- predict(pfit,type="class",newdata=gisette_vl )
  cm <- table(actual = gisette_vl$result,fitted=fit.val)
  mce <- 1 - (sum(diag(cm))/sum(cm))
  vl <- append(vl,mce)
}
plot(tr,type="b",col="blue",ylim=c(0.07,0.17),ylab="miscalculationRate",xlab="maxDepth")
lines(vl,col="red",type="b")
title(main="gisette")

###################Hill##########################
getwd()
setwd("D:\\Dropbox\\Course\\STA5635\\BInaryClassificationDatasets\\hill-valley")
setwd("/home/timedreamer/Dropbox/Course/STA5635/BInaryClassificationDatasets/hill-valley")

hill_tr <- read.delim("X.dat",sep=" ",header=F)
result_tr <- read.delim("Y.dat",header=F)

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
rank_avg_tr <- cbind(rank_avg_tr,result_tr)
colnames(rank_avg_tr)
colnames(rank_avg_tr)[11] <- "result"

hill_ts <- read.delim("Xtest.dat",sep=" ",header=F)
result_ts <- read.delim("Ytest.dat",sep=" ",header=F)

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
rank_avg_ts <- cbind(rank_avg_ts,result_ts)
colnames(rank_avg_ts)
colnames(rank_avg_ts)[11] <- "result"



tr <- numeric()
ts <- numeric()

for (i in 1:12) {
  fit <- rpart(result ~ .,method = "class",data=rank_avg_tr,maxdepth=i,xval=0)
  fit.val <- predict(fit,type="class")
  cm <- table(actual = rank_avg_tr$result,fitted=fit.val)
  mce <- 1 - (sum(diag(cm))/sum(cm))
  tr <- append(tr,mce)
  
  
  fit.val <- predict(fit, type="class", newdata= rank_avg_ts)
  cm <- table(actual = rank_avg_ts$result,fitted=fit.val)
  mce <- 1 - (sum(diag(cm))/sum(cm))
  ts <- append(ts,mce)
}
plot(tr,type="b",col="blue",ylim=c(0.16,0.37),ylab="miscalculationRate",xlab="maxDepth")
lines(ts,col="red",type="b")
title(main="hill_valley")
legend("topright",c("train","test"),lty=c(1,1),col=c("blue","red"))

tr
ts




###########Madelon

getwd()
setwd("D:\\Dropbox\\Course\\STA5635\\BInaryClassificationDatasets\\MADELON/")

madelon_tr <- read.delim("madelon_train.data",sep=" ",header=F)
result_tr <- read.delim("madelon_train.labels",header=F)
madelon_tr <- cbind(madelon_tr,result_tr)

colnames(madelon_tr)[502] <- "result"
colnames(madelon_tr)[1:501] <- paste("var",c(1:501),sep="")
colnames(madelon_tr)[1:5]


madelon_ts <- read.delim("madelon_valid.data",sep=" ",header=F)
result_ts <- read.delim("madelon_valid.labels",header=F)
madelon_ts <- cbind(madelon_ts,result_ts)

colnames(madelon_ts)[502] <- "result"
colnames(madelon_ts)[1:501] <- paste("var",c(1:501),sep="")
colnames(madelon_ts)[1:5]

tr <- numeric()
ts <- numeric()

for (i in 1:12) {
  fit <- rpart(result ~ .,method = "class",data=madelon_tr,maxdepth=i,xval=0)
  fit.val <- predict(fit,type="class")
  cm <- table(actual = madelon_tr$result,fitted=fit.val)
  mce <- 1 - (sum(diag(cm))/sum(cm))
  tr <- append(tr,mce)
  
  
  fit.val <- predict(fit, type="class", newdata= madelon_ts)
  cm <- table(actual = madelon_ts$result,fitted=fit.val)
  mce <- 1 - (sum(diag(cm))/sum(cm))
  ts <- append(ts,mce)
}
plot(tr,type="b",col="blue",ylim=c(0.15,0.39),ylab="miscalculationRate",xlab="maxDepth")
lines(ts,col="red",type="b")
title(main="madelon")
legend("topright",c("train","test"),lty=c(1,1),col=c("blue","red"))



############Miniboone

getwd()
setwd("D:\\Dropbox\\Course\\STA5635\\BInaryClassificationDatasets\\Miniboone")

mini <- read.delim('miniboone_noFirstline.txt',sep="\t",header=F)

result <- c(rep(1,times=36499),rep(0,times=93565))


mini <- cbind(mini,result)
rm(result)
mini[1:5,51]


colnames(mini)[1:50] <- paste("var",c(1:50),sep="")

tr <- numeric()

for (i in 1:12) {
  fit <- rpart(result ~ .,method = "class",data=mini,maxdepth=i,xval=4)
  pfit<- prune(fit, cp= fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])
  fit.val <- predict(pfit,type="class")
  cm <- table(actual = mini$result,fitted=fit.val)
  mce <- 1 - (sum(diag(cm))/sum(cm))
  tr <- append(tr,mce)
  
}


plot(tr,type="b",col="blue",ylab="miscalculationRate",xlab="maxDepth")
title(main="miniboone")

printcp(fit)
tr
