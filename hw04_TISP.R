###########ARCENE#####################
#read train data
setwd("D:\\Dropbox\\Course\\STA5635\\BInaryClassificationDatasets\\arcene/")
setwd("/home/timedreamer/Dropbox/Course/STA5635/BInaryClassificationDatasets/arcene")
x <- read.delim("arcene_train.data",sep=" ",header=F)
x[,10001] <- NULL

y <- read.delim("arcene_train.labels",header=F)
colnames(y) <- "result"
y <- y$result
y[y==-1] <- 0

#read test data
x.ts <- read.delim("arcene_valid.data",sep=" ",header=F)
x.ts[,10001] <- NULL
y.ts <- read.delim("arcene_valid.labels",sep=" ",header=F)
colnames(y.ts) <- "result"
y.ts <- y.ts$result
y.ts[y.ts==-1] <- 0


# remove columns that sd is 0 from both train and test data
sd.tr <- apply(x,2,sd)
x <- x[,sd.tr!=0]
x.ts <- x.ts[,sd.tr!=0]
mean.tr <- apply(x,2,mean)
sd.tr <- apply(x,2,sd)

#function to normalize train data
z.score <- function(m) {
  (m-mean(m))/sd(m)
}

x.norm <- apply(x,2,z.score)
x.norm <- cbind(rep(1,nrow(x.norm)),x.norm) # add a column of 1 in the first column, theta0


#create a blank matrix and normalize test data using mean and sd from train data.
# do this by columns. may take some time.
x.ts.norm <- matrix()
for (j in c(1:ncol(x.ts))){
  temp1 <- (x.ts[j] -mean.tr[j])/sd.tr[j]
  x.ts.norm <- cbind(x.ts.norm,temp1)
}
x.ts.norm[1] <- NULL # delete first column
x.ts.norm <- cbind(rep(1,nrow(x.ts.norm)),x.ts.norm)
x.ts.norm <- as.matrix(x.ts.norm) # convert to matrix format

dim(x);dim(x.norm)

#Sigmoid function
sigmoid <- function(z)
{
  g <- 1/(1+exp(-z))
  return(g)
}


beta <- rep(0,rep(ncol(x.norm))) # assgin original beta as 0s
lamda <- 5 # this is the rate I can change.
miss_rate <- numeric()

total <- length(y.ts)

#repeat 10 or more times and calculate miss calculation rate.
# User can change pred line to calculate misscalculation rate
# from train data or test data. also, can change beta to decide which
# data used for training.
for (i in 1:10){
  beta <- beta +t(x.norm)%*%y - t(x.norm)%*%sigmoid(x.norm%*%beta)
  beta[abs(beta) < lamda] <- 0
  
  pred <- sigmoid(x.ts.norm%*%beta) # change here
  pred[pred>0.5] <- 1
  pred[pred <=0.5] <- 0
  miss_number <- length(which(pred!=y.ts))
  
  miss_rate <- append(miss_rate,miss_number/total)
  
  i <- i+1
}

print(miss_rate)
plot(miss_rate,type="b")

#######################DEXTER
load("D:/Dropbox/Course/STA5635/BInaryClassificationDatasets/dexter/word_matrix.RData")
setwd("D:\\Dropbox\\Course\\STA5635\\BInaryClassificationDatasets\\dexter")
setwd("/home/timedreamer/Dropbox/Course/STA5635/BInaryClassificationDatasets/dexter")
load("word_matrix.RData")
x <- word_matrix_tr
rm(word_matrix_tr)
dim(x)

x.ts <- word_matrix_ts

sd.tr <- apply(x,2,sd)
x <- x[,sd.tr!=0]
x.ts <- x.ts[,sd.tr!=0] 
mean.tr <- apply(x,2,mean)
sd.tr <- apply(x,2,sd)


y <- read.delim("dexter_train.labels",header=F)
colnames(y) <- "result"
y <- y$result
y[y==-1] <- 0

y.ts <- read.delim("dexter_valid.labels",header=F)
colnames(y.ts) <- "result"
y.ts[y.ts==-1] <- 0
y.ts <- y.ts$result

x.norm <- apply(x,2,z.score)
x.norm <- cbind(rep(1,nrow(x.norm)),x.norm) 
dim(x.norm)

x.norm.ts <- vector('numeric')
for (j in c(1:ncol(x.ts))){
  temp1 <- (x.ts[,j] -mean.tr[j])/sd.tr[j]
  x.norm.ts <- cbind(x.norm.ts,temp1)
}

x.norm.ts <- cbind(rep(1,nrow(x.norm.ts)),x.norm.ts)
#x.ts.norm <- as.matrix(x.ts.norm) # convert to matrix format
x.norm.ts[1:5,1:5]

beta <- rep(0,rep(ncol(x.norm))) # assgin original beta as 0s
lamda <- 10 # this is the rate I can change. for dexter: 50
miss_rate <- numeric()

total <- length(y)


for (i in 1:10){
  beta <- beta +t(x.norm)%*%y - t(x.norm)%*%sigmoid(x.norm%*%beta)
  beta[abs(beta) < lamda] <- 0
  beta
  
  pred <- sigmoid(x.norm%*%beta) # change here
  pred[pred>0.5] <- 1
  pred[pred <=0.5] <- 0
  miss_number <- length(which(pred!=y))
  
  miss_rate <- append(miss_rate,miss_number/total)
  
  i <- i+1
}

print(miss_rate)
plot(miss_rate,type="b")



#############Gissete
setwd("D:\\Dropbox\\Course\\STA5635\\BInaryClassificationDatasets\\Gisette")
setwd("/home/timedreamer/Dropbox/Course/STA5635/BInaryClassificationDatasets/Gisette")
x <- read.delim("gisette_train.data",sep=" ",header=F)
x$V5001 <- NULL

x.ts <- read.delim("gisette_valid.data",sep=" ",header=F)
x.ts$V5001 <- NULL


sd.tr <- apply(x,2,sd)
x <- x[,sd.tr!=0]
x.ts <- x.ts[,sd.tr!=0] 
mean.tr <- apply(x,2,mean)
sd.tr <- apply(x,2,sd)


y <- read.delim("gisette_train.labels",header=F)
colnames(y) <- "result"
y <-y$result 
y[y==-1] <- 0

y.ts <- read.delim("gisette_valid.labels",header=F)
colnames(y.ts) <- "result"
y.ts <- y.ts$result
y.ts[y.ts==-1] <- 0

x.norm <- apply(x,2,z.score)
x.norm <- cbind(rep(1,nrow(x.norm)),x.norm) 
dim(x.norm)

x.norm.ts <- vector('numeric')
for (j in c(1:ncol(x.ts))){
  temp1 <- (x.ts[,j] -mean.tr[j])/sd.tr[j]
  x.norm.ts <- cbind(x.norm.ts,temp1)
}

x.norm.ts <- cbind(rep(1,nrow(x.norm.ts)),x.norm.ts)
#x.ts.norm <- as.matrix(x.ts.norm) # convert to matrix format
x.norm.ts[1:5,1:5]

beta <- rep(0,rep(ncol(x.norm))) # assgin original beta as 0s
lamda <- 5 # this is the rate I can change. for gissete: 5
miss_rate <- numeric()

total <- length(y.ts)


for (i in 1:10){
  beta <- beta +t(x.norm)%*%y - t(x.norm)%*%sigmoid(x.norm%*%beta)
  beta[abs(beta) < lamda] <- 0
  beta
  
  pred <- sigmoid(x.norm.ts%*%beta) # change here
  pred[pred>0.5] <- 1
  pred[pred <=0.5] <- 0
  miss_number <- length(which(pred!=y.ts))
  
  miss_rate <- append(miss_rate,miss_number/total)
  
  i <- i+1
}

print(miss_rate)
plot(miss_rate,type="b")


#################Hill_valley
setwd("D:\\Dropbox\\Course\\STA5635\\BInaryClassificationDatasets\\hill-valley")
load("/home/timedreamer/Dropbox/Course/STA5635/BInaryClassificationDatasets/hill-valley/hill_rankAvg.RData")
# process training data. get average and then rank.
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

# process test data. get average and then rank.

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

save(rank_avg_tr,rank_avg_ts,result_tr,result_ts,file="hill_rankAvg.RData")

x <- rank_avg_tr
y <- result_tr$result

x.ts <- rank_avg_ts
y.ts <- result_ts$result

sd.tr <- apply(x,2,sd)
x <- x[,sd.tr!=0]
x.ts <- x.ts[,sd.tr!=0] 
mean.tr <- apply(x,2,mean)
sd.tr <- apply(x,2,sd)

x.norm <- apply(x,2,z.score)
x.norm <- cbind(rep(1,nrow(x.norm)),x.norm) 
dim(x.norm)

x.norm.ts <- vector('numeric')
for (j in c(1:ncol(x.ts))){
  temp1 <- (x.ts[,j] -mean.tr[j])/sd.tr[j]
  x.norm.ts <- cbind(x.norm.ts,temp1)
}

x.norm.ts <- cbind(rep(1,nrow(x.norm.ts)),x.norm.ts)
#x.ts.norm <- as.matrix(x.ts.norm) # convert to matrix format
x.norm.ts[1:5,1:5]

beta <- rep(0,rep(ncol(x.norm))) # assgin original beta as 0s
lamda <- 60 # this is the rate I can change. for hill: 60
miss_rate <- numeric()

total <- length(y)


for (i in 1:10){
  beta <- beta +t(x.norm)%*%y - t(x.norm)%*%sigmoid(x.norm%*%beta)
  beta[abs(beta) < lamda] <- 0
  
  pred <- sigmoid(x.norm.ts%*%beta) # change here
  pred[pred>0.5] <- 1
  pred[pred <=0.5] <- 0
  miss_number <- length(which(pred!=y.ts))
  
  miss_rate <- append(miss_rate,miss_number/total)
  
  i <- i+1
}

print(miss_rate)
plot(miss_rate,type="b")


################Madelon###########
setwd("D:\\Dropbox\\Course\\STA5635\\BInaryClassificationDatasets\\MADELON")
setwd("/home/timedreamer/Dropbox/Course/STA5635/BInaryClassificationDatasets/MADELON")
x <- read.delim("madelon_train.data",sep=" ",header=F)
y <- read.delim("madelon_train.labels",header=F)
x[,501] <- NULL
colnames(y) <- "result"
y <- y$result
y[y==-1] <- 0

x.ts <- read.delim("madelon_valid.data",sep=" ",header=F)
x.ts[,501] <- NULL

sd.tr <- apply(x,2,sd)
x <- x[,sd.tr!=0]
x.ts <- x.ts[,sd.tr!=0] 
mean.tr <- apply(x,2,mean)
sd.tr <- apply(x,2,sd)



y.ts <- read.delim("madelon_valid.labels",header=F)
colnames(y.ts) <- "result"
y.ts <- y.ts$result
y.ts[y.ts==-1] <- 0



x.norm <- apply(x,2,z.score)
x.norm <- cbind(rep(1,nrow(x.norm)),x.norm) 
dim(x.norm)

x.norm.ts <- vector('numeric')
for (j in c(1:ncol(x.ts))){
  temp1 <- (x.ts[,j] -mean.tr[j])/sd.tr[j]
  x.norm.ts <- cbind(x.norm.ts,temp1)
}

x.norm.ts <- cbind(rep(1,nrow(x.norm.ts)),x.norm.ts)
#x.ts.norm <- as.matrix(x.ts.norm) # convert to matrix format
x.norm.ts[1:5,1:5]

beta <- rep(0,rep(ncol(x.norm))) # assgin original beta as 0s
lamda <- 20 # this is the rate I can change. for madelon lamda=20
miss_rate <- numeric()

total <- length(y.ts)


for (i in 1:10){
  beta <- beta +t(x.norm)%*%y - t(x.norm)%*%sigmoid(x.norm%*%beta)
  beta[abs(beta) < lamda] <- 0
  
  pred <- sigmoid(x.norm.ts%*%beta) # change here
  pred[pred>0.5] <- 1
  pred[pred <=0.5] <- 0
  miss_number <- length(which(pred!=y.ts))
  
  miss_rate <- append(miss_rate,miss_number/total)
  
  i <- i+1
}

print(miss_rate)
plot(miss_rate,type="b")

########Miniboone#####################
setwd("D:\\Dropbox\\Course\\STA5635\\BInaryClassificationDatasets\\Miniboone")
setwd("/home/timedreamer/Dropbox/Course/STA5635/BInaryClassificationDatasets/Miniboone")
x <- read.delim('miniboone_noFirstline.txt',sep="\t",header=F)

y <- as.data.frame(c(rep(1,times=36499),rep(0,times=93565)))
y_label <- cbind(rep(1:4),y)
y_label[1:5,1:2]
colnames(y_label) <- c("label","result")


x_label <- cbind(rep(1:4),x)
colnames(x_label)[1] <- "label"

########SET1
x_tr1 <- subset(x_label,label==2|label==3|label==4)
x_ts1 <- subset(x_label,label==1)

y_tr1 <- subset(y_label,label==2|label==3|label==4)
y_tr1 <- y_tr1$result
y_ts1 <- subset(y_label,label==1)
y_ts1 <- y_ts1$result

mean.tr <- apply(x_tr1,2,mean)
sd.tr <- apply(x_tr1,2,sd)

x_tr1.norm <- apply(x_tr1,2,z.score)
x_tr1.norm <- cbind(rep(1,nrow(x_tr1.norm)),x_tr1.norm) 
dim(x_tr1.norm)

x_ts1.norm <- vector('numeric')
for (j in c(1:ncol(x_ts1))){
  temp1 <- (x_ts1[,j] -mean.tr[j])/sd.tr[j]
  x_ts1.norm <- cbind(x_ts1.norm,temp1)
}

x_ts1.norm <- cbind(rep(1,nrow(x_ts1.norm)),x_ts1.norm)

x_ts1.norm[1:5,1:5]


beta <- rep(0,rep(ncol(x_tr1.norm))) # assgin original beta as 0s
lamda <- 100 # this is the rate I can change. for miniboone:100
miss_rate <- numeric()

total <- length(y_tr1)


for (i in 1:10){
  beta <- beta +t(x_tr1.norm)%*%y_tr1 - t(x_tr1.norm)%*%sigmoid(x_tr1.norm%*%beta)
  beta[abs(beta) < lamda] <- 0
  beta
  
  pred <- sigmoid(x_tr1.norm%*%beta) # change here
  pred[pred>0.5] <- 1
  pred[pred <=0.5] <- 0
  miss_number <- length(which(pred!=y_tr1))
  
  miss_rate <- append(miss_rate,miss_number/total)
  
  i <- i+1
}

print(miss_rate)
plot(miss_rate,type="b")



########SET2
x_tr2 <- subset(x_label,label==1|label==3|label==4)
x_ts2 <- subset(x_label,label==2)

y_tr2 <- subset(y_label,label==1|label==3|label==4)
y_tr2 <- y_tr2$result
y_ts2 <- subset(y_label,label==2)
y_ts2 <- y_ts2$result

mean.tr <- apply(x_tr2,2,mean)
sd.tr <- apply(x_tr2,2,sd)

x_tr2.norm <- apply(x_tr2,2,z.score)
x_tr2.norm <- cbind(rep(1,nrow(x_tr2.norm)),x_tr2.norm) 
dim(x_tr2.norm)

x_ts2.norm <- vector('numeric')
for (j in c(1:ncol(x_ts2))){
    temp1 <- (x_ts2[,j] -mean.tr[j])/sd.tr[j]
    x_ts2.norm <- cbind(x_ts2.norm,temp1)
}

x_ts2.norm <- cbind(rep(1,nrow(x_ts2.norm)),x_ts2.norm)

x_ts2.norm[1:5,1:5]


beta <- rep(0,rep(ncol(x_tr2.norm))) # assgin original beta as 0s
lamda <- 100 # this is the rate I can change. for miniboone:100
miss_rate <- numeric()

total <- length(y_ts2)


for (i in 1:10){
    beta <- beta +t(x_tr2.norm)%*%y_tr2 - t(x_tr2.norm)%*%sigmoid(x_tr2.norm%*%beta)
    beta[abs(beta) < lamda] <- 0
    beta
    
    pred <- sigmoid(x_ts2.norm%*%beta) # change here
    pred[pred>0.5] <- 1
    pred[pred <=0.5] <- 0
    miss_number <- length(which(pred!=y_ts2))
    
    miss_rate <- append(miss_rate,miss_number/total)
    
    i <- i+1
}

print(miss_rate)
plot(miss_rate,type="b")

########SET3
x_tr3 <- subset(x_label,label==1|label==2|label==4)
x_ts3 <- subset(x_label,label==3)

y_tr3 <- subset(y_label,label==1|label==2|label==4)
y_tr3 <- y_tr3$result
y_ts3 <- subset(y_label,label==3)
y_ts3 <- y_ts3$result

mean.tr <- apply(x_tr3,2,mean)
sd.tr <- apply(x_tr3,2,sd)

x_tr3.norm <- apply(x_tr3,2,z.score)
x_tr3.norm <- cbind(rep(1,nrow(x_tr3.norm)),x_tr3.norm) 
dim(x_tr3.norm)

x_ts3.norm <- vector('numeric')
for (j in c(1:ncol(x_ts3))){
    temp1 <- (x_ts3[,j] -mean.tr[j])/sd.tr[j]
    x_ts3.norm <- cbind(x_ts3.norm,temp1)
}

x_ts3.norm <- cbind(rep(1,nrow(x_ts3.norm)),x_ts3.norm)

x_ts3.norm[1:5,1:5]


beta <- rep(0,rep(ncol(x_tr3.norm))) # assgin original beta as 0s
lamda <- 100 # this is the rate I can change. for miniboone:100
miss_rate <- numeric()

total <- length(y_tr3)


for (i in 1:10){
    beta <- beta +t(x_tr3.norm)%*%y_tr3 - t(x_tr3.norm)%*%sigmoid(x_tr3.norm%*%beta)
    beta[abs(beta) < lamda] <- 0
    beta
    
    pred <- sigmoid(x_tr3.norm%*%beta) # change here
    pred[pred>0.5] <- 1
    pred[pred <=0.5] <- 0
    miss_number <- length(which(pred!=y_tr3))
    
    miss_rate <- append(miss_rate,miss_number/total)
    
    i <- i+1
}

print(miss_rate)
plot(miss_rate,type="b")
################Reg

x <- matrix(rnorm(25,mean=5,sd=2),nrow=5,ncol = 5)


y<- c(1,0,0,1,1)


x <- cbind(rep(1,nrow(x)),x) 
beta <- rep(0,rep(ncol(x)))

k=30

I <- matrix(0,nrow = 6,ncol=6)
diag(I) <- 1
I
(t(x)%*%y)/k^2

beta <- (I -(t(x)%*%x)/k^2)%*%beta +(t(x)%*%y)/k^2

########SET4
x_tr4 <- subset(x_label,label==1|label==2|label==3)
x_ts4 <- subset(x_label,label==4)

y_tr4 <- subset(y_label,label==1|label==2|label==3)
y_tr4 <- y_tr4$result
y_ts4 <- subset(y_label,label==4)
y_ts4 <- y_ts4$result

mean.tr <- apply(x_tr4,2,mean)
sd.tr <- apply(x_tr4,2,sd)

x_tr4.norm <- apply(x_tr4,2,z.score)
x_tr4.norm <- cbind(rep(1,nrow(x_tr4.norm)),x_tr4.norm) 
dim(x_tr4.norm)

x_ts4.norm <- vector('numeric')
for (j in c(1:ncol(x_ts4))){
    temp1 <- (x_ts4[,j] -mean.tr[j])/sd.tr[j]
    x_ts4.norm <- cbind(x_ts4.norm,temp1)
}

x_ts4.norm <- cbind(rep(1,nrow(x_ts4.norm)),x_ts4.norm)

x_ts4.norm[1:5,1:5]


beta <- rep(0,rep(ncol(x_tr4.norm))) # assgin original beta as 0s
lamda <- 100 # this is the rate I can change. for miniboone:100
miss_rate <- numeric()

total <- length(y_ts4)


for (i in 1:10){
    beta <- beta +t(x_tr4.norm)%*%y_tr4 - t(x_tr4.norm)%*%sigmoid(x_tr4.norm%*%beta)
    beta[abs(beta) < lamda] <- 0
    beta
    
    pred <- sigmoid(x_ts4.norm%*%beta) # change here
    pred[pred>0.5] <- 1
    pred[pred <=0.5] <- 0
    miss_number <- length(which(pred!=y_ts4))
    
    miss_rate <- append(miss_rate,miss_number/total)
    
    i <- i+1
}

print(miss_rate)
plot(miss_rate,type="b")

#########Hill_valey_orignial data

setwd("/home/timedreamer/Dropbox/Course/STA5635/BInaryClassificationDatasets/hill-valley")

hill_tr <- read.delim("X.dat",sep=" ",header=F)
result_tr <- read.delim("Y.dat",header=F)
colnames(result_tr) <- "result"

hill_ts <- read.delim("Xtest.dat",sep=" ",header=F)
result_ts <- read.delim("Ytest.dat",header=F)
colnames(result_ts) <- "result"

