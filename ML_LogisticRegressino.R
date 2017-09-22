setwd("/home/timedreamer/Dropbox/Course/STA5635/BInaryClassificationDatasets/hill-valley")
install.packages('ggplot2', repos='http://cran.us.r-project.org')
library(ggplot2)
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

num.iterations <- 1000

# Download South African heart disease data
x <- avg_total_tr
y <- result_tr$result

rm(avg_total_tr,hill_tr,rank_avg_tr,result_tr)
rm(avg1,avg2,avg3,avg4,avg5,avg6,avg7,avg8,avg9,avg10)
# Function to standardize input values
zscore <- function(x, mean.val=NA) {
    if(is.matrix(x)) return(apply(x, 2, zscore, mean.val=mean.val))
    if(is.data.frame(x)) return(data.frame(apply(x, 2, zscore, mean.val=mean.val)))
    if(is.na(mean.val)) mean.val <- mean(x)
    sd.val <- sd(x)
    if(all(sd.val == 0)) return(x) # if all the values are the same
    (x - mean.val) / sd.val 
}

# Standardize the features
x.scaled <- zscore(x)

# Gradient descent function
grad <- function(x, y, theta) {
    gradient <- (1 / nrow(y)) * (t(x) %*% (1/(1 + exp(-x %*% t(theta))) - y))
    return(t(gradient))
}

gradient.descent <- function(x, y, lamda = 0.1, alpha=0.1, num.iterations=5000, threshold=1e-5, output.path=FALSE) {
    
    # Add x_0 = 1 as the first column
    m <- if(is.vector(x)) length(x) else nrow(x)
    if(is.vector(x) || (!all(x[,1] == 1))) x <- cbind(rep(1, m), x)
    if(is.vector(y)) y <- matrix(y)
    x <- apply(x, 2, as.numeric)
    
    num.features <- ncol(x)
    
    # Initialize the parameters
    theta <- matrix(rep(0, num.features), nrow=1)
    
    # Look at the values over each iteration
    theta.path <- theta
    for (i in 1:num.iterations) {
        theta <- theta - alpha*lamda*theta -alpha * grad(x, y, theta)
        if(all(is.na(theta))) break
        theta.path <- rbind(theta.path, theta)
        if(i > 2) if(all(abs(theta - theta.path[i-1,]) < threshold)) break 
    }
    
    if(output.path) return(theta.path) else return(theta.path[nrow(theta.path),])
    
}



num.iterations=500

x.scaled <- cbind(rep(1, nrow(x)), x.scaled)
x.scaled<- apply(x.scaled, 2, as.numeric)
##############################################################################
#Sigmoid function
sigmoid <- function(z)
{
    g <- 1/(1+exp(-z))
    return(g)
}

#Cost Function
cost <- function(theta)
{
    m <- nrow(x.scaled)
    g <- sigmoid(x.scaled%*%theta)
    J <- (1/m)*sum((-y*log(g)) - ((1-y)*log(1-g)))
    return(J)
}

initial_theta <- rep(0,ncol(x))
cost(scaled.theta[68,])

cost_total <- numeric()
for (i in 1:nrow(scaled.theta)){
    cost_total <- append(cost_total,cost(scaled.theta[i,]))
}
plot(cost_total)

grad <- function(x, y, theta) {
    gradient <- (1 / nrow(y)) * (t(x) %*% (1/(1 + exp(-x %*% t(theta))) - y))
    return(t(gradient))
}

alpha=0.1
theta <- theta - alpha * grad(x1, y, theta)


plot(1:(nrow(scaled.theta)), scaled.theta[,1],xlab="iterations",ylab="theta_1")

# Look at output for various different alpha values
vary.alpha <- lapply(c(1e-12, 1e-9, 1e-7, 1e-3, 0.1, 0.9), function(alpha) gradient.descent(x=x, y=y, alpha=alpha, num.iterations=num.iterations, output.path=TRUE))

par(mfrow = c(2, 3))
for (j in 1:6) {
    plot(vary.alpha[[j]][,1], ylab="area", xlab="iteration")
}

initial_theta <- rep(0,ncol(x.scaled))

theta_optim <- optim(par=initial_theta,fn=cost)

#set theta
theta <- theta_optim$par

#cost at optimal value of the theta
theta_optim$value
