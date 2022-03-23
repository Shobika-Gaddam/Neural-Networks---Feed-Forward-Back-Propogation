#Back Propogation
#1
TKS = c(20,10,30,20,80,30)
CSS= c(90,20,40,50,50,80)
Placed = c(1,0,0,0,1,1)
df = data.frame(TKS,CSS,Placed)

require(neuralnet)
library(neural_net)
nn5 = neuralnet(Placed~TKS+CSS,data=df,rep = 5, 
                act.fct="logistic",linear.output = FALSE)
plot(nn5)

nn_backprop1 <- neuralnet(Placed~TKS+CSS,
                          data=df,rep = 5, algorithm="backprop",learningrate=0.0001)
plot(nn_backprop1)

nn6 <- neuralnet(Placed~TKS+CSS,data=df,
                 rep = 5, act.fct="logistic",linear.output = FALSE)
plot(nn6)

#2
TKS = c(20,10,30,20,80,30)
CSS= c(90,20,40,50,50,80)
Placed = c(1,0,0,0,1,1)
df = data.frame(TKS,CSS,Placed)

require(neuralnet)
library(neural_net)
nn7 = neuralnet(Placed~TKS+CSS,data=df, rep = 10, hidden=c(3,1),act.fct="logistic",linear.output = FALSE)
plot(nn)

nn_backprop2 <- neuralnet(Placed~TKS+CSS,data=df, rep = 10,hidden = c(3,1),algorithm="backprop",learningrate=0.0001)
plot(nn_backprop2)

nn8 <- neuralnet(Placed~TKS+CSS,data=df, rep = 10, hidden=c(3,1),act.fct="logistic",linear.output = FALSE)
plot(nn)

#3
library(neuralnet)
df <- read.csv('C:/Users/shobi/OneDrive/Desktop/Woxsen/Term 3/Deep Learning in R/dividendinfo.csv')
attach(df)
str(df)

scaleddata <- scale(df)
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
maxmindf <- as.data.frame(lapply(df,normalize))

trainset <- maxmindf[1:150, ]
testset <- maxmindf[151:200, ]

nn9 <- neuralnet(dividend~fcfps+earnings_growth+de+mcap+current_ratio, data = df, linear.output = FALSE, threshold = 0.01)
plot(nn)

nn_backprop3 <- neuralnet(dividend~fcfps+earnings_growth+de+mcap+current_ratio,data=df,algorithm="backprop",learningrate=0.0001)
plot(nn_backprop3)

nn10 <- neuralnet(dividend~fcfps+earnings_growth+de+mcap+current_ratio,data=df,act.fct="logistic",linear.output = FALSE)
plot(nn)

nn9 <- neuralnet(dividend~fcfps+earnings_growth+de+mcap+current_ratio, data = df, linear.output = FALSE, threshold = 0.01)
plot(nn9)

nn9 <- neuralnet(dividend~fcfps+earnings_growth+de+mcap+current_ratio,data=df,algorithm="backprop",learningrate=0.0001)
plot(nn9)

nn9 <- neuralnet(dividend~fcfps+earnings_growth+de+mcap+current_ratio,data=df,act.fct="logistic",linear.output = FALSE)
plot(nn9)

#4
set.seed(500)
library(MASS)
data <- Boston
data
apply(data,2,function(x)sum(is.na(x)))

index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index, ]
test <- data[-index, ]

lm.fit <- glm(medv~.,data = train)
summary(lm.fit)

maxs <- apply(data,2,max)
mins <- apply(data,2,min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs-mins))

train_ <- scaled[index, ]
test_ <- scaled[-index, ]

library(neuralnet)
n <- names(train_)
f <- as.formula(paste("medv~", paste(n[!n %in% "medv"], collapse = " + ")))

nn <- neuralnet(f,
                data = data,
                hidden = 1,
                linear.output = T)
plot(nn)

nn <- neuralnet(f,
                data = data,
                algorithm = "backprop",
                hidden = 1,
                learningrate = 0.0001)
plot(nn)

nn <- neuralnet(f,
                data = data,
                hidden = 1,
                linear.output = T)
plot(nn)