#McCulloch-Pitts Networks
#Single Layer Neural: Hidden = 3
#Multi Layer Neural: Hidden = c(4,2)
#the mathematical propogations are very difficult
#Feed-forward Model
require(neuralnet)
library(neuralnet)
data <- read.csv('C:/Users/shobi/OneDrive/Desktop/Woxsen/Term 3/Deep Learning in R/dividendinfo.csv')
attach(data)
str(data)

scaleddata<-scale(data)
normalize<- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
maxmindata<-as.data.frame(lapply(data,normalize))

trainset<- maxmindata[1:150, ]
testset<- maxmindata[151:200, ]

library(neuralnet)
nn <- neuralnet(dividend~fcfps+earnings_growth+de+mcap+current_ratio, data = trainset, hidden=10000, linear.output = FALSE, threshold = 0.01)
nn$result.matrix
plot(nn)

nn$result.matrix