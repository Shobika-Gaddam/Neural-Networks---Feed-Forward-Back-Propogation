#Single Layer Perceptron - y=f(w1*x1+w2*x2+b) 
#where b is bias=1; w1 and w2 are the weights and x1 and x2 are the inputs

require(neuralnet)
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

nn <- neuralnet(dividend~fcfps+earnings_growth+de
                +mcap+current_ratio, data = trainset, hidden= 3, linear.output = FALSE, threshold = 0.01)
plot(nn)
nn$result.matrix

temp_test <- subset(testset, select = c("fcfps","earnings_growth","de","mcap","current_ratio"))
head(temp_test)

nn.results <- compute(nn,temp_test)
results <- data.frame(actual = testset$dividend, prediction=nn.results$net.result)
results

fcfps = c(1,2,3)
earnings_growth = c(10,18,25)
de = c(2,3,4)
mcap = c(200,400,600)
current_ratio = c(0,1,2)
test = data.frame(fcfps,earnings_growth,de,mcap,current_ratio)

Predict=compute(nn,test)
Predict$net.result

#The company is able to give dividend