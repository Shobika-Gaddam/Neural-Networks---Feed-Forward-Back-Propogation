set.seed(8)
library(neuralnet)
data <- read.csv('C:/Users/shobi/OneDrive/Desktop/Woxsen/Term 3/Deep Learning in R/Set5- Q2.csv')
data
apply(data,2,function(x)sum(is.na(x)))

index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index, ]
test <- data[-index, ]

lm.fit <- glm(totalrunscored~.,data = train)
summary(lm.fit)

maxs <- apply(data,2,max)
mins <- apply(data,2,min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs-mins))

train_ <- scaled[index, ]
test_ <- scaled[-index, ]

n <- names(train_)
f <- as.formula(paste("totalrunscored~", paste(n[!n %in% "totalrunscored"], collapse = " + ")))

nn1 <- neuralnet(f,
                data = data,
                hidden = 1,
                linear.output = FALSE)
plot(nn1)

nn2 <- neuralnet(f,
                data = data,
                algorithm = "backprop",
                hidden = 1,
                learningrate = 0.0001)
plot(nn)

nn <- neuralnet(f,
                data = data,
                hidden = 1,
                linear.output = FALSE)
plot(nn)






library(neuralnet)

df1 <- read.csv('C:/Users/shobi/OneDrive/Desktop/Woxsen/Term 3/Deep Learning in R/Set5- Q2.csv')
attach(df1)
str(df1)

scaleddata <- scale(df)
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
maxmindf <- as.data.frame(lapply(df,normalize))

trainset <- maxmindf[1:150, ]
testset <- maxmindf[151:200, ]

nn8 <- neuralnet(totalrunscored~extraruns+runsbybatsman+batsman1runs+batsman1playedballs
                 +batsman2runs+batsman2playedballs, 
                 data = df, linear.output = FALSE, 
                 threshold = 0.01, stepmax = 100)
plot(nn8)

nn8= neuralnet(totalrunscored~extraruns+runsbybatsman+batsman1runs+
                 batsman1playedballs+batsman2runs+batsman2playedballs,
               data=df,algorithm="backprop",learningrate=0.0001)
plot(nn8)

nn6= neuralnet(totalrunscored~extraruns+runsbybatsman+batsman1runs+
                 batsman1playedballs+batsman2runs+batsman2playedballs,
               data=df,act.fct="logistic",linear.output = FALSE)
plot(nn6)

