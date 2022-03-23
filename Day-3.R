set.seed(500)
library(MASS)
data <- Boston
data
apply(data,2,function(x)sum(is.na(x)))

index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index, ]
test <- data[-index, ]

train
test

lm.fit <- glm(medv~.,data = train)
summary(lm.fit)

maxs <- apply(data,2,max)
mins <- apply(data,2,min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs-mins))
#scaling is used to maintain standardization between all the variables

train_ <- scaled[index, ]
test_ <- scaled[-index, ]

maxs
mins

train_
test_

library(neuralnet)
n <- names(train_)
f <- as.formula(paste("medv~", paste(n[!n %in% "medv"], collapse = " + ")))

nn <- neuralnet(f,
                data = train_,
                hidden = c(4,2),
                linear.output = T)
plot(nn)

n
f