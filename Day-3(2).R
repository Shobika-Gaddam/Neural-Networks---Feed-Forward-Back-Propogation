set.seed(100)
library(MASS)
data()
data <- USArrests
data
apply(data,2,function(x)sum(is.na(x)))

index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index, ]
test <- data[-index, ]

lm.fit <- glm(Assault~.,data = train)
summary(lm.fit)

maxs <- apply(data,2,max)
mins <- apply(data,2,min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs-mins))

train_ <- scaled[index, ]
test_ <- scaled[-index, ]

library(neuralnet)
n <- names(train_)
f <- as.formula(paste("Assault~", paste(n[!n %in% "Assault"], collapse = " + ")))

nn <- neuralnet(f,
                data = train_,
                hidden = c(2,1),
                linear.output = T)
plot(nn)
