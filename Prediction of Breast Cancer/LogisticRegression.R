# install.packages("mlbench")
data(BreastCancer, package="mlbench")
bc <- BreastCancer[complete.cases(BreastCancer), ]  # create copy

str(bc)

glm(Class ~ Cell.shape, family="binomial", data = bc)

# remove id column
bc <- bc[,-1]

# convert factors to numeric
for(i in 1:9) {
  bc[, i] <- as.numeric(as.character(bc[, i]))
}

bc$Class <- ifelse(bc$Class == "malignant", 1, 0)
bc$Class <- factor(bc$Class, levels = c(0, 1))

table(bc$Class)

library(caret)
'%ni%' <- Negate('%in%')  # define 'not in' func
options(scipen=999)  # prevents printing scientific notations.

# Prep Training and Test data.
set.seed(100)
trainDataIndex <- createDataPartition(bc$Class, p=0.7, list = F)  # 70% training data
trainData <- bc[trainDataIndex, ]
testData <- bc[-trainDataIndex, ]
table(trainData$Class)

# Down Sample
set.seed(100)
down_train <- downSample(x = trainData[, colnames(trainData) %ni% "Class"],
                         y = trainData$Class)

table(down_train$Class)

# Up Sample.
set.seed(100)
up_train <- upSample(x = trainData[, colnames(trainData) %ni% "Class"],
                     y = trainData$Class)

table(up_train$Class)

# Build Logistic Model
logitmod <- glm(Class ~ Cl.thickness + Cell.size + Cell.shape, family = "binomial", data=down_train)

summary(logitmod)

pred <- predict(logitmod, newdata = testData, type = "response")

y_pred_num <- ifelse(pred > 0.5, 1, 0)
y_pred <- factor(y_pred_num, levels=c(0, 1))
y_act <- testData$Class
mean(y_pred == y_act)  