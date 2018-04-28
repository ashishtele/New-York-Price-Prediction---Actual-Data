
library(xgboost)


# Custom function for R Square
Rsquare <- function(y,y_new)
{
  RSS <- sum((y-y_new)^2)
  TSS <- sum((y-mean(y))^2)
  return(1-RSS/TSS)
}

# setting seed for answer reproducibility
set.seed(131)
# Complete model - linear - 68.87%
linear_lm <- lm(sqrt(sale_price) ~ . ,data = x_train3)
summary(linear_lm)
plot(linear_lm)
prediction<- predict(linear_lm,x_test3, type="response")
sqrt(sum((x_test3$sale_price - (prediction)^2)^2))
Rsquare(x_test3$sale_price,prediction^2)


# Data split 60:40
# Feedback the dataset from cook's distance engineering to improve the model performance
data2 <- data
Ind1 <- createDataPartition(data2$sale_price,p=0.6,list=F)
x_train3 <- data2[Ind1,]
x_test3 <- data2[-Ind1,]

# Checking the cook's distance for outliers
cd <- cooks.distance(linear_lm)
a <- cbind(data2,cd)
a <- a[cd < 4/nrow(data2),]
data2 <- a[,-c("cd")]


# Decision Tree
tree_md <- train(sqrt(sale_price) ~ .,method="rpart" ,data = x_train3)
pred1 <- predict(tree_md,newdata = x_test3)
##summary(tree_md)
sqrt(sum((x_test3$sale_price - (pred1)^2)^2))
Rsquare(x_test3$sale_price,(pred1)^2)


# Random forest - 67.7%
library(randomForest)
rf <- randomForest(sqrt(sale_price)~.,data = x_train3, ntree= 600)
varImpPlot(rf)
pred3 <- predict(rf,newdata = x_test3)
sqrt(sum((x_test3$sale_price - (pred3)^2)^2))
Rsquare(x_test3$sale_price,(pred3)^2)
plot(rf)


#Lasso - #68.80%
set.seed(123) 
lambdas <- seq(1,0,-0.01)
tr.control <- trainControl(method="repeatedcv", number = 10,repeats = 10)
lasso_model <- train(sqrt(sale_price)~., data = x_train3, method = "glmnet", metric = "RMSE"
                     , trControl = tr.control,
                     tuneGrid = expand.grid(alpha=1,
                                            lambda= lambdas))
varImp(lasso_model)
lasso_model
test_pred4 <- predict(lasso_model, newdata = x_test3)
test_rmse <- sqrt(sum((x_test3$sale_price - (test_pred4)^2)^2))
Rsquare(x_test3$sale_price,(test_pred4)^2)
plot(lasso_model)

#Ridge - 68.69#%
set.seed(1234) 
lambdas <- seq(1,0,-0.01)
tr.control <- trainControl(method="repeatedcv", number = 10,repeats = 10)
ridge_model <- train(sqrt(sale_price)~., data = x_train3, method = "glmnet", metric = "RMSE"
                     , trControl = tr.control,
                     tuneGrid = expand.grid(alpha=0,
                                            lambda= 0.02))
varImp(ridge_model)
ridge_model
pred6 <- predict(ridge_model, newdata = x_test3)
ridge_model$results
Rsquare(x_test3$sale_price,(pred6)^2)
plot(ridge_model)



#gbm - 69.14%
library(parallel)

CTRL <- trainControl(method ="cv", number = 5,verboseIter = FALSE, allowParallel = TRUE)
gbmFit <- train(sqrt(sale_price) ~ ., method = "gbm", metric = "RMSE", maximize = FALSE, 
                trControl = CTRL, tuneGrid = expand.grid(n.trees = (4:10) * 
                                                           100, interaction.depth = c(5),
                                                         shrinkage = c(0.05), n.minobsinnode = c(10)), 
                data = x_train3, verbose = FALSE)
summary(gbmFit)
gbmFit$bestTune
gbmFit$results
pred5 <- predict(gbmFit,newdata = x_test3)
Rsquare(x_test3$sale_price,(pred5)^2)
plot(gbmFit)

#xgboost - 70.37%

x_train4 <- x_train3
x_test4 <- x_test3

xgbparams <- list(
  booster = 'gbtree',
  objective = 'reg:linear',
  colsample_bytree=0.2,
  eta=0.01,
  min_child_weight=2,
  max_depth = 4,
  alpha=0.3,
  lambda=0.8,
  gamma=0.01, # less overfit
  subsample=0.8,
  silent=TRUE,
  eval_metrics = 'rmse' 
  
)


set.seed(101)
xgb_cv2 <- xgb.cv(params = xgbparams, data = as.matrix(x_train4[,-5]),
                  label =x_train4$sale_price,nrounds =10000, nfold = 10, stratified =T, print_every_n = 500, verbose = T, showsd = T, prediction = T,
                  early_stopping_rounds = 400, maximize = F)

plot(xgb_cv2)
set.seed(101)
# best rounds 5856
classifier4 <- xgboost(data = as.matrix(x_train4[,-5]), params = xgbparams,
                       label = sqrt(x_train4$sale_price), nrounds = 5856, print_every_n =500,
                       early_stopping_rounds = 100)
predds4 <- predict(classifier4, newdata = as.matrix(x_test4[,-5]))
plot(predds4)
Rsquare(x_test4$sale_price,(predds4)^2)



