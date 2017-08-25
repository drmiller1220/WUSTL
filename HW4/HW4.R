setwd("C:\\Users\\drmiller1220\\Documents\\GitHub\\WUSTL\\HW4")

library(glmnet)
library(randomForest)

load("C:\\Users\\drmiller1220\\Documents\\GitHub\\WUSTL\\StudentDrinking.RData")

X <- as.data.frame(X)

# Question 1

# OLS
ols_model <- lm(alcohol ~., data = X)
summary(ols_model)

X <- as.matrix(X)

# LASSO
lasso_model <- cv.glmnet(x = X, y = alcohol, alpha = 1, nfolds=10, family="gaussian", type.measure="mse")
lasso_coefs <- coef(lasso_model, s=lasso_model$lambda)

lasso_male <- NULL
for(i in 1:lasso_coefs@Dim[2]){
  lasso_male <- append(lasso_male, lasso_coefs["male",i])
}

# ridge
ridge_model <- cv.glmnet(x = X, y = alcohol, alpha = 0, nfolds=10, family="gaussian", type.measure="mse")
ridge_coefs <- coef(ridge_model, s=ridge_model$lambda)

ridge_male <- NULL
for(i in 1:ridge_coefs@Dim[2]){
  ridge_male <- append(ridge_male, ridge_coefs["male",i])
}

# elastic-net with alpha=0.5
# LASSO and ridge both induce penalties for the coefficients based on model complexity, but the
# penalties take slightly different forms (in LASSO the penalty is the absolute value of each beta,
# while in ridge the penalty is the square of each beta).  Whereas LASSO regression induces sparsity
# in the model, such that regression coefficients "shrink" to 0 if they do not contribute to the
# model, ridge regression does not shrink the coefficients and only helps with variable
# selection.  In elastic-net, we combine both types of penalties, and our selected value of alpha
# indicates the contribution of each penalty to the model.  As alpha increases (decreases), the model
# is more informed by the ridge (LASSO) penalty component.  By selecting alpha=0.5, we are equally
# weighting the penalties.
elasticnet_model <- cv.glmnet(x = X, y = alcohol, alpha = 0.5, nfolds=10, family="gaussian", type.measure="mse")
elasticnet_coefs <- coef(elasticnet_model, s=elasticnet_model$lambda)

eln_male <- NULL
for(i in 1:elasticnet_coefs@Dim[2]){
  eln_male <- append(eln_male, elasticnet_coefs["male",i])
}

layout(matrix(c(1,2,3), nrow=1, byrow=TRUE), 
       heights = c(0.33, 0.34, 0.33))

plot(y=lasso_male, x=lasso_model$lambda, type="l", col="forestgreen", ylab="Male Coef",
     xlab="Lambda", main="LASSO", ylim = c(0, 1.1))
abline(h=ols_model$coefficients["male"])

plot(y=ridge_male, x=ridge_model$lambda, type="l", col="firebrick1", ylab="Male Coef",
     xlab="Lambda", main="Ridge", ylim = c(0, 1.1))
abline(h=ols_model$coefficients["male"])

plot(y=eln_male, x=elasticnet_model$lambda, type="l", col="dodgerblue", ylab="Male Coef",
     xlab="Lambda", main="Elastic Net", ylim = c(0, 1.1))
abline(h=ols_model$coefficients["male"])

# see saved JPEG for image

# In each of the three plots, we plot the coefficient on male (y-axis) against the lambda value,
# which essentially indicates the level of penalty we are assessing on the model (i.e. higher
# values of lambda indicate higher penalties).  In each plot, the coefficient from OLS is indicated
# with a flat black line.  In each plot, we see that the coefficient for male starts at or near
# the OLS estimate, but sharply drops as lambda increases (as the penalty term exerts more influence
# in the model).

##########################################################################################
### Part 2

# separating data set into training and validation sets

validation_x <- X[1:20,]
validation_y <- alcohol[1:20]

training_x <- X[-c(1:20),]
training_y <- alcohol[-c(1:20)]

# for ten-fold cross-validation, we break the data into 10 evenly-sized groups

training_data <- {}
training_y_ord <- NULL

# assigning the training data into folds and storing each data set in a list

assignment <- sample(1:10, dim(training_x)[1], replace = TRUE)
for(i in 1:10){
  selected <- i == assignment
  selected_df <- training_x[selected,]
  selected_v <- training_y[selected]
  selected_df <- cbind(selected_v, selected_df)
  colnames(selected_df)[1] <- "alcohol"
  training_data[[i]] <- selected_df
  training_y_ord <- append(training_y_ord, selected_v)
}

# empty objects to store predictions
ols_predictions <- NULL
lasso_predictions <- NULL
ridge_predictions <- NULL
eln_predictions <- NULL
rf_predictions <- NULL

# 10-fold validation using the 5 methods specified
for(i in 1:10){
  to_train <- training_data[-i] # isolating all data sets to use to train
  training_x <- NULL
  training_y <- NULL
  for(j in 1:length(to_train)){ # unpacking the data sets embedded in the list
    selected <- as.data.frame(to_train[[j]])
    training_y <- append(training_y, selected$alcohol)
    selected_x <- subset(selected, select = -alcohol)
    training_x <- rbind(training_x, selected_x)
  }
  validating_x <- as.data.frame(training_data[[i]])
  validating_y <- validating_x$alcohol
  validating_x <- subset(validating_x, select = -alcohol)
  
  ols_model <- lm(training_y ~., data = training_x)
  ols_preds <- predict(ols_model, newdata = validating_x)
  ols_predictions <- append(ols_predictions, ols_preds)
  
  lasso_model <- cv.glmnet(x = as.matrix(training_x), y = training_y, alpha = 1, nfolds=10, family="gaussian", type.measure="mse")
  lasso_preds <- predict(lasso_model, newx=as.matrix(validating_x), s=lasso_model$lambda.min)
  lasso_predictions <- append(lasso_predictions, lasso_preds)
  
  ridge_model <- cv.glmnet(x = as.matrix(training_x), y = training_y, alpha = 0, nfolds=10, family="gaussian", type.measure="mse")
  ridge_preds <- predict(ridge_model, newx=as.matrix(validating_x), s=ridge_model$lambda.min)
  ridge_predictions <- append(ridge_predictions, ridge_preds)
  
  eln_model <- cv.glmnet(x = as.matrix(training_x), y = training_y, alpha = 0.5, nfolds=10, family="gaussian", type.measure="mse")
  eln_preds <- predict(eln_model, newx=as.matrix(validating_x), s=ridge_model$lambda.min)
  eln_predictions <- append(eln_predictions, eln_preds)
  
  rf_model <- randomForest(x = training_x, y = training_y)
  rf_preds <- predict(rf_model, newdata=validating_x)
  rf_predictions <- append(rf_predictions, rf_preds)
}

w <- lm(training_y_ord ~ ols_predictions + lasso_predictions + ridge_predictions + eln_predictions +
          rf_predictions -1) # regressing predictions on true Y values to obtain weights
w <- coef(w)

# respecifying validation and training data in original form

validation_x <- X[1:20,] 
validation_y <- alcohol[1:20]

training_x <- X[-c(1:20),]
training_y <- alcohol[-c(1:20)]

# obtaining predictions using the validation data

lm_all <- lm(training_y ~., data = as.data.frame(training_x))
lm_preds_all <- predict(lm_all, newdata = as.data.frame(validation_x))

lasso_all <- cv.glmnet(x = as.matrix(training_x), y = training_y, alpha = 1, nfolds=10, family="gaussian", type.measure="mse")
lasso_preds_all <- predict(lasso_all, newx=as.matrix(validation_x), s=lasso_all$lambda.min)

ridge_all <- cv.glmnet(x = as.matrix(training_x), y = training_y, alpha = 0, nfolds=10, family="gaussian", type.measure="mse")
ridge_preds_all <- predict(ridge_all, newx=as.matrix(validation_x), s=ridge_all$lambda.min)

eln_all <- cv.glmnet(x = as.matrix(training_x), y = training_y, alpha = 0.5, nfolds=10, family="gaussian", type.measure="mse")
eln_preds_all <- predict(eln_all, newx=as.matrix(validation_x), s=eln_all$lambda.min)

rf_all <- randomForest(x = training_x, y = training_y)
rf_preds_all <- predict(rf_model, newdata=validation_x)

# obtaining weighted and unweighted predictions

all_predict <- cbind(lm_preds_all, lasso_preds_all, ridge_preds_all, eln_preds_all, rf_preds_all)
unweighted_predict <- rowMeans(as.matrix(all_predict))
weighted_predict <- NULL
for(i in 1:dim(all_predict)[1]){
  weighted_prediction <- all_predict[i,] %*% w
  weighted_predict <- append(weighted_predict, weighted_prediction)
}

all_predict <- cbind(all_predict, unweighted_predict, weighted_predict)
cor(all_predict)

# the correlations among predictions are all fairly high (the lowest correlation in the sample
# I am looking at it 0.76, and most are above 0.95).  The method with the lowest correlations
# is the random forest model, as it is the only method with correlations below 0.92 (5 of the 6
# comparisons are).  

# measuring average absolute difference for each model

aad <- function(true_y, pred_y){
  loss <- sum(abs(true_y-pred_y)/length(pred_y))
  return(loss)
}

aad(validation_y, all_predict[,1]) # OLS
aad(validation_y, all_predict[,2]) # lasso
aad(validation_y, all_predict[,3]) # ridge
aad(validation_y, all_predict[,4]) # elastic net
aad(validation_y, all_predict[,5]) # random forest
aad(validation_y, all_predict[,6]) # unweighted ensemble
aad(validation_y, all_predict[,7]) # weighted ensemble

# measured by average absolute difference, the weighted ensemble method performs best (smallest
# value) and the random forest performs worst (largest value)