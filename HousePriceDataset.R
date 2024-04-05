setwd("C:\\Temp\\Statistics")
install.packages("Amelia")
install.packages("randomForest")
library(caTools)
library(randomForest)
library(gbm)
library(ggplot2)
library(Amelia)
library(corrplot)
library(mgcv)
library(ggplot2)
library(tidyr)
library(magrittr)
library(dplyr)
library(splines)
kc.dataset = read.csv("./KingCounty/kc_house_data.csv")
summary(kc.dataset)
kc.dataset <- kc.dataset[, c(3:21)] # remove date and ID columns
summary(kc.dataset)

missmap(kc.dataset,col=c('yellow','black'),y.at=1,y.labels='',legend=TRUE)

corrplot(cor(kc.dataset), method = "number", type = "upper", diag = FALSE)


kc.dataset %>%
  gather(key, val, -price) %>%
  ggplot(aes(x = val, y = price)) +
  geom_point() +
  stat_smooth(method = "lm", se = TRUE, col = "blue") +
  facet_wrap(~key, scales = "free") +
  theme_gray() +
  ggtitle("Scatter plot of variables vs Price")

set.seed(4)
idx <- sample(nrow(kc.dataset), nrow(kc.dataset) * 0.80)
train_data <- kc.dataset[idx, ]
test_data <- kc.dataset[-idx, ]

linear_regression <- lm(price ~ ., data = train_data)
summary(linear_regression)

model_ns <- gam(price ~ ns(sqft_living) + ns(bedrooms) + ns(bathrooms)  + condition + floors + ns(grade) + ns(lat) + ns(long)+ ns(sqft_living15) + ns(sqft_above)+ ns(sqft_basement) + ns(sqft_lot)+ ns(sqft_lot15)+ view+ waterfront+ s(yr_built)+ yr_renovated+ ns(zipcode), data = train_data)
summary(model_ns)

model <- gam(price ~ s(sqft_living) + s(bedrooms) + s(bathrooms)  + condition + floors + s(grade) + s(lat) + s(long)+ s(sqft_living15) + s(sqft_above)+ s(sqft_basement) + s(sqft_lot)+ s(sqft_lot15)+ view+ waterfront+ s(yr_built)+ yr_renovated+ s(zipcode), data = train_data)
summary(model)
prediction = predict(model, newdata = test_data)
mse <- mean((test_data$price - prediction)^2)


ggplot() + 
  geom_point(aes(x = test_data$price, y = prediction)) +
  geom_abline(col="red")+
  labs(title = "Actual Price vs. GAM Predicted Price") +
  labs(x="Actual Price", y="GAM predicted Price")


split <- sample.split(kc.dataset, SplitRatio = 0.8)
training.set <- subset(kc.dataset, split == TRUE)
test.set <- subset(kc.dataset, split == FALSE)

# Bagging


bagging.kingcounty = randomForest(price ~. , data=training.set,mtry= ncol(training.set)-1, maxnodes=8,importance=TRUE)
plot(bagging.kingcounty)

bagging.kingcounty

bagging.predict <- predict(bagging.kingcounty, newdata = test.set)
ggplot() + 
  geom_point(aes(x = test.set$price, y = bagging.predict)) +
  geom_abline(col="red")+
  labs(title = "Actual Price vs. Bagging Predicted Price") +
  labs(x="Actual Price", y="Bagging predicted Price")



mean((bagging.predict - test.set$price)^2)


## Boosting
library(gbm)
# optimal_trees and min_rmse will be added later
parameters.grid<-expand.grid(
  shrinkage = c(.01, .1, .2),
  interaction.depth = c(2, 4, 6),
  n.minobsinnode = c(5, 10, 15),
  minimum.RMSE = 0,
  optimal.no.of.trees = 0
)
parameters.grid
for(i in 1:nrow(parameters.grid)) {
  
  set.seed(42)
  gbm.model.kc <- gbm(
    formula = price ~ .,
    distribution = "gaussian",
    data = training.set,
    n.trees = 2000,
    interaction.depth = parameters.grid$interaction.depth[i],
    shrinkage = parameters.grid$shrinkage[i],
    n.minobsinnode = parameters.grid$n.minobsinnode[i],
    train.fraction = .80)
  
  parameters.grid$minimum.RMSE[i] <- sqrt(min(gbm.model.kc$valid.error)) 
  parameters.grid$optimal.no.of.trees[i] <- which.min(gbm.model.kc$valid.error)
}

sorted_grid <- parameters.grid[order(parameters.grid$minimum.RMSE, decreasing = FALSE), ]
top_10_rows <- sorted_grid[1:10, ]
top_10_rows


parameters.grid<-expand.grid(
  shrinkage = c(.05, .1, .15),
  interaction.depth = c(4,6,8),
  n.minobsinnode = c(10, 13, 15), 
  minimum.RMSE = 0,
  optimal.no.of.trees = 0
)
parameters.grid


for(i in 1:nrow(parameters.grid)) {
  
  set.seed(42)
  gbm.model.kc <- gbm(
    formula = price ~ .,
    distribution = "gaussian",
    data = training.set,
    n.trees = 2000,
    interaction.depth = parameters.grid$interaction.depth[i],
    shrinkage = parameters.grid$shrinkage[i],
    n.minobsinnode = parameters.grid$n.minobsinnode[i],
    train.fraction = .80)
  
  parameters.grid$minimum.RMSE[i] <- sqrt(min(gbm.model.kc$valid.error))
  parameters.grid$optimal.no.of.trees[i] <- which.min(gbm.model.kc$valid.error)
}

sorted_grid <- parameters.grid[order(parameters.grid$minimum.RMSE, decreasing = FALSE), ]
top_10_rows <- sorted_grid[1:10, ]
top_10_rows


gbm.final.model <- gbm(
  formula = price ~ .,
  distribution = "gaussian",
  data = training.set,
  n.trees = 1500,
  interaction.depth = 8,
  shrinkage = 0.05,
  n.minobsinnode = 15,
  train.fraction = .80)

summary(gbm.final.model)
gbm.final.model

yhat.boost <- predict(gbm.final.model, test.set, n.trees = 1500)

boost.residuals = test.set$price - yhat.boost
boost.RMSE = sqrt(mean(boost.residuals^2))
test_mean = mean(test.set$price)
SSTot =  sum((test.set$price - test_mean)^2 )
SSRes =  sum(boost.residuals^2)
boost.rsquared  =  1 - (SSRes/SSTot)
round(boost.rsquared,3)


ggplot() + 
  geom_point(aes(x = test.set$price, y = yhat.boost)) +
  geom_abline(col = "red")+
  labs(title= "Actual Price vs. Boosting Predicted Price",x="Actual Price", y="Boosting Predicted Price")




results<-cbind(yhat.boost,test.set$price)
colnames(results)<-c('predicted','actual')
results<-as.data.frame(results)
head(results)

set.seed(42)
library(magrittr)
library(dplyr)
kc.pca <- prcomp(kc.dataset %>% select(-price), center = TRUE, scale= TRUE)
summary(kc.pca)

pca.df.kc <- data.frame(kc.pca$x[,1:12], price = kc.dataset$price)
pca.df.kc

split.pca <- sample.split(pca.df.kc, SplitRatio = 0.8)
pca.training.set <- subset(pca.df.kc, split == TRUE)
pca.test.set <- subset(pca.df.kc, split == FALSE)

lm_model <- lm(price ~ ., data = pca.training.set)
summary(lm_model)

pred <- predict(lm_model, newdata =  pca.test.set)
results<-cbind(pred,pca.test.set$price)
colnames(results)<-c('predicted','actual')
results<-as.data.frame(results)
head(results)

ggplot() + 
  geom_point(aes(x = pca.test.set$price, y = pred)) +
  geom_abline(col="red")+
  labs(title = "Plot of Actual Price vs. PCA Predicted Price") +
  labs(x="Actual Price", y="PCA Predicted Price")





remove.packages("ggplot2")
install.packages('ggplot2', dependencies = TRUE)
install.packages("ggmap")
install.packages("osmdata")
install.packages("pacman")
install.packages("tidyverse")
install.packages("sf")
library(ggmap)
library(sf)
library(tidyverse)
library(osmdata)
library(ggplot2)
bb <- getbb("king county,washington")
bb

moreBedrooms <- kc.dataset[kc.dataset$bedrooms > 5,]
moreBedrooms <- moreBedrooms[,c(1,16,17)]
mostExpensiveHouses <- kc.dataset[kc.dataset[,1] > 1500000,]
mostExpensiveHouses <- mostExpensiveHouses[,c(1,16,17)]

map <- get_map(location = c(left = -122.54315,
                            bottom = 47.4,
                            right = -121.6571,
                            top = 47.78033),
               zoom = 13, source = "stamen")
ggmap(map) + geom_point(data = moreBedrooms, 
                        aes(x = moreBedrooms$long,
                            y = moreBedrooms$lat,
                            alpha = 1),
                        color = "black",
                        size = 0.8)  + geom_point(data = mostExpensiveHouses, 
                                                  aes(x = mostExpensiveHouses$long,
                                                      y = mostExpensiveHouses$lat,
                                                      alpha = 1),
                                                  color = "red",
                                                  size = 1.2) 
