#final project stats
library(dplyr)
library(ggplot2)
hd <- read.csv(file.choose(), header = TRUE)
head(hd)

hdT <- read.csv(file.choose(), header = TRUE)
head(hdT)

summary(hd)
# filtered the neighborhood to only have the values that Century 21 Ames handles
hd2 <- hd %>% filter(Neighborhood == "NAmes" | Neighborhood == "Edwards" | Neighborhood == "BrkSide")
head(hd2)

summary(hd2$Neighborhood)
hd2$Neighborhood

hdT2 <- hdT %>% filter(Neighborhood == "NAmes" | Neighborhood == "Edwards" | Neighborhood == "BrkSide")
head(hdT2)
# filtered out just the columns i wanted from the training set & test set

Columns_to_keep <- c("SalePrice", "Id", "LotArea", "Neighborhood","YearBuilt", "GrLivArea")
Columns_to_keep2 <- c("Id", "LotArea", "Neighborhood","YearBuilt", "GrLivArea")

hdT2Updated <- select(hdT2, Columns_to_keep)
head(hdT2Updated)
hd2Updated <- select(hd2, Columns_to_keep2)
head(hd2Updated)
head(hdT2Updated)

# plot of living area v sale price
plot(hdT2Updated$GrLivArea, hdT2Updated$SalePrice,
     xlab = "living area in sqft", ylab = "Sale Price")
plot(hd2Updated)

# Create a scatterplot of Living area v Sale Price
hdT2Updated %>% ggplot(aes(x = GrLivArea, y = SalePrice, color = Neighborhood)) +
  geom_point()  + geom_smooth(method = "lm", color = "red") +
  labs(x = "Living area in sqft", y = "Sale Price", title = "Scatterplot of Living area v Sale Price")

# multiple linear regression comparing the sq living area to the sale price & neighborhood
fit <- lm(SalePrice~GrLivArea + Neighborhood, data = hdT2Updated)
summary(fit)
par(mfrow=c(2,2))
plot(fit)

confint(fit)
#first try the adjusted r squared was pretty low will continue and try to 
#have a better fitting model

# Create a scatterplot of Living area v Sale Price - taking log of x value
hdT2Updated %>% ggplot(aes(x = log(GrLivArea), y = SalePrice, color = Neighborhood)) +
  geom_point()  + geom_smooth(method = "lm", color = "red") +
  labs(x = "Living area in sqft", y = "Sale Price", title = "Scatterplot of Living area v Sale Price")

# Create a scatterplot of Living area v Sale Price - taking log of y value
hdT2Updated %>% ggplot(aes(x = GrLivArea, y = log(SalePrice))) +
  geom_point()  + geom_smooth(method = "lm", color = "red") +
  labs(x = "Living area in sqft", y = "Sale Price", title = "Scatterplot of Living area v Sale Price")

# Create a scatterplot of Living area v Sale Price - taking log of both value
hdT2Updated %>% ggplot(aes(x = log(GrLivArea), y = log(SalePrice))) +
  geom_point()  + geom_smooth(method = "lm", color = "red") +
  labs(x = "Living area in sqft", y = "Sale Price", title = "Scatterplot of Living area v Sale Price")
# fitting the model with log of living area
fit2 <- lm(SalePrice~log(GrLivArea) + Neighborhood, data = hdT2Updated)
summary(fit2)

fit3 <- lm(log(SalePrice)~log(GrLivArea) + Neighborhood, data = hdT2Updated)
summary(fit3)
par(mfrow=c(2,2))

plot(fit3)

#applying predictions to the other dataset
predictions <- predict(fit, newdata = hd2Updated)

#adding predictions to the data set
hd2Updated$SalePrice <- predictions

head(hd2Updated)

# Create a scatterplot of Living area v Sale Price - taking log of x value
hd2Updated %>% ggplot(aes(x = log(GrLivArea), y = SalePrice)) +
  geom_point()   + geom_smooth(method = "lm", color = "red") +
  labs(x = "Living area in sqft", y = "Sale Price", title = "Scatterplot of Living area v Sale Price")

# Create a scatterplot of Living area v Sale Price - taking log of x value
hd2Updated %>% ggplot(aes(x = GrLivArea, y = SalePrice)) +
  geom_point()   + geom_smooth(method = "lm", color = "red") +
  labs(x = "Living area in sqft", y = "Sale Price", title = "Scatterplot of Living area v Sale Price")


#checking for any na values in the test set
sum(is.na(hdT2Updated$SalePrice))
sum(is.na(hdT2Updated$Id))
sum(is.na(hdT2Updated$LotArea))
sum(is.na(hdT2Updated$Neighborhood))
sum(is.na(hdT2Updated$YearBuilt))
sum(is.na(hdT2Updated$GrLivArea))
#checking for na values in the train set
sum(is.na(hd2Updated$SalePrice))
sum(is.na(hd2Updated$Id))
sum(is.na(hd2Updated$LotArea))
sum(is.na(hd2Updated$Neighborhood))
sum(is.na(hd2Updated$YearBuilt))
sum(is.na(hd2Updated$GrLivArea))


cor.test(hdT2Updated$SalePrice,hdT2Updated$GrLivArea)

#run cooks dtest and dfbetas and dffits test to look at outlying points

#cooks dtest
c_dist <- cooks.distance(fit)

# Identify influential observations based on a threshold (e.g., 4/n, where n is the number of observations)
influential_observations <- which(c_dist > 4/length(c_dist))

# Display Cook's distance values and influential observations
print(c_dist)
print(influential_observations)

# Plot Cook's distance
plot(c_dist, pch = 20, main = "Cook's Distance Plot", ylab = "Cook's Distance", xlab = "Observation")
abline(h = 4/length(c_dist), col = 'red', lty = 2)  # Threshold line

# Identify the observation with the highest Cook's distance
max_cooks_index <- which.max(c_dist)
max_cooks_index

#printing the observation with the max cook distance 
hdT2Updated[339,]

# Identify the indices of the top 3 observations with the highest Cook's distance
top_indices <- order(c_dist, decreasing = TRUE)[1:3]
top_indices

#printing the top 3 
hdT2Updated[339,]
hdT2Updated[131,]
hdT2Updated[169,]

#identifying the cooks distance rating for the top 3 points
c_dist[339]
c_dist[131]
c_dist[169]

#removing points 339 & 131 both are in the Edwards neighborhood so shows some similarity
#in the data points also these points have an abnormally low sale price for the grlivarea
#removing them to test how the model performs w/o these points
rr <- c(131,339)
dfTest <- hdT2Updated[-rr,]
summary(dfTest)
dfTest[339,]
dfTest[131,]
head(dfTest)
# Create a scatterplot of Living area v Sale Price on new data set
dfTest %>% ggplot(aes(x = GrLivArea, y = SalePrice, color = Neighborhood)) +
  geom_point()  + geom_smooth(method = "lm", color = "red") +
  labs(x = "Living area in sqft", y = "Sale Price", title = "Scatterplot of Living area v Sale Price")

# Create a scatterplot of Living area v Sale Price - taking log of x value - new data set
dfTest %>% ggplot(aes(x = GrLivArea, y = log(SalePrice), color = Neighborhood)) +
  geom_point()   + geom_smooth(method = "lm", color = "red") +
  labs(x = "Living area in sqft", y = "Sale Price", title = "Scatterplot of Living area v Sale Price")

#fitting multiple models to the new data set
# multiple linear regression comparing the sq living area to the sale price & neighborhood
fit4 <- lm(SalePrice~GrLivArea + Neighborhood, data = dfTest)
summary(fit4)
plot(fit4)

fit5 <- lm(SalePrice~log(GrLivArea) + Neighborhood, data = dfTest)
summary(fit5)
plot(fit5)

fit6 <- lm(log(SalePrice)~log(GrLivArea) + Neighborhood, data = dfTest)
summary(fit6)
plot(fit6)

model <- lm(log(SalePrice)~GrLivArea + Neighborhood, data = dfTest)
summary(model)
plot(model)

#fitting the new linear model to the test adding the values to salesprice2
#applying predictions to the other dataset
predictions <- predict(fit6, newdata = hd2Updated)

#adding predictions to the data set
hd2Updated$SalePrice2 <- predictions

head(hd2Updated)

# Create a scatterplot of Living area v Sale Price2 
hd2Updated %>% ggplot(aes(x = GrLivArea, y = SalePrice2, color = Neighborhood)) +
  geom_point()  + geom_smooth(method = "lm", color = "red") +
  labs(x = "Living area in sqft", y = "Sale Price", title = "Scatterplot of Living area v Sale Price")

# Create a scatterplot of Living area v Sale Price2 - taking log of x
hd2Updated %>% ggplot(aes(x = log(GrLivArea), y = SalePrice2, color = Neighborhood)) +
  geom_point()   + geom_smooth(method = "lm", color = "red") +
  labs(x = "Living area in sqft", y = "Sale Price", title = "Scatterplot of Living area v Sale Price")

#trying new transformations since the log transformations didn't appear to work out well
#& just removing the data points didn't help with the model either

#sqrt transformation
hdT2Updated$GrLivAreaSqrt <- sqrt(hdT2Updated$GrLivArea)
head(hdT2Updated)

dfTest$GrLivAreaSqrt <- sqrt(dfTest$GrLivArea)
head(dfTest)

# Create a scatterplot of Living area v Sale Price on new data set
dfTest %>% ggplot(aes(x = GrLivAreaSqrt, y = SalePrice, color = Neighborhood)) +
  geom_point()  + geom_smooth(method = "lm", color = "red") +
  labs(x = "Living area in sqft", y = "Sale Price", title = "Scatterplot of Living area v Sale Price")

fitnew <- lm(SalePrice~GrLivAreaSqrt + Neighborhood, data = dfTest)
summary(fitnew)
plot(fitnew)

dfTest %>% ggplot(aes(x = GrLivAreaSqrt, y = SalePrice, color = Neighborhood)) +
  geom_point()  + geom_smooth(method = "lm", color = "red") +
  labs(x = "Living area in sqft", y = "Sale Price", title = "Scatterplot of Living area v Sale Price")

dfTest %>% ggplot(aes(x = sqrt(GrLivArea), y = sqrt(SalePrice), color = Neighborhood)) +
  geom_point()  + geom_smooth(method = "lm", color = "red") +
  labs(x = "Living area in sqft", y = "Sale Price", title = "Scatterplot of Living area v Sale Price")
dfTest[190,]
dfTest[169,]
# set a parameter of 2500 for the grlivarea bc it looks like the normality
#falls a good amount indicating that perhaps the information above that thershold isn't reliable
thershold_value <- 2500

new_df <- hdT2Updated[hdT2Updated$GrLivArea <= thershold_value,]

new_df %>% ggplot(aes(x = GrLivArea, y = SalePrice, color = Neighborhood)) +
  geom_point()  + geom_smooth(method = "lm", color = "red") +
  labs(x = "Living area in sqft", y = "Sale Price", title = "Scatterplot of Living area v Sale Price")

fitnew <- lm(SalePrice~GrLivArea + Neighborhood, data = new_df)
summary(fitnew)
plot(fitnew)

predictions <- predict(fitnew, newdata = hd2Updated)

#adding predictions to the data set
hd2Updated$SalePrice2 <- predictions

head(hd2Updated)

# Create a scatterplot of Living area v Sale Price2 
hd2Updated %>% ggplot(aes(x = log(GrLivArea), y = log(SalePrice2), color = Neighborhood)) +
  geom_point()  + geom_smooth(method = "lm", color = "red") +
  labs(x = "Living area in sqft", y = "Sale Price", title = "Scatterplot of Living area v Sale Price")

summary(fitnew)$fstatistic
summary(fitnew)$coefficents
install.packages("car")
library(car)

vif(fitnew)

cooks.distance(fitnew)
plot(fitnew, which = 4)

#creating dummy variables
df_dummies <- model.matrix(~ Neighborhood - 1, data = hdT2Updated)

model <- lm(SalePrice ~ GrLivArea + df_dummies, data = hdT2Updated)
summary(model)
plot(model)

#logging the model
model <- lm(SalePrice ~ GrLivArea + df_dummies, data = hdT2Updated)
summary(model)
plot(model)

model2 <- lm(log(SalePrice) ~ GrLivArea + df_dummies, data = hdT2Updated)
summary(model)
plot(model)










