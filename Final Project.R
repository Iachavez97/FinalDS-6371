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

# plot of living area v sale price
plot(hdT2Updated$GrLivArea, hdT2Updated$SalePrice,
     xlab = "living area in sqft", ylab = "Sale Price")
plot(hd2Updated)

# Create a scatterplot of Living area v Sale Price
hdT2Updated %>% ggplot(aes(x = GrLivArea, y = SalePrice)) +
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
hdT2Updated %>% ggplot(aes(x = log(GrLivArea), y = SalePrice)) +
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





