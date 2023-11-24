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






