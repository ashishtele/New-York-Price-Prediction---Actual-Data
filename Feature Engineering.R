## Outlier detection
# Reducing the data size for 'missmap' function (avoid crashing)
missmap(head(Brk_new,1000), main = "Missing Data")


# Removing columns having maximum NAs
# Difficult to impute the values 
Brk_new$tax_class[Brk_new$tax_class == ""]  <- NA
Brk_new$building_class[Brk_new$building_class == ""] <- NA
Brk_new <- Brk_new[!is.na(Brk_new$tax_class)]

# Considering and removing the sale price based on the normal range (mode)
# avoding the shewness based on business understanding
Brk_new_1 <- Brk_new[Brk_new$sale_price<3000000]
Brk_new_1 <- Brk_new_1[Brk_new_1$sale_price>10000]

plot(density((Brk_new_2$sale_price)))

# Non-zero access total value
Brk_new_1 <- Brk_new_1[Brk_new_1$AssessTot>0]
Brk_new_1 <- Brk_new_1[Brk_new_1$AssessTot<100000,]

# Limiting the gross sqft area within limit 
# removing the outliers

plot(density((Brk_new_2$gross_sqft)))
Brk_new_1 <- Brk_new_1[Brk_new_1$gross_sqft>99]
Brk_new_1 <- Brk_new_1[Brk_new_1$gross_sqft<10000]
plot(density((Brk_new_2$gross_sqft)))


# Land area in square feet 
# limiting within feasible range
plot(density((Brk_new$land_sqft)))
Brk_new_1 <- Brk_new_1[Brk_new_1$land_sqft>1000,]
Brk_new_1 <- Brk_new_1[Brk_new_1$land_sqft<7500,]
plot(density((Brk_new_1$land_sqft)))

# Limiting within the range
# positive value of building front
plot(density((Brk_new$BldgFront)))
Brk_new_1 <- Brk_new_1[Brk_new_1$BldgFront<200,]
Brk_new_1 <- Brk_new_1[Brk_new_1$BldgFront>0,]
plot(density((Brk_new_1$BldgFront)))

# Limiting the property unique locations below 1000
Brk_new_1 <- Brk_new_1[Brk_new_1$lot<1000,]
ggplot(data = Brk_new_1, aes(x=as.factor(lot),y=sqrt(sale_price), fill = LotType)) +
  geom_boxplot()

# Limiting community variable ranges
Brk_new_1 <- Brk_new_1[Brk_new_1$residential_units<8,]
Brk_new_1 <- Brk_new_1[Brk_new_1$NumBldgs<4,]
Brk_new_1 <- Brk_new_1[Brk_new_1$NumFloors<5,]
Brk_new_1 <- Brk_new_1[Brk_new_1$UnitsRes<9,]


# Normality check - Dependent variable
library(moments)
plot(density(sqrt(Brk_new_2$sale_price)))
skewness((sqrt(Brk_new_2$sale_price)))
mean((sqrt(Brk_new_2$sale_price)))
sd((sqrt(Brk_new_2$sale_price)))
kurtosis((sqrt(Brk_new_2$sale_price)))

# Saving the cleased the dataset
write.csv(Brk_new_1, file = "E:\\Predective Modeling\\Project\\New_York_1.csv")

###############################################################################
##########################   New feature creation #############################
###############################################################################

# Very few observations with garage area available
# Updating the garage area column with flag
Brk_new_2$garage_present <- ifelse(Brk_new_2$GarageArea > 0,1,0)
Brk_new_2$GarageArea <- NULL

# Creating a flag for year alter (two columns)
Brk_new_2$alter_1 <- ifelse(Brk_new_2$YearAlter1>0,1,0)
Brk_new_2$alter_2 <- ifelse(Brk_new_2$YearAlter2>0,1,0)
# Creating new column, no. of alteractions
Brk_new_2$no_alter <- Brk_new_2$alter_1 + Brk_new_2$alter_2
Brk_new_2$YearAlter1 <- NULL
Brk_new_2$YearAlter2 <- NULL

# Creating the bins based on year built
Brk_new_2$yr_built <- ifelse(Brk_new_2$year_built<1900,3,ifelse(Brk_new_2$year_built<2000,2,1))

###############################################################################
##########################   EDA   ############################################
###############################################################################


# Correlation plot - 1
library(corrplot)

corr <- cor(Brk_new_2[,c("zip_code","lot",    
                         "residential_units","commercial_units", 
                         "land_sqft","gross_sqft", 
                         "year_built","sale_price", 
                         "year_of_sale","SchoolDist", 
                         "Council", "PolicePrct", 
                         "HealthCent","Easements",
                         "NumBldgs",
                         "NumFloors","UnitsRes",
                         "UnitsTotal","BldgFront",
                         "LotType", "BsmtCode",
                         "AssessTot","population","avg_income")])
corrplot(corr, method = "color", outline = T, cl.pos = 'n', rect.col = "black",  tl.col = "indianred4", addCoef.col = "black", number.digits = 2, number.cex = 0.60, tl.cex = 0.7, cl.cex = 1, col = colorRampPalette(c("green4","white","red"))(100))

# Checking the VIF factors to remove variables with high
# collinearity values
library(car)
# High VIF
model1 <- lm(sale_price~UnitsRes+residential_units, data = Brk_new_2)
summary(model1)
vif(model1)

# High VIF
model2 <- lm(sale_price~UnitsTotal+residential_units, data = Brk_new_2)
summary(model2)
vif(model2)

#  VIF < 4
model3 <- lm(sale_price~AssessTot+residential_units, data = Brk_new_2)
summary(model3)
vif(model3)

# High VIF
model4 <- lm(sale_price~gross_sqft+residential_units, data = Brk_new_2)
summary(model4)
vif(model4)

# Removing "Residential units"


# Correlation plot - 2
corr <- cor(Brk_new_2[,c("zip_code","lot","commercial_units", 
                         "land_sqft","gross_sqft", 
                         "year_built","sale_price", 
                         "year_of_sale","SchoolDist", 
                         "Council", "PolicePrct", 
                         "HealthCent","Easements",
                         "NumBldgs",
                         "NumFloors","UnitsRes",
                         "UnitsTotal","BldgFront",
                         "LotType", "BsmtCode",
                         "AssessTot","garage_present","population","avg_income")])
corrplot(corr, method = "color", outline = T, cl.pos = 'n', rect.col = "black",  tl.col = "indianred4", addCoef.col = "black", number.digits = 2, number.cex = 0.60, tl.cex = 0.7, cl.cex = 1, col = colorRampPalette(c("green4","white","red"))(100))


# High VIF
model5 <- lm(sale_price~UnitsRes+UnitsTotal, data = Brk_new_2)
summary(model5)
vif(model5)

# Remove "UnitsRes"

# Correlation plot - 3
corr <- cor(Brk_new_2[,c("zip_code","lot","commercial_units", 
                         "land_sqft","gross_sqft", 
                         "year_built","sale_price", 
                         "year_of_sale","SchoolDist", 
                         "Council", "PolicePrct", 
                         "HealthCent","Easements",
                         "NumBldgs",
                         "NumFloors",
                         "UnitsTotal","BldgFront",
                         "LotType", "BsmtCode",
                         "AssessTot","garage_present","population","avg_income")])
corrplot(corr, method = "color", outline = T, cl.pos = 'n', rect.col = "black",  tl.col = "indianred4", addCoef.col = "black", number.digits = 2, number.cex = 0.60, tl.cex = 0.7, cl.cex = 1, col = colorRampPalette(c("green4","white","red"))(100))

# High VIF
model6 <- lm(sale_price~gross_sqft+UnitsTotal, data = Brk_new_2)
summary(model6)
vif(model6)

# High VIF
model7 <- lm(sale_price~gross_sqft+AssessTot, data = Brk_new_2)
summary(model7)
vif(model7)

# Correlation plot - 4
corr <- cor(Brk_new_2[,c("zip_code","lot","commercial_units", 
                         "land_sqft","gross_sqft", 
                         "year_built","sale_price", 
                         "year_of_sale","SchoolDist", 
                         "Council", "PolicePrct", 
                         "HealthCent","Easements",
                         "NumBldgs",
                         "NumFloors","BldgFront",
                         "LotType", "BsmtCode","garage_present","population","avg_income")])
corrplot(corr, method = "color", outline = T, cl.pos = 'n', rect.col = "black",  tl.col = "indianred4", addCoef.col = "black", number.digits = 2, number.cex = 0.60, tl.cex = 0.7, cl.cex = 1, col = colorRampPalette(c("green4","white","red"))(100))

# High VIF - keep both
model8 <- lm(sale_price~PolicePrct+Council, data = Brk_new_2)
summary(model8)
vif(model8)

## graphs for EDA

library(ggplot2)
# Lot
unique(Brk_new_2$lot)
ggplot(data = Brk_new_2, aes(x=(lot),y=sqrt(sale_price))) +
  geom_point()

# Lot_type
unique(Brk_new_2$LotType)
ggplot(data = Brk_new_2, aes(x=as.factor(LotType),y=sqrt(sale_price), fill = LotType)) +
  geom_boxplot()

# non-linear 
# need to convert to factor
Brk_new_2$LotType <- as.factor(Brk_new_2$LotType)

# commercial_units
unique(Brk_new_2$commercial_units)
ggplot(data = Brk_new_2, aes(x=as.factor(commercial_units),y=sqrt(sale_price), fill = commercial_units)) +
  geom_boxplot()
# non-linear 
# need to convert to factor

# Limiting the commercial units to 20
Brk_new_2 <-Brk_new_2[Brk_new_2$commercial_units < 20,]
ggplot(data = Brk_new_2, aes(x=as.factor(yr_built),y=sqrt(sale_price))) +
  geom_boxplot()

## Converting the variables to factors
# Factor conversion
Brk_new_2[,c("zip_code","SchoolDist","Council","PolicePrct","HealthCent","tax_class","neighborhood",
             "building_class","lot","year_of_sale",
             "Easements","BsmtCode","garage_present","residential_units","NumBldgs","NumFloors",
             "UnitsRes","UnitsTotal","year_built","alter_1","alter_2","no_alter","commercial_units")] <- 
  lapply(Brk_new_2[,c("zip_code","SchoolDist","Council","PolicePrct","HealthCent","tax_class","neighborhood"
                      ,"building_class","lot","year_of_sale",
                      "Easements","BsmtCode","garage_present","residential_units","NumBldgs",
                      "NumFloors","UnitsRes","UnitsTotal","year_built","no_alter","commercial_units"
                      ,"alter_1","alter_2")],function(x) factor(x))




# Numeric column division
df.numeric <- Brk_new_2[,c("land_sqft","gross_sqft","BldgFront","AssessTot")]

require(psych)
skewed <- apply(df.numeric,2,skewness)
skewed <- skewed[(skewed > 0.8) | (skewed < -0.8)]
skewed

ggplot(Brk_new_2,aes(land_sqft, fill = "red")) +
  geom_density(alpha=0.5) + 
  ggtitle("Density plot of Land_sqft")

ggplot(Brk_new_2,aes(gross_sqft, fill = "red")) +
  geom_density(alpha=0.5) + 
  ggtitle("Density plot of gross_sqft")

ggplot(Brk_new_2,aes(BldgFront, fill = "red")) +
  geom_density(alpha=0.5) + 
  ggtitle("Density plot of BldgFront")

ggplot(Brk_new_2,aes(AssessTot, fill = "red")) +
  geom_density(alpha=0.5) + 
  ggtitle("Density plot of AssessTot")

ggplot(Brk_new_2,aes(population, fill = "red")) +
  geom_density(alpha=0.5) + 
  ggtitle("Density plot of population")

ggplot(Brk_new_2,aes(avg_income, fill = "red")) +
  geom_density(alpha=0.5) + 
  ggtitle("Density plot of avg_income")

kurtosis <- apply(df.numeric, 2, kurtosi)
kurtosis <- kurtosis[(kurtosis > 3.0) | (kurtosis < -3.0)]
kurtosis

#df.numeric[,"lot"] <- log(1+df.numeric[,"lot"])
df.numeric[,"gross_sqft"] <- log(1+df.numeric[,"gross_sqft"])
df.numeric[,"BldgFront"] <- log(1+df.numeric[,"BldgFront"])
df.numeric[,"AssessTot"] <- log(1+df.numeric[,"AssessTot"])
df.numeric[,"land_sqft"] <- log(1+df.numeric[,"land_sqft"])


ggplot(df.numeric,aes(land_sqft, fill = "red")) +
  geom_density(alpha=0.5) + 
  ggtitle("Density plot of Land_sqft(log)")

ggplot(df.numeric,aes(gross_sqft, fill = "red")) +
  geom_density(alpha=0.5) + 
  ggtitle("Density plot of gross_sqft(log)")

ggplot(df.numeric,aes(BldgFront, fill = "red")) +
  geom_density(alpha=0.5) + 
  ggtitle("Density plot of BldgFront(log)")

ggplot(df.numeric,aes(AssessTot, fill = "red")) +
  geom_density(alpha=0.5) + 
  ggtitle("Density plot of AssessTot(log)")

# Normalize the data

scaler <- preProcess(df.numeric)
df.numeric <- predict(scaler, df.numeric)
df.numeric <- cbind(df.numeric,sale_price = Brk_new_2$sale_price)


#one hot encoding

dum <- dummyVars("~.",data = Brk_new_2[,c("zip_code","SchoolDist","Council","PolicePrct","HealthCent","tax_class","neighborhood",
                                          "building_class","year_of_sale",
                                          "Easements","BsmtCode","garage_present","residential_units","NumBldgs","NumFloors",
                                          "UnitsRes","UnitsTotal","year_built","alter_1","alter_2","no_alter","commercial_units")] )
df.categoty <- data.frame(predict(dum,newdata = Brk_new_2[,c("zip_code","SchoolDist","Council","PolicePrct","HealthCent","tax_class","neighborhood",
                                                             "building_class","year_of_sale",
                                                             "Easements","BsmtCode","garage_present","residential_units","NumBldgs","NumFloors",
                                                             "UnitsRes","UnitsTotal","year_built","alter_1","alter_2","no_alter","commercial_units")] ))


data <- cbind(df.numeric,df.categoty)

# Normality of sale_price
plot(density(sqrt(data$sale_price)))
qqnorm(sqrt(data$sale_price))
qqline(sqrt(data$sale_price))

ggplot(data,aes(sqrt(sale_price), fill = "red")) +
  geom_density(alpha=0.5) + 
  ggtitle("Density plot of Sale Price(SQRT)")

# Additional dataset creation with all variables and 
# variables excluding near zero variance

nzv.data <- nearZeroVar(data, saveMetrics = TRUE)

drop.cols <- rownames(nzv.data)[nzv.data$nzv==TRUE]
keep.cols <- rownames(nzv.data)[nzv.data$nzv==FALSE]
data1 <- data[,..keep.cols]


