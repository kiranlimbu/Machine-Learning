options(scipen=999) # to avoid scientific notation
# Load data
test_df <- read.csv("test.csv")
train_df <- read.csv("train.csv")

# combine data by row bind
df <- rbind(test_df, train_df)

# -------------------------------------------
# Inspect data
# -------------------------------------------
str(df)
head(df)

# Since we combined two dataset (test.csv and train.csv), there is a chance that ID column could have a duplicate.
anyDuplicated(df$id) # 1001

# To confirm we check the total number of rows in test.csv
nrow(test_df) # 1000
# peek to see if row number 1001 and 1002 has same ID as top two row
df[c(1001,1002,1,2), 1:4]

# remove ID column from the dataset.
df <- df[,-1]
# verify
names(df)

# Check for duplicates one more time.
nrow(unique(df))
anyDuplicated(df)

# Check for missing values
# NAs values
sapply(df, function(x) sum(is.na(x)))

# Empty cells ("" values)
sapply(df, function(x) sum(x == ""))

# NULL values
sapply(df, function(x) sum(is.null(x)))

# -------------------------------------------
# Data Understanding
# 7399 obs
# 29 variables
# 8 int type
# 1 num type
# 20 chr type
# Dependent variable (Response variable) = sale_price
# NA values = 3279 in "original_price" column
# "" values (empty) = 1443 in various columns
# NULL values = 0
# duplicate records = 0
# -------------------------------------------

# -------------------------------------------
# Remove few columns for simplification
# -------------------------------------------
df <- within(df, rm("times_viewed", "assured_buy", "registered_city", "registered_state", "is_hot", "source", "car_availability", "broker_quote", "original_price", "ad_created_on", "reserved", "booking_down_pymnt", "emi_starts_from", "rto"))
names(df) # varify


# Separating Numeric variables and character variables
df_numType <- df[,c(2,4,5,12)]
df_chrType <- df[,c(1,3,6:11,13:15)]
str(df_numType)
str(df_chrType)


# -------------------------------------------
# Exploring numeric variables
# -------------------------------------------
summary(df_numType) # catagorical: yr_mfr, total_owner

quantile(df_numType$sale_price)
# diff between Q3 and Q1 = 258,950 (print(540099-281149))
# diff between min and Q1 = 281,149
# diff between max and Q3 = 3,325,901 (print(3866000-540099))

# the diff between Q3 and Q1 is almost same as diff between min and Q1.
# This suggest that the upper 25% of the values are more widely dispersed
# We see similar trend on KMS_RUN

quantile(df_numType$kms_run)
# diff between Q3 and Q1 = 52,305 (print(540099-281149))
# diff between min and Q1 = 31,822
# diff between max and Q3 = 912,422 (print(3866000-540099))

# Mean is more sensitive to extreme values.
# And the mean is much higher than the median, we can not trust mean value. 
# we will use median for SALE_PRICE, KMS_RUN

# The upper 25% of the values are even more widely dispersed

# IQR() diff between Q1 and Q3, that can be used in removing outliers
IQR(df$sale_price) # 258700


# -------------------------------------------
# Visualizing numeric variables
# -------------------------------------------
options(scipen=999) # to avoid scientific notation

par(mfrow=c(1,2)) # placeholder for 1 row, 2 col
for (i in 2:3) {
    boxplot(df_numType[,i], main=names(df_numType)[i], ylab=colnames(df_numType[i]))
}
# The minimum and maximum values can be illustrated using the whiskers that extend below
# and above the box. The plot shows several outliers on the high ends for both sale_price and kms_run.
# These outliers are responsible for our earlier finding, which helped mean value to be much higher
# then the median.

par(mfrow=c(1,2)) # placeholder for 1 row, 2 col
for (i in 2:3) {
    hist(df_numType[,i], main=names(df_numType)[i], xlab=colnames(df_numType[i]))
}
# It seems that both the histogram are not evenly divided, instead both have right skew.

var(df_numType$sale_price) # 79925073012
# variance: larger numbers indicate that the data are spread more widely around the mean

sd(df_numType$sale_price) # 282710.2 # on average the the price is 282710.2 away from the mean

# -------------------------------------------
# Data cleaning - numeric variables
# -------------------------------------------

# check 0 values -----------------------------
# sale_price starts with 0 which is not normal. Lets investigate
sapply(df_numType, function(x) sum(x == 0))

# Replace 0 with relative values ------------------------
# Find matching value
car_zero_value <- df[df$sale_price==0,c("car_name", "car_rating", "kms_run", "sale_price", "total_owners", "transmission")]
car_zero_value

# Look for price with similar attribute
car_zero_value_surrounding <- df[df$transmission=="manual" & df$total_owners<=2 & df$car_rating=="great" & df$car_name=="maruti swift dzire" & df$kms_run>38000 & df$kms_run<42000 & df$sale_price!=0, c("make", "car_rating", "kms_run", "sale_price")]
car_zero_value_surrounding

# use median value to replace 0
df$sale_price[df$sale_price==0] <- median(car_zero_value_surrounding$sale_price)
df[df$sale_price==0,] # verify 0 is replaced

# Repeat the process for sale_price that has 35 value, considering similar attributes
df$sale_price[df$sale_price==35] <- 45000

# remove outliers for SALE_PRICE ---------------------------------------------------

# Sale_price OUTLIER ---------
# step 1, find threshold
# (Q1, Q3, Q3-Q1, 1.5times)
# Exclude values less then, (Q1 - 1.5*(Q3-Q1)). I have no outliers below so we'll ignore this.
# Exclude valuees above, (Q3 + 1.5*(Q3-Q1))
print(540099 + 1.5 * 258700) # 928149
# exclude rows that has values above 928149
df <- df[df$sale_price < 928149,]
nrow(df) # 6985 (7399 - 6985 = 414 rows removed)

boxplot(df$sale_price, main="Boxplot of Used Car - Prices", ylab="Prise (INR)")

# KMS_RUN OUTLIER----------------------
# (Q3 + 1.5*(Q3-Q1))
quantile(df$kms_run)
IQR(df$kms_run) # 51582
print(84232 + 1.5 * 51582) # 161605
# only keep rows that has values below 161605
df <- df[df$kms_run < 161605,]
nrow(df) # 6820 (6985 - 6820 = 165 rows removed)

boxplot(df$kms_run, main="Boxplot of Used Car - Mileage", ylab="Odometer (Km)")
# -------------------------------------------
# Visualizing categorical variables
# -------------------------------------------
str(df_chrType)

# We will use YR_MFR and TOTAL_OWNERS as categorical data even though it is numeric type vector.
# Each year can be applied to multiple cars.
table(df_numType$yr_mfr)
table(df_numType$total_owners)

table(df_chrType$fuel_type)
table(df_chrType$city)
table(df_chrType$body_type) # 103 empty cell
table(df_chrType$transmission) # 556 empty cell

table(df_chrType$variant)

# most advertised car
make_table <- table(df_chrType$make)
make_prod <- prop.table(make_table)*100
round(make_prod, digits = 1) # Maruti (domestic car) is the most advertised car

table(df_chrType$make)
table(df_chrType$model)
table(df_chrType$car_rating) # 9 empty cell
table(df_chrType$fitness_certificate) # 8 empty cell
table(df_chrType$warranty_avail)

# -------------------------------------------
# Data cleaning - categorical variables
# -------------------------------------------

# Body_type --------------------------------------------------------------
table(df_chrType$body_type) # 103 empty cell

# Find empty cell
emptyCell <- df[df$body_type=="", ]
emptyCell[emptyCell$body_type=="" & emptyCell$model=="new  wagon-r", c("make", "model", "body_type", "variant")]

# Find complete row with similar attribute
df[df$model=="new  wagon-r" & df$make=="maruti", c("make", "model", "body_type", "variant")]
# Or search for bodytype information in internet

# Fill empty cells # 103 cells
df$body_type[df$body_type=="" & df$make=="hyundai" & df$model=="creta"] <- "suv"
df$body_type[df$body_type=="" & df$make=="maruti" & df$model=="swift"] <- "hatchback"
df$body_type[df$body_type=="" & df$make=="maruti" & df$model=="swift dzire"] <- "sedan"
df$body_type[df$body_type=="" & df$make=="ford" & df$model=="ecosport"] <- "suv"
df$body_type[df$body_type=="" & df$make=="maruti" & df$model=="alto 800"] <- "hatchback"
df$body_type[df$body_type=="" & df$make=="maruti" & df$model=="800"] <- "hatchback"
df$body_type[df$body_type=="" & df$make=="hyundai" & df$model=="new santro"] <- "hatchback"
df$body_type[df$body_type=="" & df$make=="maruti" & df$model=="new  wagon-r"] <- "hatchback"
df$body_type[df$body_type=="" & df$make=="hyundai" & df$model=="verna"] <- "sedan"
df$body_type[df$body_type=="" & df$make=="volkswagen" & df$model=="vento"] <- "sedan"
df$body_type[df$body_type=="" & df$make=="mg" & df$model=="hector"] <- "suv"
df$body_type[df$body_type=="" & df$make=="honda" & df$model=="city zx"] <- "sedan"
df$body_type[df$body_type=="" & df$make=="ford" & df$model=="freestyle"] <- "suv"
df$body_type[df$body_type=="" & df$make=="skoda" & df$model=="fabia"] <- "hatchback"
df$body_type[df$body_type=="" & df$make=="kia" & df$model=="seltos"] <- "suv"
df$body_type[df$body_type=="" & df$make=="mahindra" & df$model=="marazzo"] <- "suv"
df$body_type[df$body_type=="" & df$make=="honda" & df$model=="jazz"] <- "hatchback"
df$body_type[df$body_type=="" & df$make=="audi" & df$model=="a6"] <- "luxury sedan"
df$body_type[df$body_type=="" & df$make=="renault" & df$model=="captur"] <- "suv"
df$body_type[df$body_type=="" & df$make=="audi" & df$model=="a6"] <- "luxury sedan"
df$body_type[df$body_type=="" & df$make=="honda" & df$model=="city"] <- "sedan"
df$body_type[df$body_type=="" & df$make=="toyota" & df$model=="corolla altis"] <- "sedan"
df$body_type[df$body_type=="" & df$make=="tata" & df$model=="harrier"] <- "suv"
df$body_type[df$body_type=="" & df$make=="tata" & df$model=="indica v2"] <- "hatchback"
df$body_type[df$body_type=="" & df$make=="maruti" & df$model=="omni e"] <- "suv"
df$body_type[df$body_type=="" & df$make=="chevrolet" & df$model=="optra"] <- "sedan"
df$body_type[df$body_type=="" & df$make=="maruti" & df$model=="sx4"] <- "sedan"
df$body_type[df$body_type=="" & df$make=="maruti" & df$model=="vitara brezza"] <- "suv"
df$body_type[df$body_type=="" & df$make=="volvo" & df$model=="xc60"] <- "suv"
df$body_type[df$body_type=="" & df$make=="mg" & df$model=="zs ev"] <- "suv"

# TRANSMISSION --------------------------------------------------------------
table(df_chrType$transmission) # 103 empty cell
# Find empty cell
emptyCell <- df[df$transmission=="", ]
head(emptyCell)
emptyCell[emptyCell$transmission=="" & emptyCell$car_name=="maruti swift", c("make", "model", "variant", "transmission")]

# Find complete row with similar attribute 
df[df$transmission!="" & df$car_name=="maruti swift", c("make", "model", "variant", "transmission")]


# Fill empty cells
df$transmission[df$transmission=="" & df$make=="toyota" & df$model=="corolla altis"] <- "manual"
df$transmission[df$transmission=="" & df$make=="maruti" & df$model=="swift dzire"] <- "manual"
df$transmission[df$transmission=="" & df$make=="honda" & df$model=="amaze"] <- "manual"
df$transmission[df$transmission=="" & df$make=="hyundai" & df$model=="creta"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="maruti" & df$model=="alto k10"] <- "manual"
df$transmission[df$transmission=="" & df$make=="maruti" & df$model=="eeco"] <- "manual"
df$transmission[df$transmission=="" & df$make=="mahindra" & df$model=="xuv500"] <- "manual"
df$transmission[df$transmission=="" & df$make=="maruti" & df$model=="swift"] <- "manual"
df$transmission[df$transmission=="" & df$make=="hyundai" & df$model=="venue"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="maruti" & df$model=="baleno"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="maruti" & df$model=="omni e"] <- "manual"
df$transmission[df$transmission=="" & df$make=="tata" & df$model=="tigor"] <- "manual"
df$transmission[df$transmission=="" & df$make=="hyundai" & df$model=="verna"] <- "manual"
df$transmission[df$transmission=="" & df$make=="ford" & df$model=="freestyle"] <- "manual"
df$transmission[df$transmission=="" & df$make=="maruti" & df$model=="ertiga"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="hyundai" & df$model=="elite i20"] <- "manual"
df$transmission[df$transmission=="" & df$make=="maruti" & df$model=="s cross"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="mercedes bez" & df$model=="e class"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="mahindra" & df$model=="xuv 3oo"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="maruti" & df$model=="ciaz"] <- "manual"
df$transmission[df$transmission=="" & df$make=="maruti" & df$model=="s presso"] <- "manual"
df$transmission[df$transmission=="" & df$make=="hyundai" & df$model=="grand i10 nios"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="mg" & df$model=="hector"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="maruti" & df$model=="wagon r 1.0"] <- "manual"
df$transmission[df$transmission=="" & df$make=="honda" & df$model=="city"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="honda" & df$model=="jazz" & df$variant=="1.2 vx at"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="honda" & df$model=="jazz" & df$variant!="1.2 vx at"] <- "manual"
df$transmission[df$transmission=="" & df$make=="volkswagen" & df$model=="vento"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="maruti" & df$model=="baleno"] <- "automatic" #
df$transmission[df$transmission=="" & df$make=="mercedes benz" & df$model=="e class"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="ford" & df$model=="figo"] <- "manual"
df$transmission[df$transmission=="" & df$make=="mercedes benz" & df$model=="c class" & df$variant=="c 220 cdi elegance mt"] <- "manual"
df$transmission[df$transmission=="" & df$make=="mercedes benz" & df$model=="c class" & df$variant!="c 220 cdi elegance mt"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="tata" & df$model=="nexon "] <- "manual"
df$transmission[df$transmission=="" & df$make=="ford" & df$model=="ecosport" & df$variant=="1.5 tdci titanium plus"] <- "manual"
df$transmission[df$transmission=="" & df$make=="ford" & df$model=="ecosport" & df$variant!="1.5 tdci titanium plus"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="volkswagen" & df$model=="ameo"] <- "manual"
df$transmission[df$transmission=="" & df$make=="maruti" & df$model=="new  wagon-r"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="renault" & df$model=="kwid"] <- "manual"
df$transmission[df$transmission=="" & df$make=="hyundai" & df$model=="grand i10"] <- "manual"
df$transmission[df$transmission=="" & df$make=="kia" & df$model=="seltos"] <- "manual"
df$transmission[df$transmission=="" & df$make=="honda" & df$model=="city zx"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="hyundai" & df$model=="new elantra"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="hyundai" & df$model=="new santro"] <- "automatic" 
df$transmission[df$transmission=="" & df$make=="tata" & df$model=="zest"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="maruti" & df$model=="xl6"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="hyundai" & df$model=="new santro"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="audi" & df$model=="q3"] <- "manual"
df$transmission[df$transmission=="" & df$make=="datsun" & df$model=="redi go"] <- "manual"
df$transmission[df$transmission=="" & df$make=="maruti" & df$model=="sx4"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="tata" & df$model=="harrier"] <- "manual"
df$transmission[df$transmission=="" & df$make=="maruti" & df$model=="vitara brezza"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="mahindra" & df$model=="scorpio"] <- "manual"
df$transmission[df$transmission=="" & df$make=="maruti" & df$model=="800"] <- "manual"
df$transmission[df$transmission=="" & df$make=="maruti" & df$model=="ritz"] <- "manual"
df$transmission[df$transmission=="" & df$make=="audi" & df$model=="a4"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="volkswagen" & df$model=="polo"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="honda" & df$model=="civic"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="skoda" & df$model=="superb"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="renault" & df$model=="captur"] <- "manual"
df$transmission[df$transmission=="" & df$make=="maruti" & df$model=="ignis"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="renault" & df$model=="triber"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="ford" & df$model=="figo aspire"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="nissan" & df$model=="sunny"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="audi" & df$model=="a3"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="maruti" & df$model=="alto"] <- "manual"
df$transmission[df$transmission=="" & df$make=="tata" & df$model=="nano"] <- "manual"
df$transmission[df$transmission=="" & df$make=="toyota" & df$model=="glanza"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="tata" & df$model=="tiago"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="mahindra" & df$model=="bolero"] <- "manual"
df$transmission[df$transmission=="" & df$make=="mahindra" & df$model=="marazzo"] <- "manual"
df$transmission[df$transmission=="" & df$make=="toyota" & df$model=="etios liva"] <- "manual"
df$transmission[df$transmission=="" & df$make=="toyota" & df$model=="yaris"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="toyota" & df$model=="innova"] <- "manual"
df$transmission[df$transmission=="" & df$make=="hyundai" & df$model=="aura"] <- "manual"
df$transmission[df$transmission=="" & df$make=="skoda" & df$model=="rapid"] <- "manual"
df$transmission[df$transmission=="" & df$make=="mahindra" & df$model=="marazzo"] <- "manual"
df$transmission[df$transmission=="" & df$make=="hyundai" & df$model=="tucson new"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="jeep" & df$model=="compass"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="honda" & df$model=="crv"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="hyundai" & df$model=="xcent"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="skoda" & df$model=="fabia"] <- "manual"
df$transmission[df$transmission=="" & df$make=="audi" & df$model=="a6"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="hyundai" & df$model=="eon"] <- "manual"
df$transmission[df$transmission=="" & df$make=="mahindra" & df$model=="tuv300"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="tata" & df$model=="indica v2"] <- "manual"
df$transmission[df$transmission=="" & df$make=="mg" & df$model=="zs ev"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="nissan" & df$model=="kicks"] <- "manual"
df$transmission[df$transmission=="" & df$make=="hyundai" & df$model=="xcent"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="fiat" & df$model=="punto pure "] <- "manual"
df$transmission[df$transmission=="" & df$make=="mercedes benz" & df$model=="gla class"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="maruti" & df$model=="wagon r"] <- "manual"
df$transmission[df$transmission=="" & df$make=="honda" & df$model=="wr-v"] <- "manual"
df$transmission[df$transmission=="" & df$make=="volvo" & df$model=="xc60"] <- "automatic"
df$transmission[df$transmission=="" & df$make=="maruti" & df$model=="alto 800"] <- "manual"
df$transmission[df$transmission=="" & df$make=="chevrolet" & df$model=="optra"] <- "manual"
df$transmission[df$transmission=="" & df$make=="renault" & df$model=="duster"] <- "manual"


# RATING --------------------------------------------------------------
table(df_chrType$car_rating) # 9 empty cell
emptyCell <- df[df$car_rating=="", ]
emptyCell[, c("sale_price", "yr_mfr", "total_owners", "kms_run", "make")]

# Find complete row with similar attribute 
df[df$yr_mfr==2005 & df$kms_run>98000 & df$kms_run<108000, c("car_rating", "make", "yr_mfr", "kms_run", "sale_price")]

# Fill empty cells
df$car_rating[df$car_rating=="" & df$yr_mfr==2012 & df$kms_run>90000 & df$kms_run<100000] <- "great"
df$car_rating[df$car_rating=="" & df$yr_mfr==2014 & df$kms_run>32000 & df$kms_run<40000] <- "great"
df$car_rating[df$car_rating=="" & df$yr_mfr==2005 & df$kms_run>52000 & df$kms_run<63000] <- "good"
df$car_rating[df$car_rating=="" & df$yr_mfr==2008 & df$kms_run>53000 & df$kms_run<57000] <- "fair"
df$car_rating[df$car_rating=="" & df$yr_mfr==2006 & df$kms_run>65000 & df$kms_run<70000] <- "great"
df$car_rating[df$car_rating=="" & df$yr_mfr==2007 & df$kms_run>11000 & df$kms_run<15000] <- "great"
df$car_rating[df$car_rating=="" & df$yr_mfr==2011 & df$kms_run>73000 & df$kms_run<75000] <- "great"
df$car_rating[df$car_rating=="" & df$yr_mfr==2001 & df$kms_run>65000 & df$kms_run<100000] <- "fair"
df$car_rating[df$car_rating=="" & df$yr_mfr==2005 & df$kms_run>98000 & df$kms_run<108000] <- "fair"


# Fitness_certificate -------------------------------------------------
table(df_chrType$fitness_certificate) # 7 empty cell

emptyCell <- df[df$fitness_certificate=="", ]
emptyCell[, c("yr_mfr", "fitness_certificate", "kms_run")]

# Find complete row with similar attribute 
df[df$yr_mfr==2015 & df$kms_run>50000 & df$kms_run<58000, c("car_rating", "yr_mfr", "sale_price", "fitness_certificate")]

# Fill empty cells
df$fitness_certificate[df$fitness_certificate==""] <- "True"
table(df$fitness_certificate)

# -------------------------------------------
# Converting categorical variables to numeric
# -------------------------------------------
str(df_chrType)
# There is still one more column that we can remove "car_name". The column basically represents "make" and "model" combine.
df_chrType <- df_chrType[,-1]
df <- df[,-1]

# Change Formate
for (i in colnames(df_chrType)) {
    df[[i]] <- as.factor(df[[i]])
    df[[i]] <- as.numeric(df[[i]])
}

# verify
str(df)

# -------------------------------------------
# Visualizing relationships
# -------------------------------------------

df_simplified <- df # This helps us retain the clean dataset. Any mistake the df dataset will not affect the model and any mistake on df_simplified will not affect the df dataset.

library(psych)
pairs.panels(cor(df_simplified[c("sale_price", "yr_mfr", "fuel_type", "kms_run", "city", "body_type")]))
pairs.panels(cor(df_simplified[c("sale_price", "transmission", "variant", "make", "model")]))
pairs.panels(cor(df_simplified[c("sale_price", "total_owners", "car_rating", "fitness_certificate", "warranty_avail")]))

#-------------------------------------------
# TRAINING THE MODEL
# ------------------------------------------

set.seed(15) # helps produce the same result
idx <- sample(2, nrow(df), replace=TRUE, prob=c(0.7,0.3)) # split the dataset into 70% and 30%
train_data <- df[idx==1,] # 70%
test_data <- df[idx==2,] # 30%

model <- lm(sale_price ~ kms_run + transmission + car_rating + total_owners + warranty_avail + variant + yr_mfr + fuel_type + body_type + city, data = train_data)

coef(model)

#PLot PRICE VS ODOMETER
plot(df_simplified$sale_price~df_simplified$kms_run, main="Price VS Odometer", col="red")
abline(lm(df_simplified$sale_price~df_simplified$kms_run, data=train_data), col="blue")

# PLOT PRICE VS CONDITION
plot(df_simplified$sale_price~df_simplified$car_rating, main="Price VS Condition", col="green")
abline(lm(df_simplified$sale_price~df_simplified$car_rating, data=train_data), col="red")

# ----------------------------------------------
# PREDICTING VALUES
# ----------------------------------------------
predictions <- predict(model, test_data)

dev.off() # resets par() function
plot(predictions~test_data$sale_price, main="Sales price predictions against actual", col="blue")
abline(0,1, col="red")
# perfect model should have 45 degree tangent. I can see how well my model predicted. 

# ----------------------------------------------
# Analyze the model performance
# ----------------------------------------------
summary(model)

# Residuals section:
# Residual is equal to the true value minus the predicted value, the maximum error of 591,623 suggests that the model under predicted expenses by nearly 592,000 for at least one observation. The majority of predictions were between 70,514 (Q1) over the true value and 63,484 (Q3) under the true value.

# P-value
# Small p-values suggest that the true coeficient is very unlikely to be zero which means that the feature is extremely unlikely to have no relationship with the dependent variable. Some of the p-values have stars (***) which correspond to the significance level. p-values less than the significance level are considered statistically significant. Our model has several highly significant variables, and they relate to the outcome in logical ways.

# Multiple R-squred value
# It is similar to the correlation coefficient. Closer the value is to 1.0, the better the model perfectly explains the data. Our R-squared value is 0.6169, we can tell the model explains nearly 62 percent of the variation in the dependent variable. It is not uncommon for regression models of real-world data to have fairly low R-squared values. Overall, 60 percent is not a bad result. Since, Multiple R-squared and Adjusted R-squared values over lap, we know that the model does not penelize for the current number of variables. This tells us that removing any variable will not make any difference to the model.

# what could I do to make better.
# summary.
# 1. I can normalize all the variables












# ----------------------------------------------
# K-nn Classification
# ----------------------------------------------
# Exploring Data
str(df_simplified)
summary(df_simplified)

# remove sale_price (use it as output to compare)
itemsClassifi <- df_simplified[-4]
# confirm it has be removed
names(itemsClassifi)

# k-NN is heavily dependent upon the measurement scale of the input features. Looking into our features, we notice that value range are huge and differe, which will force the distance calculation impact to be different for each features. This could potentially cause problems for our classifier. To solve this problem we will apply normalization to rescale the features to standard range of values.

# create normalize function
normalize <- function(x) {
    return ((x-min(x)) / (max(x) - min(x)))
}

# apply the function to dataset
itemsClassifi_n <- as.data.frame(lapply(itemsClassifi, normalize))

# confirm function is applied
summary(itemsClassifi_n)

# -----------------------------------------------
# creating training and test dataset
# -----------------------------------------------
# Split them into training set (80%) and testing set (20%)
train_dt <- itemsClassifi_n[1:5456,] # 80%
test_dt <- itemsClassifi_n[5457:6820,] # 20%

# confirm
nrow(train_dt) # 5456 rows
nrow(test_dt) # 1364 rows

# Change the categorize sale_price to "high", "medium", "low" into "price" column
summary(df_simplified["sale_price"])
quantile(df_simplified$sale_price)
# Q1 below is low
# between Q3 and Q1 is medium
# Q3 and above is high
df_simplified$price[df_simplified$sale_price>=505224 & df_simplified$sale_price<940000] <- "high"
df_simplified$price[df_simplified$sale_price>276374 & df_simplified$sale_price<505224] <- "medium"
df_simplified$price[df_simplified$sale_price<=276374] <- "low"

# confirm
str(df_simplified$price)
table(df_simplified$price)
head(df_simplified)
names(df_simplified)

# -------------------------------------------------------------------------------------
# Create class labels with traget variables "sale_price"
train_labels <- df_simplified[1:5456, 15] # 80%
test_labels <- df_simplified[5457:6820, 15] # 20%
table(train_labels)
str(test_labels)


# ---------------------------------------------
# Find optimal k value
# ---------------------------------------------
set.seed(50)
# algorithm requires dependent variable as well
fitModel <- cbind(train_dt, train_labels) 

ctrl <- trainControl(method="repeatedcv", repeats=3)
knnFit <- train(train_labels~., data=fitModel, method="knn", trControl=ctrl, tuneLength=50)
knnFit
plot(knnFit)

# -----------------------------------------------
# Train a model
# -----------------------------------------------
library(class)

predicted <- knn(train = train_dt, test = test_dt, cl = train_labels, k=5) # k, square root of 5456


# -----------------------------------------------
# Evaluate
# -----------------------------------------------
library(gmodels)

CrossTable(x=test_labels, y=predicted, prop.chisq=FALSE)

# Cross Table
# each box contains 4 different values:
# 1. true negative (TN) # Example 207 correctly identified
# TP = actual value
# TN = diagonal
# FP = horizontal
# FN = vertical

# Accuracy = (TP + TN)/(TP+TN+FP+FN)

# high accuracy = 0.7827586 (k=83)
(207 + (88 + 613)) / (207 + (88+613) + (0+185) + (6+61))
# high accuracy =  0.8600252(k=5)
(303 + (1364-303)) / (303 + (1364-303) + (392-303) + (436-303))

# low accuracy = 0.8121646 (k=83)
(88 + (207+613)) / (88 + (207+613) + (6+193) + (0+11))
# low accuracy = 0.866582 (k=5)
(131 + (1364-131)) / (131 + (1364-131) + (287-131) + (185-131))

# medium accuracy = 0.6686303 (k=83)
(613 + (207+88)) / (613 + (207+88) + (61+11) + (185+193))
# medium accuracy = 0.7714932 (k=5)
(512 + (1364-512)) / (512 + (1364-512) + (685-512) + (743-512))


# Evaluate using ConfusionMatrix ---------------------
library(caret)

new_dt <- as.factor(test_labels)
confusionMatrix(new_dt, predicted)

