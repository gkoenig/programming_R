data <- read.csv(file = "./hw1_data.csv", header=TRUE)
validMeasures <- complete.cases(data)

# Question 15: What is the value of Ozone in the 47th row?
data$Ozone[47]

# Question 16: How many missing values are in the Ozone column of this data frame?
sum(my_nas <- is.na(data$Ozone))

# What is the mean of the Ozone column in this dataset? 
# Exclude missing values (coded as NA) from this calculation.
vals<-data$Ozone[!is.na(data$Ozone)]
mean(vals)

# Extract the subset of rows of the data frame 
# where Ozone values are above 31 and Temp values are above 90. 
# What is the mean of Solar.R in this subset?
extract1 <- subset.data.frame(data, Ozone>30 & Temp>90)
mean( vals <- extract1$Solar.R[!is.na(extract1$Solar.R)] )

# What is the mean of "Temp" when "Month" is equal to 6?
extract2 <- data[data$Month==6, ]
mean( vals <- extract2$Temp[!is.na(extract2$Temp)] )

# What was the maximum ozone value in the month of May (i.e. Month = 5)?
monthMay <- data[data$Month==5, ]
max(monthMay$Ozone,na.rm = TRUE)
