# Microncleus Test in Mexican Welders
# Cleaning the Data
# This work is licensed under a Creative Commons Attribution 4.0 International License.

# Loading and installing required packages
if(!require(psych)){
    install.packages("psych")
}

if(!require(car)){
    install.packages("car")
}

library(stats)
library(graphics)
library(car)
library(psych)

# Loading the csv file into the workspace
data <- read.csv("http://files.figshare.com/2037162/micronuclei_new_data.csv", header = TRUE, sep = ",")
raw.data <- data

# No need of a single-value (male) variable in the data frame
data$gender <- NULL
attach(data)

# Ordering the data frame by cases and controls, then age, and then bmi.
data <- data[order(case, age, bmi, decreasing = TRUE),]

attach(data)

# Transforming all variables to class = "numeric"
for(i in seq(1, ncol(data))){
    if(class(data[ , i]) != "numeric"){
        data[ , i] <- as.numeric(data[ , i])
    }
}

# Subsetting cases and controls
cases <- data[which(data$case == 1), ]
cases$case <- NULL
names(cases) <- paste("case", names(cases), sep = ".")

controls <- data[which(data$case == 0), ]
controls$case <- NULL
names(controls) <- paste("control", names(controls), sep = ".")

remove(i, raw.data)

