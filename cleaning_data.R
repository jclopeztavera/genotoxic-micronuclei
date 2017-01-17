# Microncleus Test in Mexican Welders: Getting and Cleaning Data
# This work is licensed under a Creative Commons Attribution 4.0 International License.

## Loading the csv file into the workspace...
data <-
    read.csv("https://files.figshare.com/2037162/micronuclei_new_data.csv")

## Due to a socio-demographic feature, the participants (welders)
## were all male. No need of the male constant in the data frame...
data$gender <- NULL

## Subsetting cases and controls...
cases <- data[which(data$case == 1),]
cases$case <- NULL
names(cases) <- paste("case", names(cases), sep = ".")

controls <- raw.data[which(raw.data$case == 0),]
controls$case <- NULL
names(controls) <- paste("control", names(controls), sep = ".")

dput(data, file = "micronuclei-data.csv")
#dump("data", file = "micronuclei-dumpdata.R") # uncomment for R-formatted data
