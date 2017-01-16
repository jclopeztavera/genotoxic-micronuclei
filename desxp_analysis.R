# Microncleus test in mexican welders
# Exploratory analysis and descriptive statistics
# This work is licensed under a Creative Commons Attribution 4.0 International License.

# Sourcing the previous script, which imports the data set and gets it ready for analyses
source("http://files.figshare.com/2037299/cleaning_data.R", echo = FALSE)

attach(data)
attach(cases)
attach(controls)

# Descriptive statistics
d.data <- describe(data)
d.cases <- describe(controls)
d.controls <- describe(cases)

# Comparing means and counts of non-micronucleus test data
# Comparing age and BMI using a t-test for two samples with unequal variances 
age.cmp <- t.test(case.age, control.age, 
                  exact = TRUE, 
                  alternative = "two.sided", 
                  conf.level = 0.95)

bmi.cmp <- t.test(case.bmi, control.bmi, 
                  exact = TRUE, 
                  alternative = "two.sided", 
                  conf.level = 0.95)

# Comparing the binary data using the fisher's exact test for count data
smoke.cmp <- fisher.test(smoke, case, 
                         alternative = "two.sided", 
                         conf.int = TRUE, 
                         conf.level = 0.95)

drink.cmp <- fisher.test(drink, case, 
                         alternative = "two.sided", 
                         conf.int = TRUE, 
                         conf.level = 0.95)

amalgam.cmp <- fisher.test(amalgam, case, 
                           alternative = "two.sided", 
                           conf.int = TRUE, 
                           conf.level = 0.95)


# Function that plots the densities of cases and controls
plot.d <- function(a, b){
    a <- lapply(a, density)
    b <- lapply(b, density)
    for(i in seq(2, length(a))){  
        p <- plot(x = range(a[[i]]$x, b[[i]]$x), 
                  y = range(a[[i]]$y, b[[i]]$y), 
                  xlab = names(a[i]), ylab = "Density", 
                  main = paste(names(a[i]), "density plot"))
        lines(a[[i]], col = "red", lwd = 2)  
        lines(b[[i]], col = "blue", lwd = 2) 
        legend("topright", c("Cases", "Controls"), 
               lty = c(1, 1), lwd = c(2, 2), col=c("red","blue"))
        #readline(prompt = "Hit <Return> to see next plot:")
    }
}

plot.d(cases, controls)

# Using box plots to compare the frequencies of MNi and NA between cases and controls
boxploting <- function(x, y){
    for(i in seq(7, ncol(x))){
        xi <- dput(x[, i])
        yi <- dput(y[, i])
        if(length(xi) != length(yi))
        {length(xi) == length(yi)}
        datum <- cbind(xi, yi)
        names(datum) <- lapply(c(x, y), names)
        b <- boxplot(datum, 
                     data = datum,
                     varwidth = TRUE,
                     outline = TRUE,
                     show.names = TRUE, 
                     names = c(names(x[i]), names(y[i])),
                     outpch = 20)
        #readline(prompt = paste("Press <Return> to see next boxplot"))
    }
}

boxploting(controls, cases)

# Testing if the data has a normal distribution using the Shapiro-Wilk Normality Test
# Function that prints if each variable is normal (p >= 0.1) or not (p < 0.1)
shapiro.wilk <- function(data){
    norm.test <- lapply(data, shapiro.test)
    for(i in seq(1, length(norm.test))){
        if(norm.test[[i]]$p.value >= 0.1){
            message(paste(names(norm.test[i]), "is normally-distributed"))
        }else{
            message(paste(names(norm.test[i]), "is not normally-distributed"))
        }
    }
}

shapiro.wilk(data)
shapiro.wilk(cases)

# Controls have no exposition years, so we ommit that variable
test.controls <- controls
test.controls$control.exp.years <- NULL
shapiro.wilk(test.controls)

# Stabilizing the variance by transforming the micronucleus test data using squared root
data[ , 8:14] <- sqrt(data[ , 8:14])

# RE-Subsetting cases and controls with the transformed data
cases <- data[which(data$case == 1), ]
cases$case <- NULL
names(cases) <- paste("case", names(cases), sep = ".")
controls <- data[which(data$case == 0), ]
controls$case <- NULL
names(controls) <- paste("control", names(controls), sep = ".")

# Re-running all the previous tests, now on the transformed data. 
plot.d(cases, controls)
boxploting(controls, cases)
shapiro.wilk(data)
shapiro.wilk(cases)
# Controls have no exposition years, so we ommit that variable
test.controls <- controls
test.controls$control.exp.years <- NULL
shapiro.wilk(test.controls)

remove(test.controls, i, shapiro.wilk, plot.d, boxploting) # Keeping the environment tidy
