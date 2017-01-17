# Microncleus test in mexican welders
# Exploratory analysis and descriptive statistics
# This work is licensed under a Creative Commons Attribution 4.0 International License.

## Sourcing the previous script, which loads (and attaches) the data and gets it ready
source(
    "https://raw.githubusercontent.com/jclopeztavera/genotoxic-micronuclei/master/cleaning_data.R",
    echo = FALSE
)

## loading the psych package
if (!require(psych)) {
    install.packages("psych")
    library(psych)
}

## General descriptive statistics
d.data <- describe(data) # for the whole dataset
d.cases <- describe(controls) # describing the welders data
d.controls <- describe(cases) # describing the control data

## Comparing means and counts of non-micronucleus test data
### Comparing age and BMI using a t-test for two samples with unequal variances
age.cmp <- t.test(
    case.age,
    control.age,
    exact = TRUE,
    alternative = "two.sided",
    conf.level = 0.95
) # the age between welders and controls does not differ significantly

bmi.cmp <- t.test(
    case.bmi,
    control.bmi,
    exact = TRUE,
    alternative = "two.sided",
    conf.level = 0.95
)  # the BMI between welders and controls does not differ significantly

### Comparing the binary data using Fisher's exact test for count data
smoke.cmp <- fisher.test(
    smoke,
    case,
    alternative = "two.sided",
    conf.int = TRUE,
    conf.level = 0.95
) # no significant difference in smoking between welders and controls

drink.cmp <- fisher.test(
    drink,
    case,
    alternative = "two.sided",
    conf.int = TRUE,
    conf.level = 0.95
) # no significant difference in drinking between welders and controls

amalgam.cmp <- fisher.test(
    amalgam,
    case,
    alternative = "two.sided",
    conf.int = TRUE,
    conf.level = 0.95
) # no significant difference in amalgam presence between welders and controls

## Defining a function that comparatively plots the densities of variables
plot.density <- function(a, b) {
    a <- lapply(a, density)
    b <- lapply(b, density)
    for (i in seq(2, length(a))) {
        plot(
            x = range(a[[i]]$x, b[[i]]$x),
            y = range(a[[i]]$y, b[[i]]$y),
            xlab = names(a[i]),
            ylab = "Density",
            main = paste(names(a[i]), "density plot")
        )
        lines(a[[i]], col = "red", lwd = 2)
        lines(b[[i]], col = "blue", lwd = 2)
        legend(
            "topright",
            c("Cases", "Controls"),
            lty = c(1, 1),
            lwd = c(2, 2),
            col = c("red", "blue")
        )
        #readline(prompt = "Hit <Return> to see next plot:")
    }
}
plot.density(cases, controls)

# Defining a function that box-plots the
boxploting <- function(x, y) {
    for (i in seq(7, ncol(x))) {
        xi <- dput(x[, i])
        yi <- dput(y[, i])
        if (length(xi) != length(yi))
        {
            length(xi) == length(yi)
        }
        datum <- cbind(xi, yi)
        names(datum) <- lapply(c(x, y), names)
        boxplot(
            datum,
            data = datum,
            varwidth = TRUE,
            outline = TRUE,
            show.names = TRUE,
            names = c(names(x[i]), names(y[i])),
            outpch = 20
        )
        #readline(prompt = paste("Press <Return> to see next boxplot"))
    }
}
boxploting(controls, cases)

## Defining a function that uses the Shapiro-Wilk Normality Test to return whether each variable is normally distributed (p >= 0.1) or not (p < 0.1)
shapiro.wilk <- function(data) {
    norm.test <- lapply(data, shapiro.test)
    for (i in seq(1, length(norm.test))) {
        if (norm.test[[i]]$p.value >= 0.1) {
            message(paste(names(norm.test[i]), "is normally-distributed"))
        } else {
            message(paste(names(norm.test[i]), "is not normally-distributed"))
        }
    }
}
shapiro.wilk(data)
shapiro.wilk(cases)
### Controls have no exposition years, so we ommit that variable
test.controls <- controls
test.controls$control.exp.years <- NULL
shapiro.wilk(test.controls)
rm(test.controls)

## Stabilizing the variance by transforming the data using squared root
sqrt.data <- sqrt(data[, 8:14])
sqrt.data <- cbind(data[, 1:7], sqrt.data)

## Subsetting again cases and controls with the transformed data
sqrt.cases <- sqrt.data[which(sqrt.data$case == 1), ]
sqrt.cases$case <- NULL
names(sqrt.cases) <-
    paste("sqrt.case", names(sqrt.cases), sep = ".")
sqrt.controls <- sqrt.data[which(sqrt.data$case == 0), ]
sqrt.controls$case <- NULL
names(sqrt.controls) <-
    paste("sqrt.control", names(sqrt.controls), sep = ".")

## Running again all the previous tests, now on the transformed data.
plot.density(sqrt.cases, sqrt.controls)
boxploting(sqrt.controls, sqrt.cases)
shapiro.wilk(sqrt.data)
shapiro.wilk(sqrt.cases)
### Controls have no exposition years, so we ommit that variable
test.sqrt.controls <- sqrt.controls
test.sqrt.controls$sqrt.control.exp.years <- NULL
shapiro.wilk(test.sqrt.controls)
rm(test.sqrt.controls)
