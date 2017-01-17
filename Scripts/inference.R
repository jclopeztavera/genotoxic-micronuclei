# Microncleus test in mexican welders
# Inferential statistics: Hypotheses testing and regressions
# This work is licensed under a Creative Commons Attribution 4.0 International License.

## Sourcing the R script that loads the data and gets it ready
source(
    "https://raw.githubusercontent.com/jclopeztavera/genotoxic-micronuclei/master/Scripts/cleaning_data.R",
    echo = FALSE
)

## Sourcing the exploratory data analysis
#source("https://raw.githubusercontent.com/jclopeztavera/genotoxic-micronuclei/master/desxp_analysis.R") # Uncomment to run

# attach(data)
# attach(cases)
# attach(controls)

## Stabilizing the variance by transforming the data using squared root
sqrt.data <- sqrt(data[, 8:14])
sqrt.data <- cbind(data[, 1:7], sqrt.data)

## Subsetting again cases and controls with the transformed data
sqrt.cases <- sqrt.data[which(sqrt.data$case == 1),]
sqrt.cases$case <- NULL
names(sqrt.cases) <-
    paste("sqrt.case", names(sqrt.cases), sep = ".")
sqrt.controls <- sqrt.data[which(sqrt.data$case == 0),]
sqrt.controls$case <- NULL
names(sqrt.controls) <-
    paste("sqrt.control", names(sqrt.controls), sep = ".")

attach(sqrt.cases)
attach(sqrt.controls)

## Testing the equality of MN and NA frequencies between cases and controls using the Wilcoxon signed rank test with continuity correction:
### For micronucleated cells...
mn.test <- wilcox.test(
    sqrt.case.micronuclei,
    sqrt.control.micronuclei,
    exact = FALSE,
    alternative = "two.sided",
    conf.int = TRUE,
    conf.level = 0.95,
    paired = TRUE
)

### For binucleated cells...
bn.test <- wilcox.test(
    sqrt.case.binucleated,
    sqrt.control.binucleated,
    exact = FALSE,
    alternative = "two.sided",
    conf.int = TRUE,
    conf.level = 0.95,
    paired = TRUE
)

### For lobulated nuclei cells...
ln.test <- wilcox.test(
    sqrt.case.lobulated,
    sqrt.control.lobulated,
    exact = FALSE,
    alternative = "two.sided",
    conf.int = TRUE,
    conf.level = 0.95,
    paired = TRUE
)

### For pyknotic cells...
pk.test <- wilcox.test(
    sqrt.case.pyknotic,
    sqrt.control.pyknotic,
    exact = FALSE,
    alternative = "two.sided",
    conf.int = TRUE,
    conf.level = 0.95,
    paired = TRUE
)

### For condensed-chromatin-cells...
cc.test <- wilcox.test(
    sqrt.case.condensed,
    sqrt.control.condensed,
    exact = FALSE,
    alternative = "two.sided",
    conf.int = TRUE,
    conf.level = 0.95,
    paired = TRUE
)

### For karyorrhectic cells...
kr.test <- wilcox.test(
    sqrt.case.karyorrhexis,
    sqrt.control.karyorrhexis,
    exact = FALSE,
    alternative = "two.sided",
    conf.int = TRUE,
    conf.level = 0.95,
    paired = TRUE
)

### For karyolytic cells...
kl.test <- wilcox.test(
    sqrt.case.karyolysis,
    sqrt.control.karyolysis,
    exact = FALSE,
    alternative = "two.sided",
    conf.int = TRUE,
    conf.level = 0.95,
    paired = TRUE
)


## Linear regressions of significant NA against non-nuclear variables
### Creating the formulas
cc <- formula(condensed ~ case)
bn <- formula(binucleated ~ case)

# Initial quasi-poisson models
bn.model <- glm(
    bn,
    family = quasipoisson(link = "sqrt"),
    model = TRUE,
    method = "glm.fit",
    x = FALSE,
    y = TRUE
)

cc.model <- glm(
    cc,
    family = quasipoisson(link = "sqrt"),
    model = TRUE,
    method = "glm.fit",
    x = FALSE,
    y = TRUE
)

### Adding and dropping significant terms
#### Stepwise forward selection of terms for bn.model
add1(bn.model,
     scope = ~. + age + exp.years + bmi + smoke + drink + amalgam,
     test = "F")
bn.model <- update(bn.model, ~ . + smoke) # adding smoke variable
add1(bn.model,
     scope = ~. + age + exp.years + bmi + drink + amalgam,
     test = "F")
drop1(bn.model, test = "F")  # backward step to check the model
#### Stepwise forward selection of terms for cc.model
add1(cc.model, scope = ~. * age * amalgam, test = "F") # no variables to add

### Summarise the models
summary(bn.model)
summary(cc.model)

## Preparing figures for PLoS
### Installing the required packages
if (!require(ggplot2)) {
    install.packages("ggplot2")
    library(ggplot2)
}

if (!require(sjPlot)) {
    install.packages("sjPlot")
    library(sjPlot)
}

if (!require(extrafont)) {
    install.packages("extrafont") # to get the Arial and Open Symbol fonts
    library(extrafont)
}

#font_import(); y # takes a few minutes, uncomment to run

### Plotting the BN model
bn.plot <- sjp.glm(
    bn.model,
    type = "glm",
    sortOdds = TRUE,
    title = "Odds Ratios of Binucleated Cells",
    axisLabels.y = c("Welding Fumes", "Tobacco Use", "Age", "Intercept"),
    axisTitle.x = "Odds Ratios",
    axisLimits = NULL,
    breakTitleAt = 50,
    breakLabelsAt = 25,
    gridBreaksAt = 0.5,
    transformTicks = FALSE,
    geom.size = 1.5,
    geom.colors = "Set1",
    hideErrorBars = FALSE,
    interceptLineType = 2,
    interceptLineColor = "grey70",
    coord.flip = TRUE,
    showIntercept = TRUE,
    showAxisLabels.y = TRUE,
    showValueLabels = TRUE,
    labelDigits = 2,
    showPValueLabels = TRUE,
    showModelSummary = FALSE,
    facet.grid = FALSE,
    show.se = TRUE,
    showOriginalModelOnly = TRUE,
    printPlot = TRUE
)
bn.plot$plot + theme_bw(base_size = 12, base_family = "Arial") # Formatting

### Plotting the CC model
cc.plot <- sjp.glm(
    cc.model,
    type = "glm",
    sortOdds = TRUE,
    title = "Odds Ratios of Condensed-Chromatin Cells",
    axisLabels.y = c("Welding Fumes", "Tobacco Use", "Age", "Intercept"),
    axisTitle.x = "Odds Ratios",
    axisLimits = NULL,
    breakTitleAt = 50,
    breakLabelsAt = 25,
    gridBreaksAt = 0.5,
    transformTicks = FALSE,
    geom.size = 1.5,
    geom.colors = "Set1",
    hideErrorBars = FALSE,
    interceptLineType = 2,
    interceptLineColor = "grey70",
    coord.flip = TRUE,
    showIntercept = TRUE,
    showAxisLabels.y = TRUE,
    showValueLabels = TRUE,
    labelDigits = 2,
    showPValueLabels = TRUE,
    showModelSummary = FALSE,
    facet.grid = FALSE,
    show.se = TRUE,
    showOriginalModelOnly = TRUE,
    printPlot = TRUE
)
cc.plot$plot + theme_bw(base_size = 12, base_family = "Arial") # Formatting
