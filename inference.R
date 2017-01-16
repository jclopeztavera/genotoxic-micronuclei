# Microncleus test in mexican welders
# Inferential statistics: Hypotheses testing and regressions
# This work is licensed under a Creative Commons Attribution 4.0 International License.

# Sourcing the exploratory data analysis (lots of plotting
# source("desxp_analysis.R") # Uncomment to run

# Sourcing the previous script, which imports the data set and gets it ready for analyses
source("http://files.figshare.com/2037299/cleaning_data.R", echo = FALSE)

attach(data)
attach(cases)
attach(controls)

# Stabilizing the variance by transforming the micronucleus test data using squared root
data[ , 8:14] <- sqrt(data[ , 8:14])

# RE-Subsetting cases and controls with the transformed data
cases <- data[which(data$case == 1), ]
cases$case <- NULL
names(cases) <- paste("case", names(cases), sep = ".")
controls <- data[which(data$case == 0), ]
controls$case <- NULL
names(controls) <- paste("control", names(controls), sep = ".")

# Testing the equality of MN and NA frequencies between cases and controls using the Wilcoxon signed rank test with continuity correction:
# Ommiting the last observation of cases so the test can be truly paired (i,j) 

# For micronucleated cells...
mn.test <- wilcox.test(case.micronuclei, control.micronuclei, 
                       exact = FALSE,
                       alternative = "two.sided", 
                       conf.int = TRUE,
                       conf.level = 0.95, 
                       paired = TRUE)

# For binucleated cells...
bn.test <- wilcox.test(case.binucleated, control.binucleated, 
                       exact = FALSE,
                       alternative = "two.sided", 
                       conf.int = TRUE,
                       conf.level = 0.95, 
                       paired = TRUE)

# For lobulated nuclei cells...
ln.test <- wilcox.test(case.lobulated, control.lobulated, 
                       exact = FALSE,
                       alternative = "two.sided", 
                       conf.int = TRUE,
                       conf.level = 0.95, 
                       paired = TRUE)

# For pyknotic cells...
pk.test <- wilcox.test(case.pyknotic, control.pyknotic, 
                       exact = FALSE,
                       alternative = "two.sided", 
                       conf.int = TRUE,
                       conf.level = 0.95, 
                       paired = TRUE)

# For condensed-chromatin-cells...
cc.test <- wilcox.test(case.condensed, control.condensed, 
                       exact = FALSE,
                       alternative = "two.sided", 
                       conf.int = TRUE,
                       conf.level = 0.95, 
                       paired = TRUE)

# For karyorrhectic cells...
kr.test <- wilcox.test(case.karyorrhexis, control.karyorrhexis, 
                       exact = FALSE,
                       alternative = "two.sided", 
                       conf.int = TRUE,
                       conf.level = 0.95, 
                       paired = TRUE)

# For karyolytic cells...
kl.test <- wilcox.test(case.karyolysis, control.karyolysis, 
                       exact = FALSE,
                       alternative = "two.sided", 
                       conf.int = TRUE,
                       conf.level = 0.95, 
                       paired = TRUE)


# Linear regressions of significant NA against non-nuclear covariates (* will be included in the model)
# condensed 

# Creating the formulas 
cc <- formula(condensed ~ case)
bn <- formula(binucleated ~ case)

# Attaching the original data set, since the model already transforms the data. 
source("http://files.figshare.com/2037299/cleaning_data.R", echo = FALSE)
attach(data)

# Initial quasi-poisson models
bn.fit <- glm(bn, family = quasipoisson(link = "sqrt"),
              model = TRUE, method = "glm.fit",
              x = FALSE, y = TRUE)

cc.fit <- glm(cc, family = quasipoisson(link = "sqrt"),
              model = TRUE, method = "glm.fit",
              x = FALSE, y = TRUE)

# Adding and dropping significant terms
# Stepwise forward selection of terms for bn.fit
add1(bn.fit, scope = ~.+age+exp.years+bmi+smoke+drink+amalgam, test = "F") 
bn.fit <- update(bn.fit, ~.+smoke) # adding smoke variable
add1(bn.fit, scope = ~.+age+exp.years+bmi+drink+amalgam, test = "F") 
drop1(bn.fit, test = "F")  # backward step to check the model
# Stepwise forward selection of terms for cc.fit
add1(cc.fit, scope = ~.*age*amalgam, test = "F") # no variables to add

# Summarise the models
summary(bn.fit)
summary(cc.fit)

# Preparing figures for PLoS
# Installing the required packages
if(!require(sjPlot)){
    install.packages("sjPlot")
}
library(sjPlot)

if(!require(extrafont)){
    install.packages("extrafont") # to get the Arial font
}
library(extrafont)
# takes a few minutes, uncomment to run
#font_import(); y  

# Plotting the BN model
bn.plot <- sjp.glm(bn.fit, type = "glm", 
                   sortOdds = TRUE, 
                   title = "Odds Ratios of Binucleated Cells",
                   axisLabels.y = c("Welding Fumes", "Tobacco Use", "Age", "Intercept"),
                   axisTitle.x = "Odds Ratios", axisLimits = NULL,
                   breakTitleAt = 50, breakLabelsAt = 25, gridBreaksAt = 0.5,
                   transformTicks = FALSE, 
                   geom.size = 1.5, geom.colors = "Set1",
                   hideErrorBars = FALSE, 
                   interceptLineType = 2,
                   interceptLineColor = "grey70", 
                   coord.flip = TRUE, 
                   showIntercept = TRUE,
                   showAxisLabels.y = TRUE, showValueLabels = TRUE, 
                   labelDigits = 2,
                   showPValueLabels = TRUE, 
                   showModelSummary = FALSE, 
                   facet.grid = FALSE,
                   show.se = TRUE, 
                   showOriginalModelOnly = TRUE, 
                   printPlot = TRUE)
bn.plot$plot + theme_bw(base_size = 12, base_family = "OpenSymbol") # Formatting

# Plotting the CC model
cc.plot <- sjp.glm(cc.fit, type = "glm", 
                   sortOdds = TRUE, 
                   title = "Odds Ratios of Condensed-Chromatin Cells",
                   axisLabels.y = c("Welding Fumes", "Tobacco Use", "Age", "Intercept"),
                   axisTitle.x = "Odds Ratios", axisLimits = NULL,
                   breakTitleAt = 50, breakLabelsAt = 25, gridBreaksAt = 0.5,
                   transformTicks = FALSE, 
                   geom.size = 1.5, geom.colors = "Set1",
                   hideErrorBars = FALSE, 
                   interceptLineType = 2,
                   interceptLineColor = "grey70", 
                   coord.flip = TRUE, 
                   showIntercept = TRUE,
                   showAxisLabels.y = TRUE, showValueLabels = TRUE, 
                   labelDigits = 2,
                   showPValueLabels = TRUE, 
                   showModelSummary = FALSE, 
                   facet.grid = FALSE,
                   show.se = TRUE, 
                   showOriginalModelOnly = TRUE, 
                   printPlot = TRUE)
cc.plot$plot + theme_bw(base_size = 12, base_family = "OpenSymbol") # Formatting

