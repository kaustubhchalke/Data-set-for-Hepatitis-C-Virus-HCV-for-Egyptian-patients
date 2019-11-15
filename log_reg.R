#### Logistic Regression #####
#install.packages("cowplot", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(cowplot)
library(dplyr)
library(ggplot2)
data <- read.csv("C:/Users/saiprasad/Desktop/Fall 2019/Multi analysis/MVA/Project/Dataset/HCV-EGY-Data.csv")
attach(data)
Survivorship = data$Survivorship <- if_else( RNA.EOT>= 400000 , 'NC','C')
cbind(data.frame(Survivorship),data)

data$Survivorship <- as.factor(data$Survivorship)
#####################################
##
## Reformat the data so that it is
## 1) Easy to use (add nice column names)
## 2) Interpreted correctly by glm()..
##
#####################################
head(data) # you see data, but no column names
str(data)
# this shows that we need to tell R which columns contain factors it also shows us that there are some missing values. There are "?"s
## in the dataset. These are in the "ca" and "thal" columns. First, convert "?"s to NAs...
data[data == "?"] <- NA
## Now add factors for variables that are factors and clean up the factors that had missing data...
data[data$Gender == 1,]$Gender <- "M"
data[data$Gender == 2,]$Gender <- "F"
data$Gender <- as.factor(data$Gender)
data[data$Fever == 1,]$Fever <- "No"
data[data$Fever == 2,]$Fever <- "Yes"
data$Fever <- as.factor(data$Fever)
data[data$Nausea.Vomting == 1,]$Nausea.Vomting <- "No"
data[data$Nausea.Vomting == 2,]$Nausea.Vomting <- "Yes"
data$Nausea.Vomting <- as.factor(data$Nausea.Vomting)
data[data$Headache == 1,]$Headache <- "No"
data[data$Headache == 2,]$Headache <- "Yes"
data$Headache <- as.factor(data$Headache)
data[data$Diarrhea == 1,]$Diarrhea <- "No"
data[data$Diarrhea == 2,]$Diarrhea <- "Yes"
data$Diarrhea <- as.factor(data$Diarrhea)
data[data$Fatigue...generalized.bone.ache == 1,]$Fatigue...generalized.bone.ache <- "No"
data[data$Fatigue...generalized.bone.ache == 2,]$Fatigue...generalized.bone.ache <- "Yes"
data$Fatigue...generalized.bone.ache <- as.factor(data$Fatigue...generalized.bone.ache)
data[data$Jaundice == 1,]$Jaundice <- "No"
data[data$Jaundice == 2,]$Jaundice <- "Yes"
data$Jaundice <- as.factor(data$Jaundice)
data[data$Epigastric.pain == 1,]$Epigastric.pain <- "No"
data[data$Epigastric.pain == 2,]$Epigastric.pain <- "Yes"
data$Epigastric.pain <- as.factor(data$Epigastric.pain)
data[data$Baselinehistological.staging == 1,]$Baselinehistological.staging <- "Portal Fibrosis"
data[data$Baselinehistological.staging == 2,]$Baselinehistological.staging<- "Few Septa"
data[data$Baselinehistological.staging == 3,]$Baselinehistological.staging <- "Many Septa "
data[data$Baselinehistological.staging == 4,]$Baselinehistological.staging <- "Cirrhosis"
data$Baseline.histological.Grading <- as.factor(data$Baseline.histological.Grading)
data$Baselinehistological.staging <- as.factor(data$Baselinehistological.staging)
str(data)

###################################
xtabs(~ Survivorship + Gender, data=data)
xtabs(~ Survivorship + Fever, data=data)
xtabs(~ Survivorship + Nausea.Vomting, data=data)
xtabs(~ Survivorship + Headache, data=data)
xtabs(~ Survivorship + Diarrhea, data=data)
xtabs(~ Survivorship + Fatigue...generalized.bone.ache, data=data)
xtabs(~ Survivorship + Jaundice, data=data)
xtabs(~ Survivorship + Epigastric.pain, data=data)
xtabs(~ Survivorship + Baselinehistological.staging, data=data)
## Now we are ready for some logistic regression. First we'll create a very
## simple model that uses sex to predict heart disease
##
xtabs(~ Survivorship + Gender, data=data)
## Most of the females are healthy and most of the males are unhealthy.
## Being female is likely to decrease the odds in being unhealthy.
##    In other words, if a sample is female, the odds are against it that it
##    will be unhealthy
## Being male is likely to increase the odds in being unhealthy...
##    In other words, if a sample is male, the odds are for it being unhealthy
logistic_simple <- glm(Survivorship ~ Gender, data=data, family="binomial")
summary(logistic_simple)
## The intercept is the log(odds) a female will be unhealthy. This is because
## female is the first factor in "sex" (the factors are ordered,
## alphabetically by default,"female", "male")
## Now let's look at the second coefficient...
##   sexM        1.2737     0.2725   4.674 2.95e-06 ***
##
## sexM is the log(odds ratio) that tells us that if a sample has sex=M, the
## odds of being unhealthy are, on a log scale, 1.27 times greater than if
## a sample has sex=F.
female.log.odds <- log(253 /425)
female.log.odds
# Now you know how these are calculated
male.log.odds.ratio <- log((229 / 478) / (253/425))
male.log.odds.ratio
## Now calculate the overall "Pseudo R-squared" and its p-value
## NOTE: Since we are doing logistic regression...
## Null devaiance = 2*(0 - LogLikelihood(null model))
##               = -2*LogLikihood(null model)
## Residual deviance = 2*(0 - LogLikelihood(proposed model))
##                   = -2*LogLikelihood(proposed model)
ll.null <- logistic_simple$null.deviance/-2
ll.proposed <- logistic_simple$deviance/-2
ll.null
ll.proposed
## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll.null - ll.proposed) / ll.null
## chi-square value = 2*(LL(Proposed) - LL(Null))
## p-value = 1 - pchisq(chi-square value, df = 2-1)
1 - pchisq(2*(ll.proposed - ll.null), df=1)
1 - pchisq((logistic_simple$null.deviance - logistic_simple$deviance), df=1)
## Lastly, let's  see what this logistic regression predicts, given
## that a patient is either female or male (and no other data about them).
predicted.data <- data.frame(probability.of.Survivorship=logistic_simple$fitted.values,Gender=data$Gender)
predicted.data
## We can plot the data...
ggplot(data=predicted.data, aes(x=Gender, y=probability.of.Survivorship)) +
  geom_point(aes(color=Gender), size=5) +
  xlab("Gender") +
  ylab("Predicted probability of getting HCV Disease")
## Since there are only two probabilities (one for females and one for males),
## we can use a table to summarize the predicted probabilities.
xtabs(~ probability.of.Survivorship + Gender, data=predicted.data)
#####################################
##
## Now we will use all of the data available to predict heart disease. This is not the best way to do this
##
#####################################
logistic <- glm(Survivorship ~ ., data=data, family="binomial")
summary(logistic)
## Now calculate the overall "Pseudo R-squared" and its p-value
ll.null <- logistic$null.deviance/-2
ll.proposed <- logistic$deviance/-2
## McFadden's Pseudo R^2 = [ LL(Null) - LL(Proposed) ] / LL(Null)
(ll.null - ll.proposed) / ll.null
## The p-value for the R^2
1 - pchisq(2*(ll.proposed - ll.null), df=(length(logistic$coefficients)-1))
## now we can plot the data
predicted.data <- data.frame(probability.of.Survivorship=logistic$fitted.values,Survivorship=data$Survivorship)
predicted.data <- predicted.data[order(predicted.data$probability.of.Survivorship, decreasing=FALSE),]
predicted.data$rank <- 1:nrow(predicted.data)
## Lastly, we can plot the predicted probabilities for each sample having
## heart disease and color by whether or not they actually had heart disease
ggplot(data=predicted.data, aes(x=rank, y=probability.of.Survivorship)) +
  geom_point(aes(color=Survivorship), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability of getting HCV disease")
# Few packages for confusion matrix. Lets look at them one by one
#install.packages("regclass", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(regclass)
confusion_matrix(logistic)
#install.packages("caret", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(caret)
pdata <- predict(logistic,newdata=data,type="response" )
pdata
data$Survivorship
#pdataF <- as.factor(ifelse(test=as.numeric(pdata>0.5) == 0, yes="Healthy", no="Unhealthy"))
#install.packages("e1071", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(e1071)
#confusionMatrix(pdataF, data$Survivorship)
#install.packages("pROC", lib="/Library/Frameworks/R.framework/Versions/3.5/Resources/library")
library(pROC)
roc(data$Survivorship,logistic$fitted.values,plot=TRUE)
par(pty = "s")
roc(data$Survivorship,logistic$fitted.values,plot=TRUE)
## NOTE: By default, roc() uses specificity on the x-axis and the values range
## from 1 to 0. This makes the graph look like what we would expect, but the
## x-axis itself might induce a headache. To use 1-specificity (i.e. the
## False Positive Rate) on the x-axis, set "legacy.axes" to TRUE.
roc(data$Survivorship,logistic$fitted.values,plot=TRUE, legacy.axes=TRUE)
roc(data$Survivorship,logistic$fitted.values,plot=TRUE, legacy.axes=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage")
roc(data$Survivorship,logistic$fitted.values,plot=TRUE, legacy.axes=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4)
roc(data$Survivorship,logistic$fitted.values,plot=TRUE, legacy.axes=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4)
## If we want to find out the optimal threshold we can store the
## data used to make the ROC graph in a variable...
roc.info <- roc(data$Survivorship, logistic$fitted.values, legacy.axes=TRUE)
str(roc.info)
roc.df <- data.frame(tpp=roc.info$sensitivities*100, ## tpp = true positive percentage
                     fpp=(1 - roc.info$specificities)*100, ## fpp = false positive precentage
                     thresholds=roc.info$thresholds)
roc.df
head(roc.df) ## head() will show us the values for the upper right-hand corner of the ROC graph, when the threshold is so low
## (negative infinity) that every single sample is called "obese".
## Thus TPP = 100% and FPP = 100%
tail(roc.df) ## tail() will show us the values for the lower left-hand corner
## of the ROC graph, when the threshold is so high (infinity)
## that every single sample is called "not obese".
## Thus, TPP = 0% and FPP = 0%
## now let's look at the thresholds between TPP 60% and 80%
roc.df[roc.df$tpp > 60 & roc.df$tpp < 80,]
roc(data$Survivorship,logistic$fitted.values,plot=TRUE, legacy.axes=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, percent=TRUE)
roc(data$Survivorship,logistic$fitted.values,plot=TRUE, legacy.axes=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, percent=TRUE, print.auc=TRUE)
roc(data$Survivorship,logistic$fitted.values,plot=TRUE, legacy.axes=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, percent=TRUE, print.auc=TRUE, partial.auc=c(100, 90), auc.polygon = TRUE, auc.polygon.col = "#377eb822", print.auc.x=45)
# Lets do two roc plots to understand which model is better
roc(data$Survivorship, logistic_simple$fitted.values, plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#377eb8", lwd=4, print.auc=TRUE)
# Lets add the other graph
plot.roc(data$Survivorship, logistic$fitted.values, percent=TRUE, col="#4daf4a", lwd=4, print.auc=TRUE, add=TRUE, print.auc.y=40)
legend("bottomright", legend=c("Simple", "Non Simple"), col=c("#377eb8", "#4daf4a"), lwd=4) # Make it user friendly





