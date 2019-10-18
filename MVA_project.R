#Importing the Hepatatis c Dataset
HCV= read.csv("HCV-Egy-Data.csv")
HCV

#Summary
attach(HCV)
summary(HCV)

#Dimensions of the data set
NROW(HCV)
NCOL(HCV)

#Displaying the column names of the dataset
colnames(HCV)

#Another menthod for dimensions
dim(HCV)

#Preprocessing data was done but did'nt find any discrepancies.
na= is.na(HCV)
na
any(is.na(HCV))

#Displaying the first six rows of the datasets
head(HCV)
tail(HCV)

#Differentiating data set based on gender
Gen_male = HCV[HCV$Gender== '1',]
Gen_female = HCV[HCV$Gender=='2',]

#Exploring symptoms
#**********************Male Data Exploration*****************
#Fever
Fev_male = Gen_male[Gen_male$Fever == '2',]
Fev_male
summary(Fev_male)

#Vomiting and Nausea
Nau_male = Gen_male[Gen_male$Nausea.Vomting =='2',]
Nau_male
summary(Nau_male)

#Fatigue
Fat_male = Gen_male[Gen_male$Fatigue...generalized.bone.ache =='2',]
Fat_male
summary(Fat_male)

#Jaundice
Jau_male = Gen_male[Gen_male$Jaundice  =='2',]
Jau_male
summary(Jau_male)

#Stomack pain
sto_male = Gen_female[Gen_female$Epigastric.pain =='2',]
sto_male
summary(sto_male)

#*********************Female Data Exploration*****************
#Fever
Fev_female = Gen_female[Gen_female$Fever == '2',]
Fev_female
summary(Fev_female)

#Vomiting and Nausea
Nau_female = Gen_female[Gen_female$Nausea.Vomting =='2',]
Nau_female
summary(Nau_female)

#Fatigue
Fat_female = Gen_female[Gen_female$Fatigue...generalized.bone.ache =='2',]
Fat_female
summary(Fat_female)

#Jaundice
Jau_female = Gen_female[Gen_female$Jaundice  =='2',]
Jau_female
summary(Jau_female)

#Stomack pain
sto_female = Gen_female[Gen_female$Epigastric.pain =='2',]
sto_female
summary(sto_female)



#CORRELATION, COVARIANCE AND DISTANCE
covariance<-cov(HCV[,c(11:16,23)]) #variamce-covariance matrix created
correlation<-cor(HCV[,c(11:16,23)]) #standardized
#colmeans
cm<-colMeans(HCV[,c(11:16,23)])
distance<-dist(scale(HCV[,c(11:16,23)],center=FALSE))
#Calculating di(generalized distance for all observations of our data)
#before that first extract all numeric variable in a dataframe
x<-HCV[,c(11:16,23)]
d <- apply(x, MARGIN = 1, function(x) + t(x - cm) %*% solve(covariance) %*% (x - cm))



#Exlporation of the data for high chances of HCV Infection
#Here RNA.base value if it is more than 700000 units then virus is detected in high quantity.
#Here ALT.1 if value is greater than 57 then it is not normal.
#we sorted the data on these two components.

library(dplyr)
HCV_male =  HCV %>% filter(Gender == 1 & RNA.Base>= 700000 & ALT.1 >= 57)
HCV_male

HCV_female =  HCV %>% filter(Gender == 2 & RNA.Base>= 700000 & ALT.1 >= 57)
HCV_female


#Box Plot
boxplot(RNA.Base, main="RNA.BASE Box plot",yaxt="n", xlab="RNA", horizontal=TRUE)
boxplot(ALT.1, main="ALT.1 Box plot",yaxt="n", xlab="ALT", horizontal=TRUE)
boxplot(WBC, main="WBC Box plot",yaxt="n", xlab="WBC", horizontal=TRUE)
boxplot(RBC, main="WBC Box plot",yaxt="n", xlab="RBC", horizontal=TRUE)
boxplot(AST.1, main="AST.1 Box plot",yaxt="n", xlab="AST", horizontal=TRUE)


#plotting, Are they in a straight line.  
#Male Plotting of the dataset is done for five different attributes.
qqnorm(HCV_male[,"RNA.Base"], main = "RNA.Base"); qqline(HCV_male[,"RNA.Base"])
qqnorm(HCV_male[,"ALT.1"], main = "ALT.1"); qqline(HCV_male[,"ALT.1"])
qqnorm(HCV_male[,"WBC"], main = "WBC"); qqline(HCV_male[,"WBC"])
qqnorm(HCV_male[,"RBC"], main = "RBC"); qqline(HCV_male[,"RBC"])
qqnorm(HCV_male[,"AST.1"], main = "AST.1"); qqline(HCV_male[,"AST.1"])

#Female, Are they in a straight line.
#FeMale Plotting of the dataset is done for five different attributes.
qqnorm(HCV_female[,"RNA.Base"], main = "RNA.Base"); qqline(HCV_female[,"RNA.Base"])
qqnorm(HCV_female[,"ALT.1"], main = "ALT.1"); qqline(HCV_female[,"ALT.1"])
qqnorm(HCV_female[,"WBC"], main = "WBC"); qqline(HCV_female[,"WBC"])
qqnorm(HCV_female[,"RBC"], main = "RBC"); qqline(HCV_female[,"RBC"])
qqnorm(HCV_female[,"AST.1"], main = "AST.1"); qqline(HCV_female[,"AST.1"])


#Visualisatiom
#Chiplot
library(HSAUR2)
library(tools)
library(MVA)

#Chiplot
#For male data
with(HCV_male, chiplot(RNA.Base, ALT.1))

#For Female Data
with(HCV_female, chiplot(RNA.Base, ALT.1))



library(GGally)

ggpairs(HCV_male, columns=c("AST.1","RNA.EOT","WBC","ALT.1", "RBC"), color="Survivorship")
ggpairs(HCV_female, columns=c("AST.1","RNA.EOT","WBC","ALT.1", "RBC"), color="Survivorship")
summary(lm(data = HCV , RNA.EOT~Age))
summary(lm(data = HCV , RNA.EOT~Gender))
summary(lm(data = HCV , RNA.EOT~WBC))
summary(lm(data = HCV , RNA.EOT~ALT.1))
cor(HCV)


#PRINCIPLE COMPONENT ANALYSIS**************ASSIGNMENT  3***********************************************************

#Importing the Hepatatis c Dataset
HCV_NEW= read.csv("HCV-Egy-Data.csv")
HCV<-  HCV_NEW
attach(HCV)
library(dplyr)
HCV$Survivorship <- if_else( RNA.EOT>= 400000 , 'NC','C')
Survivorship
cbind(Survivorship,HCV)
#Summary
summary(HCV)
#Dimensions of the data set
NROW(HCV)
NCOL(HCV)

#Another menthod for dimensions
dim(HCV)

#Preprocessing data was done but did'nt find any discrepancies.
na= is.na(HCV)
na
any(is.na(HCV))

#Displaying the first six rows of the datasets
head(HCV)
tail(HCV)

correlation<-cor(HCV[1:29]) #standardized
correlation

# Using prcomp to compute the principal components (eigenvalues and eigenvectors). With scale=TRUE, variable means are set to zero, and variances set to one
hcv_pca <- prcomp(HCV[1:29],scale=TRUE)
hcv_pca
summary(hcv_pca)
# sample scores stored in sparrows_pca$x
# singular values (square roots of eigenvalues) stored in sparrow_pca$sdev
# loadings (eigenvectors) are stored in sparrows_pca$rotation
# variable means stored in sparrows_pca$center
# variable standard deviations stored in sparrows_pca$scale
# A table containing eigenvalues and %'s accounted, follows
# Eigenvalues are sdev^2
(eigen_hcv <- hcv_pca$sdev^2)
names(eigen_hcv) <- paste("PC",1:29,sep="")
eigen_hcv
sumlambdas <- sum(eigen_hcv)
sumlambdas
propvar <- eigen_hcv/sumlambdas
propvar
cumvar_hcv <- cumsum(propvar)
cumvar_hcv
matlambdas <- rbind(eigen_hcv,propvar,cumvar_hcv)
rownames(matlambdas) <- c("Eigenvalues","Prop. variance","Cum. prop. variance")
round(matlambdas,29)
summary(hcv_pca)
hcv_pca$rotation
print(hcv_pca)
# Sample scores stored in hcv_pca$x
hcv_pca$x
############################################### Identifying the scores by their survival status
hcvtyp_pca <- cbind(data.frame(Survivorship),hcv_pca$x)
hcvtyp_pca
# Means of scores for all the PC's classified by Survival status
tabmeansPC <- aggregate(hcvtyp_pca[,2:30],by=list(Survivorship=HCV$Survivorship),mean)
tabmeansPC
tabmeansPC <- tabmeansPC[rev(order(tabmeansPC$Survivorship)),]
tabmeansPC
tabfmeans <- t(tabmeansPC[,-1])
tabfmeans
colnames(tabfmeans) <- t(as.vector(tabmeansPC[1]))
tabfmeans
# Standard deviations of scores for all the PC's classified by Survival status
tabsdsPC <- aggregate(hcvtyp_pca[,2:30],by=list(Survivorship=HCV$Survivorship),sd)
tabfsds <- t(tabsdsPC[,-1])
colnames(tabfsds) <- t(as.vector(tabsdsPC[1]))
tabfsds

#t-test
t.test(PC1~HCV$Survivorship,data=hcvtyp_pca)
t.test(PC2~HCV$Survivorship,data=hcvtyp_pca)
t.test(PC3~HCV$Survivorship,data=hcvtyp_pca)
t.test(PC4~HCV$Survivorship,data=hcvtyp_pca)
t.test(PC5~HCV$Survivorship,data=hcvtyp_pca)
t.test(PC6~HCV$Survivorship,data=hcvtyp_pca)
t.test(PC7~HCV$Survivorship,data=hcvtyp_pca)
t.test(PC8~HCV$Survivorship,data=hcvtyp_pca)
t.test(PC9~HCV$Survivorship,data=hcvtyp_pca)
t.test(PC10~HCV$Survivorship,data=hcvtyp_pca)
t.test(PC11~HCV$Survivorship,data=hcvtyp_pca)
t.test(PC12~HCV$Survivorship,data=hcvtyp_pca)
t.test(PC13~HCV$Survivorship,data=hcvtyp_pca)
t.test(PC14~HCV$Survivorship,data=hcvtyp_pca)
t.test(PC15~HCV$Survivorship,data=hcvtyp_pca)
t.test(PC16~HCV$Survivorship,data=hcvtyp_pca)
t.test(PC17~HCV$Survivorship,data=hcvtyp_pca)
t.test(PC18~HCV$Survivorship,data=hcvtyp_pca)
t.test(PC19~HCV$Survivorship,data=hcvtyp_pca)
t.test(PC20~HCV$Survivorship,data=hcvtyp_pca)
t.test(PC21~HCV$Survivorship,data=hcvtyp_pca)
t.test(PC22~HCV$Survivorship,data=hcvtyp_pca)
t.test(PC23~HCV$Survivorship,data=hcvtyp_pca)
t.test(PC24~HCV$Survivorship,data=hcvtyp_pca)
t.test(PC25~HCV$Survivorship,data=hcvtyp_pca)
t.test(PC26~HCV$Survivorship,data=hcvtyp_pca)
t.test(PC27~HCV$Survivorship,data=hcvtyp_pca)
t.test(PC28~HCV$Survivorship,data=hcvtyp_pca)
t.test(PC29~HCV$Survivorship,data=hcvtyp_pca)


# F ratio tests
var.test(PC1~HCV$Survivorship,data=hcvtyp_pca)
var.test(PC2~HCV$Survivorship,data=hcvtyp_pca)
var.test(PC3~HCV$Survivorship,data=hcvtyp_pca)
var.test(PC4~HCV$Survivorship,data=hcvtyp_pca)
var.test(PC5~HCV$Survivorship,data=hcvtyp_pca)
var.test(PC6~HCV$Survivorship,data=hcvtyp_pca)
var.test(PC7~HCV$Survivorship,data=hcvtyp_pca)
var.test(PC8~HCV$Survivorship,data=hcvtyp_pca)
var.test(PC9~HCV$Survivorship,data=hcvtyp_pca)
var.test(PC10~HCV$Survivorship,data=hcvtyp_pca)
var.test(PC11~HCV$Survivorship,data=hcvtyp_pca)
var.test(PC12~HCV$Survivorship,data=hcvtyp_pca)
var.test(PC13~HCV$Survivorship,data=hcvtyp_pca)
var.test(PC14~HCV$Survivorship,data=hcvtyp_pca)
var.test(PC15~HCV$Survivorship,data=hcvtyp_pca)
var.test(PC16~HCV$Survivorship,data=hcvtyp_pca)
var.test(PC17~HCV$Survivorship,data=hcvtyp_pca)
var.test(PC18~HCV$Survivorship,data=hcvtyp_pca)
var.test(PC19~HCV$Survivorship,data=hcvtyp_pca)
var.test(PC20~HCV$Survivorship,data=hcvtyp_pca)
var.test(PC21~HCV$Survivorship,data=hcvtyp_pca)
var.test(PC22~HCV$Survivorship,data=hcvtyp_pca)
var.test(PC23~HCV$Survivorship,data=hcvtyp_pca)
var.test(PC24~HCV$Survivorship,data=hcvtyp_pca)
var.test(PC25~HCV$Survivorship,data=hcvtyp_pca)
var.test(PC26~HCV$Survivorship,data=hcvtyp_pca)
var.test(PC27~HCV$Survivorship,data=hcvtyp_pca)
var.test(PC28~HCV$Survivorship,data=hcvtyp_pca)
var.test(PC29~HCV$Survivorship,data=hcvtyp_pca)

# Plotting the scores for the first and second components
plot(hcvtyp_pca$PC1, hcvtyp_pca$PC2,pch=ifelse(hcvtyp_pca$Survivorship == "S",1,16),xlab="PC1", ylab="PC2", main="49 HCV against values for PC1 & PC2")
abline(h=0)
abline(v=0)
legend("bottomleft", legend=c("Cured","Not_Cured"), pch=c(1,16))
plot(eigen_hcv, xlab = "Component number", ylab = "Component variance", type = "l", main = "Scree diagram")
plot(log(eigen_hcv), xlab = "Component number",ylab = "log(Component variance)", type="l",main = "Log(eigenvalue) diagram")
print(summary(hcv_pca))
View(hcv_pca)
diag(cov(hcv_pca$x))
xlim <- range(hcv_pca$x[,1])
hcv_pca$x[,1]
hcv_pca$x
plot(hcv_pca$x,xlim=xlim,ylim=xlim)
hcv_pca$rotation[,1]
hcv_pca$rotation
plot(HCV[,-1])
hcv_pca$x
plot(hcv_pca)
#get the original value of the data based on PCA
center <- hcv_pca$center
scale <- hcv_pca$scale
new_HCV1 <- as.matrix(HCV[,-1])
new_HCV1
drop(scale(new_HCV1,center=center, scale=scale)%*%hcv_pca$rotation[,1])
predict(hcv_pca)[,1]
#The aboved two gives us the same thing. predict is a good function to know.
out <- sapply(1:5, function(i){plot(HCV$Survivorship,hcv_pca$x[,i],xlab=paste("PC",i,sep=""),ylab="Survivorship")})
pairs(hcv_pca$x[,1:5], ylim = c(-6,4),xlim = c(-6,4),panel=function(x,y,...){text(x,y,HCV$Survivorship)})










