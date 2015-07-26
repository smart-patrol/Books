


install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)



??fuel
data(FuelEconomy)
library(lattice)

str(cars2010)
names(cars2010)

cars2010$EngDispl
cars2010$FE

plot(cars2010$EngDispl, cars2010$FE)

mod <- lm(EngDispl ~ FE, data=cars2010)
summary(mod)

attach(cars2010)
plot( FE, EngDispl )
abline(mod, col="red")
detach(cars2010)

pred <- predict(mod, newdata=cars2011, 
               type="response")

plot( pred, cars2011$EngDispl)
ablni(mod, col="red")

mod <- lm(FE ~ EngDispl  + I(EngDispl)^2, data=cars2010)
summary(mod)

plot(  EngDispl, FE )
abline(mod, col="red")

#------------------------------------------
# Chapter 3
getPackages(2:3)

apropos("confusion")
data(segmentationOriginal)

seg <- subset(segmentationOriginal, Case == "Train")

str(seg)
dim(seg)

# save into seperate vectors then removed
cell <- seg$Cell
class <- seg$Class
seg <- seg[,-(1:3)]

# satus are simply binary repoructions of other vars - remove
stat_col <- grep("Status", names(seg))
stat_col
seg <- seg[, -stat_col]

# went from 119 to 58
dim(seg)

# transform features
library(e1071)
skewness(seg$AngleCh1)
skewed <- apply(seg, 2, skewness)
skewed

library(caret)
Ch1Area_Trans <- BoxCoxTrans(seg$AreaCh1)
Ch1Area_Trans
head(seg$AreaCh1)
predict(Ch1Area_Trans , head(seg$AreaCh1))
(819^(-.9)-1)/(-.9)

# using PCA
pca_ob <- prcomp(seg, center = T, scale = T)
perc_var <- pca_ob$sd^2/sum(pca_ob$sd^2)*100
perc_var[1:3]
head(pca_ob$x[,1:5])  #values stored as x
head(pca_ob$rotation[,1:5]) #rotation stores the loadings

head(spatialSign(seg))

# all transfroms 
?preProcess
trans <- preProcess(seg, 
            method = c("YeoJohnson","center","scale","pca","knnImpute"))
trans
t_seg <- predict(trans, seg)
head(t_seg[,1:5])

# filtering

nearZeroVar(seg)
rho <- cor(seg)

dim(rho)
rho[1:4, 1:4]
# visualize the correlation structure
library(corrplot)
corrplot(rho, order = "hclust")
# filter on corr
hi_corr <- findCorrelation(rho, cutoff = 0.75)
length(hi_corr)
hi_corr #33 intercorrelated
seg <- seg[ ,-hi_corr] # 58 to 25

#---------------------------------------------------------------------
# Exercises
# 3.1
# (a) Using visualizations, explore the predictor variables to understand their
# distributions as well as the relationships between predictors.
# (b) Do there appear to be any outliers in the data? Are any predictors skewed?
# (c) Are there any relevant transformations of one or more predictors that
# might improve the classification model?

library(mlbench)
data(Glass)
str(Glass)
attach(Glass)

plot(Type)
prop.table(table(Type))*100

n = length(Glass)-1


for(i in 1:n){
  
  y = names(Glass)[i]
  
  #boxplot( Glass[,i] ~ Glass$Type , xlab = "Type", ylab=y)
  
  hist(Glass[,i] ,    main = paste("Histogram of" , y))
  
  qqnorm(Glass[,i] ,   main = paste("Normplot of" , y))
  
}

# near normal with outliers : Si , AI, NA and RI


# K, BA, CA are highest skwewed to left
# MG is right skewed
apply(Glass[,-10], 2, skewness)

#those with high peaks : K, CA, BA
apply(Glass[,-10], 2, kurtosis)

rho <- cor(Glass[,-10])
round(abs(rho),2)
# strongest correlation between RI and Ca at 0.8
# some 0.4 - 0.3 present
# notabely Ca and Mg - Ba and MB - Mg and Al -  all MG!
corrplot(rho)
plot(RI, Ca )
splom(Glass[,-10])

summary(Glass)
detach(Glass)
# all variables could benefit from normality transform -  even the ones with outliers
# outliers would need to be kept in or taken out
# Mg and Ca/RI would need to be excluded
# center and scale prior to modeling

nearZeroVar(Glass)
hi_corr <- findCorrelation(rho, cutoff = 0.75)
hi_corr
Glass <- Glass[,-hi_corr]
names(Glass)

trans <- preProcess(Glass[ ,names(Glass) != "Type"], 
                    method = c("YeoJohnson","center","scale"))
trans
t_gl <- predict(trans, Glass)
head(t_gl)
t_gl$Type <- Glass$Type

names(t_gl)

n = length(t_gl)-1

for(i in 1:n){
  
  y = names(t_gl)[i]
  
  qqnorm(t_gl[,i] ,   main = paste("Normplot of" , y))
  
}

#---------------------------------------------------------------------
# Exercises
# 3.2
library(mlbench)
data(Soybean)

#(a) Investigate the frequency distributions for the categorical predictors. Are
#any of the distributions degenerate in the ways discussed earlier in this
#chapter?
#(b) Roughly 18 % of the data are missing. Are there particular predictors that
#are more likely to be missing? Is the pattern of missing data related to
#the classes?
#(c) Develop a strategy for handling missing data, either by eliminating
#predictors or imputation.

str(Soybean)

n = length(Soybean)

for(i in 1:n){
  
  y = names(Soybean)[i]
  tbl = prop.table(table(Soybean[,i]))*100
  barplot(tbl, main = paste("Plot of" , y))
  
}

# a) some appear to have little variance

sum(is.na(Soybean))

sapply(Soybean, function(x) sum(is.na(x)))

dim(Soybean)

# 1/6 or so missing:   seed.tmt, germ, hail, lodging, leaf.mild,
# fruiting.bodies, sever, fruit.spots, seed.discolor, seed.size, shriveling
# Anything 15% or above I would omit or...

# since these are factors  - treat it as a new categorical variable !
?Soybean

levels(Soybean$roots)
Soybean$roots <- as.character(Soybean$roots)
Soybean$roots <- ifelse( is.na(Soybean$roots) == T, "miss", Soybean$roots)
Soybean$roots <- as.factor(Soybean$roots)  
print(levels(Soybean$roots))


n = length(Soybean)

for(i in 1:n){  

  Soybean[,i] <- as.character(Soybean[,i])
  Soybean[,i] <- ifelse( is.na(Soybean[,i]) == T, "miss", Soybean[,i])
  Soybean[,i] <- as.factor(Soybean[,i])  
  print(levels(Soybean[,i]))
  
}

summary(Soybean)

#---------------------------------------------------------------------
# Exercises
# 3.3
# (b) Do any of the individual predictors have degenerate distributions?
# (c) Generally speaking, are there strong relationships between the predictor data? If so, how could correlations in the predictor set be reduced?
# Does this have a dramatic effect on the number of predictors available for
# modeling?

library(caret)
data(BloodBrain)

?BloodBrain
str(bbbDescr)

hist(logBBB)

# doing something different
library(psych)

?psych

dscp <- as.data.frame(round(describe(bbbDescr),2))
dscp$mm_diff <- abs(dscp$mean - dscp$median)

s_dscp <-
  subset(dscp, mm_diff > 2 & abs(skew) > 1
           , c(vars,mean, sd, median, skew, kurtosis))
#View(s_dscp)

row.names(s_dscp) # all of these have degenenrate distrobutions

#pairs.panels()
chart <- bbbDescr[ ,names(bbbDescr) %in% row.names(s_dscp)] 
n = length(chart)

for(i in 1:n){  
  
  y = names(chart)[i]
  
  hist(chart[,i] ,    main = paste("Histogram of" , y), c="greay")  

}

# correlations
rho <- round(abs(cor(bbbDescr)),2) > 0.75
h_corrs <- as.data.frame(ifelse(rho == T, 1,0))
colSums(h_corrs) 
# some of the features are highly intercorrelated as expected

# since there are several dimensions to this data set a dimensions reduction techinque OR
# selection based on variable importance would be reccomended.

# how many acre actually correlated with the outcome variable?
round(abs(cor(logBBB, bbbDescr)),2)[ 1,]

# using PCA
pca_ob <- prcomp(bbbDescr, center = T, scale = T)
summary(pca_ob)   # 99% of Variace hit by PC22 , 75% by PC9
screeplot(pca_ob, npcs = 22, type = "lines")

head(pca_ob$rotation)
head(pca_ob$x)


