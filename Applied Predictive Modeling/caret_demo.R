#http://www.edii.uclm.es/~useR-2013/Tutorials/kuhn/user_caret_2up.pdf

library(caret)

data(segmentationData)

seg = segmentationData 
rm(segmentationData )

seg$Cell <- NULL

tr = subset(seg, Case == "Train")
tst = subset(seg, Case == "Test")
tr$Case <- NULL
tst$Case <- NULL

str(tr)
table(tr$Class)

tr_x <- tr[, names(tr) != "Class"]
## Methods are "BoxCox", "YeoJohnson", center", "scale",
## "range", "knnImpute", "bagImpute", "pca", "ica" and
## "spatialSign"
preProc <- preProcess(tr_x, method = c("center","scale"))
preProc

scl_tr <- predict(preProc, tr_x)
head(tr_x)

# To get honest estimates of performance, all data transformations should
# be included within the cross-validation loop.

caret:::createFolds
caret:::createMultiFolds
caret:::createDataPartition

caret:::createResample
?createResample

# example d tree
library(rpart)
prt1 <- rpart( Class~., data=tr, control= rpart.control(maxdepth = 2))
prt1
# convert to party class to plot
library(party)
library(partykit)        

prt1a <- as.party(prt1)
plot(prt1a)
# comparing
plot(prt1) ; text(prt1)


# Specifically, the "one SE" rule is used: estimate the standard error of
# performance for each tree size then choose the simplest tree within one
# standard error of the absolute best tree size
rpartFull <- rpart( Class ~. , data=tr)
rpartFull

r_pred <- predict(rpartFull, tst, type = "class") 
confusionMatrix(r_pred, tst$Class)

#-------------------------------------------------------
# now with caret and tuning parameters

train( Class~., data=tr, method = "rpart", tuneLength = 30)

ctrl <- trainControl(method = "repeatedcv", repeats = 3)

train( Class~., data=tr, 
       method = "rpart", 
       tuneLength = 30, 
       trControl = ctrl)

# set classProbs and twoClassSummary for ROC curve

ctrl <- trainControl(method = "repeatedcv"
                     , repeats = 3
                     ,summaryFunction = twoClassSummary
                     ,classProbs = T
                     )
set.seed(1)
rpt_tn <- train( Class~., data=tr, method = "rpart",
                 tuneLength = 30,
                 metric = "ROC",
                 trControl = ctrl)
rpt_tn

rpt_tn$finalModel

plot(rpt_tn, scales = list( x = list(log = 10)))

r_pred2 <- predict(rpt_tn, tst)

confusionMatrix(r_pred2, tst$Class)

# getting predicting class probabilities
rpt_probs <- predict(rpt_tn, tst, type = "prob")
head(rpt_probs)

# generating ROC curve
install.packages("pROC")
library(pROC)
rpt_ROC <- roc(tst$Class, rpt_probs[,"PS"],
         levels = rev(tst$Class) )
         
plot(rpt_ROC, type = "S", print.thres = 0.5)


# fit 100 models for each iteration of resampling
grid <- expand.grid(.model = "tree",
                    .trials = c(1:100),
                    .winnow = FALSE)

c5_tn <- train( tr_x , tr$Class, method = "C5.0",
                metric = "ROC", tuneGrid = grid,
                trControl = ctrl)

c5_tn 
c5_tn$finalModel

plot(c5_tn) #prorfile

c5pred <- predict(c5_tn, tst)
confusionMatrix( c5pred, tst$Class)

c5probs <- predict( c5_tn, tst, type="prob")
head(c5probs,5)
library(pROC)
c5ROC <- roc( predictor = c5probs$PS,
              response = tst$Class,
              levels = rev(levels(tst$Class)))
c5ROC

# compare - on same plot
plot( rpt_ROC, type="S")
plot( c5ROC, add = T, col = "#9E0142")

# probablity of poor segmentation
histogram( ~c5probs$PS | tst$Class, xlab = "Probability of Poor Segmentation")

#-------------------------------------------------------------------------
# Support Vector Machines

# tuning is important in SVM because it can overfit the data!
set.seed(1)
svmtn <- train( x = tr_x, y = tr$Class, 
                method = "svmRadial", tuneLength = 9,
                preProc = c("center","scale"),
                metric = "ROC",
                trControl = ctrl)
svmtn
svmtn$finalModel
plot(svmtn, metric = "ROC", scales = list(x = list(log = 2)))

s_pred <- predict(svmtn, tst[ ,names(tst) != "Class"])
confusionMatrix(s_pred, tst$Class)

s_prob <- predict(svmtn, tst[ ,names(tst) != "Class"], type="prob")
head(s_prob)
s_ROC <- roc( predictor = s_prob$PS, response = tst$Class, levels =rev(levels(tst$Class)))


plot( rpt_ROC, type="S")
plot( c5ROC, add = T, col = "#9E0142")
plot( s_ROC, add = T, col = "#00FFFF")

#-----------------------------------------------------------------------

# Comparing model performance with resampling

# collates results of all 3 models
cv <- resamples(list( CART = rpt_tn, SVM = svmtn, C5.0 = c5_tn))
summary(cv)

# visualizing resampling

splom( cv, metric = "ROC")
xyplot(cv, metric = "ROC")
parallelplot(cv, metric = "ROC")
dotplot(cv, metric = "ROC")

# comparing model differences
roc_dif <- diff(cv, metric = "ROC")
summary(roc_dif)
dotplot(roc_dif, metric ="ROC")
















