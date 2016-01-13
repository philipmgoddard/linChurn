#################################################
# load packages, data, and register cores for parallel
library(dplyr)
library(FUNctions)
library(caret)
library(AppliedPredictiveModeling)
library(Hmisc)
library(C50)
library(e1071)
library(subselect)
library(corrplot)
library(ggbiplot)
library(doMC)

registerDoMC(4)
data(churn)

#################################################
#################################################
# have a quick look at data
# we see a very strong class imbalance, so we may want
# to take this into account for the metric to tune models to
# View(churnTrain)
describe(churnTrain)
summary(churnTrain)

# accuracy not the best metric- as I can get 85% accuracy by
# just guess no churn. More interested in sensitivity
# ie out of those we say churn, who actually churns?
table(churnTrain$churn)

# pull out predictive factors and the outcome
trainOutcome <- churnTrain$churn
testOutcome <- churnTest$churn
trainPred <- churnTrain[!names(churnTrain) == "churn"]
testPred <- churnTest[!names(churnTest) == "churn"]

#################################################
#################################################
# strategy: explore covariates, highlight any potential issues
# recode categorical as dummies
# calculate interactions between numerical predictors
# extract a full and reduced set of predictors
# build variety of models
# evaluate models with lift curves, calibration plots etc

##################################################
#################################################
# step 1: some visualisations

# firstly see what class the predictors are
vapply(churnTrain, class, character(1))

# seperate out factor, double (continuous) and integer (count) predictors
# train
facCols <- trainPred[, vapply(trainPred, is.factor, logical(1))]
numCols <- trainPred[, vapply(trainPred, is.double, logical(1))]
countCols <- trainPred[, vapply(trainPred, is.integer, logical(1))]

# test 
facColsT <- testPred[, vapply(testPred, is.factor, logical(1))]
numColsT <- testPred[, vapply(testPred, is.double, logical(1))]
countColsT <- testPred[, vapply(testPred, is.integer, logical(1))]

# CONTINUOUS NUMERIC PREDICTORS
# custom phil plots- if you dont have my FUNctions library you 
# cant make these. Also, you may want to build your own
# color theme- philTheme() probably isn't available to you!
plotListDouble <- ggplotListDens(numCols, trainOutcome)
ggMultiplot(plotListDouble, cols = 3)

# investigate correlated covariates- costs and minutes
ggplot(numCols, aes(x = total_day_minutes,
                    y = total_day_charge,
                    color = factor(trainOutcome))) +
  geom_point(alpha = 0.4, size = 4) +
  theme_bw() +
  scale_color_manual(values = philTheme()[c(4, 1)], name = "Churn") +
  theme(legend.position = c(0.1, 0.8),
        legend.text.align = 0) 

# investigate pairwise relation with caret::featurePlot
transparentTheme(trans = 0.1)
featurePlot(x = numCols,
            y = trainOutcome,
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 2))

# COUNT DATA
plotListCount <- ggplotListHist(countCols, trainOutcome)
ggMultiplot(plotListCount, cols = 3)

# pairs
transparentTheme(trans = 0.1)
featurePlot(x = countCols,
            y = trainOutcome,
            plot = "pairs",
            ## Add a key at the top
            auto.key = list(columns = 2))

# do we see any obvious class seperation here? can repeat this
# for continous numeric as well
theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
featurePlot(x = countCols,
            y = ifelse(trainOutcome== "yes", 1, 0),
            plot = "scatter",
            layout = c(4, 2))

# FACTORS
plotListFac <- ggplotListBar(facCols, trainOutcome)
ggMultiplot(plotListFac, cols = 2)

##################################################
##################################################
# Lets act on what we discovered with visualistions.
# Remember everything done for train must be done for test!

# loose the charges, as correlated with minutes
numCols <- numCols[, !names(numCols) %in% c("total_day_charge",
                                            "total_eve_charge",
                                            "total_night_charge",
                                            "total_intl_charge")]
numColsT <- numColsT[, !names(numCols) %in% c("total_day_charge",
                                              "total_eve_charge",
                                              "total_night_charge",
                                              "total_intl_charge")]

# remove number_vmail_message, as linear models may struggle with bimodal
# distribution
countCols <- countCols[, !names(countCols) %in% "number_vmail_messages"]
countColsT <- countColsT[, !names(countColsT) %in% "number_vmail_messages"]

# identifying those that are skewed
vapply(numCols, skewness, numeric(1))
vapply(countCols, skewness, numeric(1))

# need to transofrm total_intl_calls and
# number_customer_service_calls.
# caret has funky preprocess options, but lets stay simple with a sqrt.
countCols$total_intl_calls <- sqrt(countCols$total_intl_calls)
countCols$number_customer_service_calls <- sqrt(countCols$number_customer_service_calls)

countColsT$total_intl_calls <- sqrt(countColsT$total_intl_calls)
countColsT$number_customer_service_calls <- sqrt(countColsT$number_customer_service_calls)

##################################################
##################################################
# process categorical data

# can look at chisq or fisher tests to ivestigate odds
# or obtain p-values for association where appropriate
table(churnTrain$international_plan, churnTrain$churn)
fisher.test(table(churnTrain$international_plan, churnTrain$churn))

# can we diagnose how informative the state is?
# chisq test fails- my guess is too many levels
stateTab <- table(churnTrain$state, churnTrain$churn)
chisq.test(stateTab)

# for now, lets keep all factors. dummy up.
catDummies <- dummyVars(~. ,
                        data = facCols)

facTrans <- data.frame(predict(catDummies, facCols))
facTransT <- data.frame(predict(catDummies, facColsT))

# important to remember that for linear models, some will be negatively impacted
# by colliniear predictors, eg those where a binary yes/no we only need one to deduce
# the other. To be on the safe side, lets remove one for each category.
# the unfortunate side effect is that models become less interpretable. 

# use subselect::trim.matrix to find collinear
reducedCovMat <- cov(facTrans[, ])
trimmingResults <- trim.matrix(reducedCovMat)
trimmingResults$names.discarded

# remove offending columns
facTrans <- facTrans[, !names(facTrans) %in% trimmingResults$names.discarded]
facTransT <- facTransT[, !names(facTransT) %in% trimmingResults$names.discarded]

# rename
facTrans <- facTrans %>%
  dplyr::rename(voice_mail_plan = voice_mail_plan.yes,
                international_plan = international_plan.yes,
                area_code_510 = area_code.area_code_510,
                area_code_408 = area_code.area_code_408)

facTransT <- facTransT %>%
  dplyr::rename(voice_mail_plan = voice_mail_plan.yes,
                international_plan = international_plan.yes,
                area_code_510 = area_code.area_code_510,
                area_code_408 = area_code.area_code_408)

# check for zero variance and near zero variance
# we see that states have near zero variance...
# could imply not highly informative. For sensitive models
# we definitley don't want to include. Not such an issue for those 
# which can peform feature selection.
# we will come back to this when building a full and reduced set 
# of predictors
nzvFac <- nearZeroVar(facTrans, saveMetric = TRUE)

#################################################
#################################################
# next step - lets combine all of our numerical predictors
numInput <- cbind(numCols, countCols)
numInputT <- cbind(numColsT, countColsT)

# calculate interactions
trainInput <- quadInteraction(numInput)
testInput <-  quadInteraction(numInputT)

trainInput <- cbind(trainInput, facTrans)
testInput <- cbind(testInput, facTransT)

#################################################
#################################################
# start filtering. make a full set and a reduced set- full set for
# models that can do feature selection, reduce set for those that cannot

# remove near zero variance for reduced set
isNZV <- nearZeroVar(trainInput, saveMetrics = TRUE)
fullSet <- names(trainInput[, !isNZV$zeroVar])
reducedSet <- names(trainInput[, !isNZV$nzv])

# investigate correlation- set a threshold of 0.9 for 
# reduced set, 0.99 for full set
trainCorr <- cor(trainInput)
highCorr <- findCorrelation(trainCorr, cutoff = 0.9)
fullCorr <- findCorrelation(trainCorr, cutoff = 0.99)
highCorrNames <- names(trainInput)[highCorr]
fullCorrNames <- names(trainInput)[fullCorr]

fullSet <- fullSet[!fullSet %in% fullCorrNames]
reducedSet <- reducedSet[!reducedSet %in% highCorrNames]

# do a pretty correlation plot
corrplot(cor(trainInput[, reducedSet]), order = "hclust", tl.cex = .35)

# do a pca plot
trainPCA <- prcomp(trainInput[, reducedSet], scale = TRUE)
type <- trainOutcome
reduced_pca <- ggbiplot(trainPCA, obs.scale = 1, 
                        var.scale = 1,
                        groups = type,
                        ellipse = TRUE,
                        circle = TRUE,
                        var.axes = FALSE,
                        varname.size = 3,
                        alpha = 0.3)
reduced_pca +
  theme_bw() +
  scale_color_manual(values = philTheme()[c(6, 1)], name = "Churn") 

#################################################
#################################################
# fit models
# always set seeds before training for reproduceability

# set up train control
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     classProbs = TRUE,
                     savePredictions = TRUE,
                     summaryFunction = twoClassSummary)

#################################################
# glm
set.seed(476)
glmTune <- train(x = trainInput[, reducedSet],
                 y = trainOutcome,
                 method = "glm",
                 preProc = c("center", "scale"),
                 metric = "Sens",
                 trControl = ctrl)
glmTune

# can save and load models like this:
#save(glmTune, file = "glmChurn.Rdata")
#load("glmChurn.Rdata")

# predictions for test set: can do for class and probabilities
glmPred <- predict(glmTune, newdata = testInput[, reducedSet])
glmPredProb <- predict(glmTune, newdata = testInput[, reducedSet], type = "prob")

# sensitivity of 0.33 on test set
confusionMatrix(data = glmPred, reference = testOutcome)

#################################################
# lda
set.seed(476)
ldaTune <- train(x = trainInput[, reducedSet],
                 y = trainOutcome,
                 method = "lda",
                 preProc = c("center", "scale"),
                 metric = "Sens",
                 trControl = ctrl)
ldaTune
#save(ldaTune, file = "ldaChurn.Rdata")
#load("ldaChurn.Rdata")

ldaPred <- predict(ldaTune, newdata = testInput[, reducedSet])
ldaPredProb <- predict(ldaTune, newdata = testInput[, reducedSet], type = "prob")

# sensitivity of 0.375 on test set
confusionMatrix(data = ldaPred, reference = testOutcome)

#################################################
# plsda
# even though could take full set, experience shows it peforms better with reduced
set.seed(476)
plsdaTune <- train(x = trainInput[, reducedSet],
                   y = trainOutcome,
                   method = "pls",
                   tuneGrid = expand.grid(.ncomp = 5:20),
                   preProc = c("center", "scale"),
                   probMethod = "Bayes",
                   metric = "Sens",
                   trControl = ctrl)

plsdaTune
#save(plsdaTune, file = "plsdaChurn.Rdata")
#load("plsdaChurn.Rdata")

plsdaPred <- predict(plsdaTune, newdata = testInput[, reducedSet])
plsdaPredProb <- predict(plsdaTune, newdata = testInput[, reducedSet], type = "prob")

# sensitivity of 0.388 on test set
confusionMatrix(data = plsdaPred, reference = testOutcome)

#################################################
# glmnet
# firstly set up a grid of training parameters
glmnetGrid <- expand.grid(.alpha = c(0.05, 0.1, 0.15, 0.2, 0.4, 0.6, 0.8, 1),
                          .lambda = seq(0.0, 0.2, length = 11))

set.seed(476)
glmnetTune <- train(x = trainInput[, fullSet],
                    y = trainOutcome,
                    method = "glmnet",
                    tuneGrid = glmnetGrid,
                    preProc = c("center", "scale"),
                    metric = "Sens",
                    family = "binomial",
                    trControl = ctrl)
glmnetTune
#save(glmnetTune, file = "glmnetChurn.Rdata")
#load("glmnetChurn.Rdata")

# as an aside, get coefficients with this syntax
# coef(glmnetTune$finalModel, glmnetTune$bestTune$lambda)

glmnetPred <- predict(glmnetTune, newdata = testInput[, fullSet])
glmnetPredProb <- predict(glmnetTune, newdata = testInput[, fullSet], type = "prob")

# sensitivity of 0.446 on test set
confusionMatrix(data = glmnetPred, reference = testOutcome)

#################################################
# penalised lda
penldaGrid <- expand.grid(.NumVars = seq(1, 101, 4),
                          .lambda = c(0.03, 0.1))
set.seed(476)
penldaTune <- train(x = trainInput[, fullSet],
                          y = trainOutcome,
                          method = "sparseLDA",
                          tuneGrid = penldaGrid,
                          preProc = c("center", "scale"),
                          metric = "Sens",
                          trControl = ctrl)
penldaTune
#save(penldaTune, file = "penldaChurn.Rdata")
#load("penldaChurn.Rdata")

penldaPred <- predict(penldaTune, newdata = testInput[, fullSet])
penldaPredProb <- predict(penldaTune, newdata = testInput[, fullSet], type = "prob")

# sensitivity of 0.455 on test set
confusionMatrix(data = penldaPred, reference = testOutcome)

#################################################
#################################################
# gather results for comparison

# look at training resamples for model metrics
models <- list(glm = glmTune,
               lda = ldaTune,
               plsda = plsdaTune,
               glmnet = glmnetTune,
               penlda = penldaTune)
bwplot(resamples(models))

# pull out results so can look at lift curves and calibration
# need predicted probabilities for this
results <- data.frame(glm = glmPredProb$yes,
                      lda = ldaPredProb$yes,
                      plsda = plsdaPredProb$yes,
                      glmnet = glmnetPredProb$yes,
                      penlda = penldaPredProb$yes,
                      class = testOutcome)

# calibration curves
# to make max's pretty plots use bookTheme() from APM package
trellis.par.set(bookTheme())
calCurve <- calibration(class ~ glm + lda + plsda + glmnet + penlda,
                        data = results)
calCurve
xyplot(calCurve,
       auto.key = list(columns = 3))

# lift curves
liftCurve <- lift(class ~ glm +  penlda, data = results)
liftCurve
xyplot(liftCurve,
       auto.key = list(columns = 2,
                       lines = TRUE,
                       points = FALSE))


# in test set, 1667 total
# 224 churn, 1443 non churn
# 26% of total samples gives 80% of churners
# .26 * 1667 = 433
# of this 433, 0.8 * 224 churn (179)
# of this 433, the remaining 254 do not churn
# so we have reached 245/1443 = 17.6% of non churners to get 80% of churners

