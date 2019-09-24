## Load Libraries DPLYR and GGPLOT2 for data visualization and data transformations

library(ggplot2)
library(dplyr)
library(mice)
library(corrplot)
library(Hmisc)

## Load Training Data

trainingdata = read.csv(file="C:/Users/hecsa/Documents/data 621 hw 1/moneyball-training-data.csv",header = TRUE,sep=',')
evaldata = read.csv(file="C:/Users/hecsa/Documents/data 621 hw 1/moneyball-evaluation-data.csv",header = TRUE,sep=',')
## Summary Statistics

names(trainingdata)

sum1 = summary(trainingdata[,2:length(names(trainingdata))]) ## exclude index

print(sum1)

## Initial view of data frame

head(trainingdata,5) ## All variables are quantitative

## correlation matrix to see relationship between variables

cor(trainingdata[,2:length(names(trainingdata))]) ## doing this is ineffective; requires transformation to parse NAs

## NA Transformation: Applied to TEAM_BATTING_SO,TEAM_BASERUN_SB,TEAM_BASERUN_CS,TEAM_BATTING_HBP,TEAM_PITCHING_SO,TEAM_FIELDING_DP

## First, we determine the % of NAs relative to the data set.

naperTEAMBATTINGSO = 102/length(trainingdata$INDEX)
naperTEAMBASERUNSB = 131/length(trainingdata$INDEX)
naperTEAMBASERUNCS = 772/length(trainingdata$INDEX)
naperTEAMBATTINGhbp = 2085/length(trainingdata$INDEX)
naperTEAMPITCHINGSO = 102/length(trainingdata$INDEX)
naperTEAMFIELDINGDP = 286/length(trainingdata$INDEX)

## If the % of NAs is less than 10% we will be imputing the average for missing values

percentlist = list(naperTEAMBATTINGSO,naperTEAMBASERUNSB,naperTEAMBASERUNCS,naperTEAMBATTINGhbp,naperTEAMPITCHINGSO,naperTEAMFIELDINGDP)

ifelse(percentlist>.1,"remove","impute")

## to many entries are missing to consider the following columns relevant to the analysis: TEAM_BASERUN_CS,TEAM_BATTING_HBP,TEAM_FIELDING_DP
## we remove these columns to focus only on the relevant datapoints

todrop = c('INDEX','TEAM_BASERUN_CS','TEAM_BATTING_HBP','TEAM_FIELDING_DP')
newtrainingdata = trainingdata[,!names(trainingdata) %in% todrop]

# impute columns less than 10%

tempdata = mice(data = newtrainingdata, m = 13, method = "pmm", maxit = 50, seed = 500)
completedatatraining = complete(tempdata,1)

# correlation matrix of completed data

correlationmatrix = cor(completedatatraining)

corrandpvalues = rcorr(as.matrix(completedatatraining))

print(corrandpvalues)

corrplot(correlationmatrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

## Using the correlation and p value matrix it is shown that target wins is statistically significant for all variables except TEAM_BATTING_SO

## we now build our first multi regression model

fit = lm(TARGET_WINS~.-TEAM_BATTING_SO,completedatatraining)
summary(fit)
par(mfrow=c(2,2))

plot(fit)
predict(fit)

## model 1 has 4 coefficients that are not statistically significant. Lets pull out the two largest p-values and see how that affects the model in fit2

fit2 = lm(TARGET_WINS~.-TEAM_BATTING_SO-TEAM_PITCHING_HR-TEAM_PITCHING_SO,completedatatraining)
summary(fit2)
par(mfrow=c(2,2))

plot(fit2)
predict(fit2)

## Model 2 a still has two none statistically significant variables that we can weed out. Model 3 will be the fit less those variables

fit3 = lm(TARGET_WINS~.-TEAM_BATTING_SO-TEAM_PITCHING_HR-TEAM_PITCHING_SO-TEAM_PITCHING_BB-TEAM_BATTING_BB,completedatatraining)
summary(fit3)
par(mfrow=c(2,2))

plot(fit3)
predict(fit3)

### Model three can still be made more efficient by pulling out TEAM_BATTING_2B and TEAM_PITCHING_H

fit4 = lm(TARGET_WINS~.-TEAM_BATTING_SO-TEAM_PITCHING_HR-TEAM_PITCHING_SO-TEAM_PITCHING_BB-TEAM_BATTING_BB-TEAM_PITCHING_H-TEAM_BATTING_2B,completedatatraining)
summary(fit4)
par(mfrow=c(2,2))

plot(fit4)
predict(fit4)

## prediction using fit4 and evaluation data

summary(evaldata)

evalteambattingso = 18/length(evaldata$INDEX)
evalTEAM_BASERUN_SB = 13/length(evaldata$INDEX)
evalTEAM_BASERUN_CS = 87/length(evaldata$INDEX)
evalTEAM_BATTING_HBP = 240/length(evaldata$INDEX)
evalTEAM_PITCHING_SO = 18/length(evaldata$INDEX)
evalTEAM_FIELDING_DP = 31/length(evaldata$INDEX)


evalnewtrainingdata = evaldata[,!names(evaldata) %in% todrop]

 ### impute columns less than 10%

tempdata2 = mice(data = evalnewtrainingdata, m = 13, method = "pmm", maxit = 50, seed = 500)
evalcompletedatatraining = complete(tempdata2,1)

predict(fit4,newdata=evalcompletedatatraining)
