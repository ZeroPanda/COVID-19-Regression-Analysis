# Importing Necessary libraries
library('rcompanion')
library('DescTools')
library('readxl')
library('MASS')
library('ROCR')
library('ggplot2')
library('corrplot')
library('latticeExtra')
library('boot')
library('gridExtra')

# Importing disease.xlsx file
# Skipping first column as it is just the ID column
disease <- read_excel("C:/Users/Shrey/Downloads/COVID-19/disease.xlsx", 
                      col_types = c("skip", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric"))

# Summarizing disease dataset
summary(disease)

# Skipping parainfluenza_2 here
disease <- read_excel("C:/Users/Shrey/Downloads/COVID-19/disease.xlsx", 
                      col_types = c("skip", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "skip", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", 
                                    "numeric"))

# Renaming column names
names(disease)[1] <- "Patient_Age_Quantile"
names(disease)[2] <- "SARS_COV2_Result"
names(disease)[3] <- "Respiratory_Syncytial_Virus"
names(disease)[4] <- "Influenza_B"
names(disease)[5] <- "Influenza_A"
names(disease)[6] <- "Coronavirus_HKU1"
names(disease)[7] <- "Parainfluenza_1"
names(disease)[9] <- "Bordetella_pertussis"
names(disease)[10] <- "Inf_A_H1N1_2009"
names(disease)[13] <- "Parainfluenza_4"
names(disease)[15] <- "Chlamydophila_pneumoniae"
names(disease)[16] <- "Parainfluenza_3"
names(disease)[17] <- "Rhinovirus_OR_Enterovirus"

# attaching the final dataset for ease of use
attach(disease)

# CramerV Correlation to check for any correlation between catagorical variables
# Except Patient's age, all other variables are catagorical(binary)
disease.corr = PairApply(disease[,names(disease) != "Patient_Age_Quantile"],cramerV, symmetric = TRUE)
# Displaying correlation with variable SARS_COV2_Result
disease.corr[,2]
# Correlation plot
par(mfrow=c(1,1))
corrplot(disease.corr, method = "square", type = "lower")


# Dividing Train/Test data with 80% training and 20% test
sample_size <- floor(0.8 * nrow(disease))
train <- sample(nrow(disease), size = sample_size)
disease.train <- as.data.frame(disease[train,])
disease.test <- as.data.frame(disease[-train,])


# Logistic regression considering all the variables on the targer variable SARS_COV2_Result
disease.function = paste("SARS_COV2_Result", "~", ".")
disease.glm = glm(as.formula(disease.function), data = disease.train , family = binomial)
# Summarizing the analysis
summary(disease.glm)


# After reducing dimensions
disease.function = paste("SARS_COV2_Result", "~", "Patient_Age_Quantile + Rhinovirus_OR_Enterovirus")
disease.glm = glm(as.formula(disease.function), data = disease.train , family = binomial)
summary(disease.glm)

# 10 fold cross-validation to verify the model
disease.cv.glm = cv.glm(disease.train,disease.glm,K=10)
plot(disease.cv.glm)

# Predicting on test data based on training set
disease.glm.predict <- predict(disease.glm,disease.test,type = "response")
summary(disease.glm.predict)
# Mean predict results for SARS Cov2 Results diagnosis
tapply(disease.glm.predict, disease.test$SARS_COV2_Result, mean)

# Confusion matrix for threshold of 1%
disease.confusion = table(disease.test$SARS_COV2_Result, disease.glm.predict > 0.01)
disease.confusion

# False negative error rate (Type II error)
disease.type2error = disease.confusion[1,1]/(disease.confusion[1,1]+disease.confusion[2,2])
disease.type2error

# Plotting ROCR curve
disease.ROCRpred = prediction(disease.glm.predict, disease.test$SARS_COV2_Result)
disease.ROCRperf = performance(disease.ROCRpred, "tpr", "fpr")
par(mfrow=c(1,2))
plot(disease.ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

# Probability of SARS vs Rhinovirus plot
disease.predicted.data <- data.frame(
  probability.of.having.SARS=disease.glm.predict,
  Rhinovirus=disease.test$Rhinovirus_OR_Enterovirus)
disease.predicted.data <- disease.predicted.data[
  order(disease.predicted.data$probability.of.having.SARS, decreasing=FALSE),]
disease.predicted.data$rank <- 1:nrow(disease.predicted.data)

plot(probability.of.having.SARS ~ Rhinovirus,disease.predicted.data, type = "l", lwd = 2)



# Importing condition.xlsx file
condition <- read_excel("C:/Users/Shrey/Downloads/COVID-19/condition.xlsx", 
                        col_types = c("skip", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", 
                                      "numeric"))

# Renaming column names
names(condition)[1] <- "Patient_Age_Quantile"
names(condition)[2] <- "SARS_COV2_Result"
names(condition)[5] <- "Mean_corpuscular_hemoglobin_MCH"
names(condition)[6] <- "Red_blood_cells"
names(condition)[11] <- "Mean_corpuscular_volume_MCV"
names(condition)[12] <- "Red_blood_cell_distribution_width_RDW"
names(condition)[15] <- "Mean_corpuscular_hemoglobin_concentration_MCHC"
names(condition)[16] <- "Mean_platelet_volume"
names(condition)[18] <- "Proteina_C_reativa_mg_dL"

# Summarizing disease dataset
summary(condition)

# attaching the final dataset
attach(condition)

# Correlation between varables
condition.corr = cor(condition)
condition.corr[,2]
# Correlation plot
par(mfrow=c(1,1))
corrplot(condition.corr, method = "square", type = "lower")


# Dividing Train/Test data with 80% training dataset
sample_size <- floor(0.8 * nrow(condition))
train_ind <- sample(nrow(condition), size = sample_size)
condition.train <- as.data.frame(condition[train_ind,])
condition.test <- as.data.frame(condition[-train_ind,])


# Logistic regression considering all the variables on the targer variable SARS_COV2_Result
condition.function = paste("SARS_COV2_Result", "~", ".")
condition.glm = glm(as.formula(condition.function), data = condition.train , family = binomial)
summary(condition.glm)


# after reducing dimensions
condition.function = paste("SARS_COV2_Result", "~", "Patient_Age_Quantile + Leukocytes + Eosinophils + Red_blood_cell_distribution_width_RDW + Platelets + Proteina_C_reativa_mg_dL")
condition.glm = glm(as.formula(condition.function), data = condition.train , family = binomial)
summary(condition.glm)

# 10 fold cross-validation to verify the model
cv.glm(condition.train,condition.glm,K=10)$delta[1]

# Predicting on test data based on training set
condition.glm.predict <- predict(condition.glm,condition.test,type = "response")
summary(condition.glm.predict)
tapply(condition.glm.predict, condition.test$SARS_COV2_Result, mean)

# Confusion matrix for threshold of 1%
condition.confusion = table(condition.test$SARS_COV2_Result, condition.glm.predict > 0.01)
condition.confusion

# False negative error rate (Type II error)
condition.type2error = condition.confusion[1,1]/(condition.confusion[1,1]+condition.confusion[2,2])
condition.type2error

# Plotting ROCR curve
condition.ROCRpred = prediction(condition.glm.predict, condition.test$SARS_COV2_Result)
condition.ROCRperf = performance(condition.ROCRpred, "tpr", "fpr")
plot(condition.ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

# Creating a dataframe with variables and predicted values of SARS results

condition.predict.dataframe <- data.frame(
  probability.of.having.SARS=exp(condition.glm$fitted.values)/(1 + exp(condition.glm$fitted.values)),
  Leukocytes=condition.train$Leukocytes,
  Patient_Age_Quantile = condition.train$Patient_Age_Quantile,
  Eosinophils = condition.train$Eosinophils,
  Red_blood_cell_distribution_width_RDW = condition.train$Red_blood_cell_distribution_width_RDW,
  Platelets = condition.train$Platelets,
  Proteina_C_reativa_mg_dL = condition.train$Proteina_C_reativa_mg_dL)


plot1 = ggplot(data=condition.predict.dataframe, aes(x=Patient_Age_Quantile, y=probability.of.having.SARS)) +
  geom_point(aes(color=Patient_Age_Quantile), size=3)+
  geom_smooth(se = T , lwd=1.5 , col= "red", method = 'loess')+
  ylim(NA,1)+ scale_color_gradient(guide = 'none')
plot2 = ggplot(data=condition.predict.dataframe, aes(x=Leukocytes, y=probability.of.having.SARS)) +
  geom_point(aes(color=Leukocytes), size=3)+
  geom_smooth(se = T , lwd=1.5 , col= "red", method = 'loess')+
  ylim(NA,1)+ scale_color_gradient(guide = 'none')
plot3 = ggplot(data=condition.predict.dataframe, aes(x=Red_blood_cell_distribution_width_RDW, y=probability.of.having.SARS)) +
  geom_point(aes(color=Red_blood_cell_distribution_width_RDW), size=3)+
  geom_smooth(se = T , lwd=1.5 , col= "red", method = 'loess')+
  ylim(NA,1)+ scale_color_gradient(guide = 'none')
plot4 = ggplot(data=condition.predict.dataframe, aes(x=Eosinophils, y=probability.of.having.SARS)) +
  geom_point(aes(color=Eosinophils), size=3)+
  geom_smooth(se = T , lwd=1.5 , col= "red", method = 'loess')+
  ylim(NA,1)+ scale_color_gradient(guide = 'none')
plot5 = ggplot(data=condition.predict.dataframe, aes(x=Platelets, y=probability.of.having.SARS)) +
  geom_point(aes(color=Platelets), size=3)+
  geom_smooth(se = T , lwd=1.5 , col= "red", method = 'loess')+
  ylim(NA,1)+ scale_color_gradient(guide = 'none')
plot6 = ggplot(data=condition.predict.dataframe, aes(x=Proteina_C_reativa_mg_dL, y=probability.of.having.SARS)) +
  geom_point(aes(color=Proteina_C_reativa_mg_dL), size=3)+
  geom_smooth(se = T , lwd=1.5 , col= "red", method = 'loess')+
  ylim(NA,1)+ scale_color_gradient(guide = 'none')
# Plotting the values
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol=3 , nrow = 2)

#Seperate investigation

#Platelets            ***
#Monocytes            ***
#Hemoglobin           *
#Red_blood_cells      *
#Mean_platelet_volume *


# Plotting Seperate probability graphs glm
par(mfrow=c(1,1))
plotting.function <- function(var){
  condition.sep.function = paste("SARS_COV2_Result", "~", as.character(var))
  condition.sep.glm = glm(as.formula(condition.sep.function), data = condition.train , family = binomial)
  print(summary(condition.sep.glm))
  cv.glm(condition.train,condition.sep.glm,K=10)$delta[1]
  
  condition.predicted.data <- data.frame(
    probability.of.SARS=condition.sep.glm$fitted.values,
    variable=condition.train[,as.character(var)])
  
  condition.predicted.data <- condition.predicted.data[
    order(condition.predicted.data$variable, decreasing=FALSE),]
  
  condition.predicted.data$rank <- 1:nrow(condition.predicted.data)
  
  ggplot(data=condition.predicted.data, aes(x= variable, y=probability.of.SARS)) +
    geom_point(aes(color=variable), size=3) +
    xlab(as.character(var)) +
    ylab("Probability of having SARS CoV-2") +
    scale_colour_gradient(low = "darkgreen", high = "darkred", na.value = NA, guide = F) +
    # ggtitle(coef(summary(condition.sep.glm))[,'Pr(>|z|)']) 
    # ggtitle('Linear fit (Train data)',paste('beta1 Resid. dev.',anova(condition.sep.glm)['Resid. Dev'][2,1]))+
    ggtitle('Linear fit (Train data)')+
    ylim(0,1)
    
}

plotfun1 = plotting.function("Platelets")
plotfun2 = plotting.function("Monocytes")
plotfun3 = plotting.function("Hemoglobin")
plotfun4 = plotting.function("Red_blood_cells")
plotfun5 = plotting.function("Mean_platelet_volume")


# GAM using natural spline instead of glm
plotting.function2= function(var){ 
  require(gam)
  variable = as.character(var)
  condition.sep.function = paste("SARS_COV2_Result ~ ns(", as.character(var),")")
  condition.sep.gam = gam(as.formula(condition.sep.function), data = condition.train , family = binomial)
  print(summary(condition.sep.gam))
  
  preds = predict(condition.sep.gam, newdata = condition.test, se = T)
  se.bands = preds$fit + cbind(fit = 0, lower = -2 * preds$se, upper = 2 * preds$se)
  prob.bands = exp(se.bands)/(1 + exp(se.bands))
  
  
  condition.predicted.data <- data.frame(
    condition.test[,as.character(var)], prob.bands)
  
  condition.predicted.data <- condition.predicted.data[
    order(condition.predicted.data[,1], decreasing=FALSE),]
  
  attach(condition.predicted.data)
    ggplot() + 
    geom_line(data = condition.predicted.data, aes(x = condition.predicted.data[,1], y = condition.predicted.data[,2]), lwd = 1.2, color = "navyblue") +
    geom_line(data = condition.predicted.data, aes(x = condition.predicted.data[,1], y = condition.predicted.data[,3]),lwd = 1.2, color = "red4", linetype ="dashed") +
    geom_line(data = condition.predicted.data, aes(x = condition.predicted.data[,1], y = condition.predicted.data[,4]),lwd = 1.2, color = "red4", linetype ="dashed") +
    geom_point(data = condition.predicted.data, aes(x = condition.predicted.data[,1], y = condition.test$SARS_COV2_Result), color = "purple4") +
    xlab(as.character(var)) +
    ylab('Probability of having SARS CoV-2')+
    ggtitle('Spline fit')
    # ggtitle('Spline fit',paste('beta1 Resid. Dev.',anova(condition.sep.gam)['Resid. Dev'][2,1]))
}

plotfun2.1 = plotting.function2("Platelets")
plotfun2.2 = plotting.function2("Monocytes")
plotfun2.3 = plotting.function2("Hemoglobin")
plotfun2.4 = plotting.function2("Red_blood_cells")
plotfun2.5 = plotting.function2("Mean_platelet_volume")


# Poly with degree glm
plotting.function3= function(var,deg){ 
  require(ISLR)
  variable = as.character(var)
  condition.sep.function = paste("SARS_COV2_Result ~ poly(", as.character(var),",degree=",as.character(deg),")")
  condition.sep.gam = glm(as.formula(condition.sep.function), data = condition.train , family = binomial)
  print(summary(condition.sep.gam))
  
  preds = predict(condition.sep.gam, newdata = condition.test, se = T)
  se.bands = preds$fit + cbind(fit = 0, lower = -2 * preds$se, upper = 2 * preds$se)
  prob.bands = exp(se.bands)/(1 + exp(se.bands))
  
  
  condition.predicted.data <- data.frame(
    condition.test[,as.character(var)], prob.bands)
  
  condition.predicted.data <- condition.predicted.data[
    order(condition.predicted.data[,1], decreasing=FALSE),]
  
  attach(condition.predicted.data)
  ggplot() + 
    geom_line(data = condition.predicted.data, aes(x = condition.predicted.data[,1], y = condition.predicted.data[,2]), lwd = 1.2, color = "orange") +
    geom_line(data = condition.predicted.data, aes(x = condition.predicted.data[,1], y = condition.predicted.data[,3]),lwd = 1.2, color = "darkgreen", linetype ="dashed") +
    geom_line(data = condition.predicted.data, aes(x = condition.predicted.data[,1], y = condition.predicted.data[,4]),lwd = 1.2, color = "darkgreen", linetype ="dashed") +
    geom_point(data = condition.predicted.data, aes(x = condition.predicted.data[,1], y = condition.test$SARS_COV2_Result), color = "darkblue") +
    xlab(as.character(var)) +
    ylab('Probability of having SARS CoV-2')+
    #ggtitle('Poly fit with degree 4',paste('beta1 Resid. Dev.',anova(condition.sep.gam)['Resid. Dev'][2,1]))
    ggtitle('Poly fit with degree 4')
}

plotfun3.1 = plotting.function3("Platelets",4)
plotfun3.2 = plotting.function3("Monocytes",4)
plotfun3.3 = plotting.function3("Hemoglobin",4)
plotfun3.4 = plotting.function3("Red_blood_cells",4)
plotfun3.5 = plotting.function3("Mean_platelet_volume",4)

grid.arrange(plotfun1, plotfun2, plotfun3, plotfun4, plotfun5,plotfun2.1, plotfun2.2, plotfun2.3, plotfun2.4, plotfun2.5,plotfun3.1, plotfun3.2, plotfun3.3, plotfun3.4, plotfun3.5, ncol=5 , nrow = 3)



#Shrinkage with LASSO

library(glmnet)
x=model.matrix(SARS_COV2_Result~.-1,data=condition.train) 
y=condition.train$SARS_COV2_Result
par(mfrow=c(1,3))
fit.lasso=glmnet(x,y)
plot(fit.lasso,xvar="lambda",label=TRUE)
cv.lasso=cv.glmnet(x,y)
plot(cv.lasso)
coef(cv.lasso)

dim(disease.train)
set.seed(1)
train=sample(seq(nrow(condition.train)),nrow(condition.train) * 0.8,replace=FALSE)

lasso.tr=glmnet(x[train,],y[train])
lasso.tr
pred=predict(lasso.tr,x[-train,])
dim(pred)
rmse= sqrt(apply((y[-train]-pred)^2,2,mean))
plot(log(lasso.tr$lambda),rmse,type="b",xlab="Log(lambda)")
lam.best=lasso.tr$lambda[order(rmse)[1]]
lam.best
coef(lasso.tr,s=lam.best)
