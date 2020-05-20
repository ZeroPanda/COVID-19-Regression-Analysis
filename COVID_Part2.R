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
cv.glm(disease.train,disease.glm,K=10)$delta[1]

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
  probability.of.having.SARS=condition.glm$fitted.values,
  Leukocytes=condition.train$Leukocytes,
  Patient_Age_Quantile = condition.train$Patient_Age_Quantile,
  Eosinophils = condition.train$Eosinophils,
  Red_blood_cell_distribution_width_RDW = condition.train$Red_blood_cell_distribution_width_RDW,
  Platelets = condition.train$Platelets,
  Proteina_C_reativa_mg_dL = condition.train$Proteina_C_reativa_mg_dL)

plot1 = ggplot(data=condition.predict.dataframe, aes(x=Patient_Age_Quantile, y=probability.of.having.SARS)) +
  geom_point(aes(color=Patient_Age_Quantile), size=4)
plot2 = ggplot(data=condition.predict.dataframe, aes(x=Leukocytes, y=probability.of.having.SARS)) +
  geom_point(aes(color=Leukocytes), size=4)
plot3 = ggplot(data=condition.predict.dataframe, aes(x=Red_blood_cell_distribution_width_RDW, y=probability.of.having.SARS)) +
  geom_point(aes(color=Red_blood_cell_distribution_width_RDW), size=4)
plot4 = ggplot(data=condition.predict.dataframe, aes(x=Eosinophils, y=probability.of.having.SARS)) +
  geom_point(aes(color=Eosinophils), size=4)
plot5 = ggplot(data=condition.predict.dataframe, aes(x=Platelets, y=probability.of.having.SARS)) +
  geom_point(aes(color=Platelets), size=4)
plot6 = ggplot(data=condition.predict.dataframe, aes(x=Proteina_C_reativa_mg_dL, y=probability.of.having.SARS)) +
  geom_point(aes(color=Proteina_C_reativa_mg_dL), size=4)
# Plotting the values
grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, ncol=3 , nrow = 2)


#Seperate investigation

#Platelets            ***
#Monocytes            ***
#Hemoglobin           *
#Red_blood_cells      *
#Mean_platelet_volume *


# Plotting Seperate probability graphs
plotting.function <- function(var,variableORrank){
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
    scale_colour_gradient(low = "darkgreen", high = "darkred", na.value = NA) +
    ggtitle(coef(summary(condition.sep.glm))[,'Pr(>|z|)'])
}

plotfun1 = plotting.function("Platelets")
plotfun2 = plotting.function("Monocytes")
plotfun3 = plotting.function("Hemoglobin")
plotfun4 = plotting.function("Red_blood_cells")
plotfun5 = plotting.function("Mean_platelet_volume")
grid.arrange(plotfun1, plotfun2, plotfun3, plotfun4, plotfun5, ncol=3 , nrow = 2)
