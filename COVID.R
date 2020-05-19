# Importing Necessary libraries
library('rcompanion')
library('DescTools')
library('readxl')
library('MASS')
library('ROCR')
library('ggplot2')
library('corrplot')


# Importing disease.xlsx file
# Skipping first column as it is just the ID column
disease <- read_excel("C:/Users/Amit R. Amin/Downloads/COVID-19/disease.xlsx", 
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
disease <- read_excel("C:/Users/Amit R. Amin/Downloads/COVID-19/disease.xlsx", 
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

# Predicting on test data based on training set
disease.glm.predict <- predict(disease.glm,disease.test,type = "response")
summary(disease.glm.predict)
# Mean predict results for SARS Cov2 Results diagnosis
tapply(disease.glm.predict, disease.test$SARS_COV2_Result, mean)

# Confusion matrix for threshold of 1%
table(disease.test$SARS_COV2_Result, disease.glm.predict > 0.01)

# False negative error rate (Type II error)
36/(36+22)

# Plotting ROCR curve
disease.ROCRpred = prediction(disease.glm.predict, disease.test$SARS_COV2_Result)
disease.ROCRperf = performance(disease.ROCRpred, "tpr", "fpr")
plot(disease.ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))





# Importing condition.xlsx file
condition <- read_excel("C:/Users/Amit R. Amin/Downloads/COVID-19/condition.xlsx", 
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

# attaching the final dataset for ease of use
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

# Predicting on test data based on training set
condition.glm.predict <- predict(condition.glm,condition.test,type = "response")
summary(condition.glm.predict)
tapply(condition.glm.predict, condition.test$SARS_COV2_Result, mean)

# Confusion matrix for threshold of 1%
table(condition.test$SARS_COV2_Result, condition.glm.predict > 0.01)

# False negative error rate (Type II error)
24/(24+19)


# Plotting ROCR curve
condition.ROCRpred = prediction(condition.glm.predict, condition.test$SARS_COV2_Result)
condition.ROCRperf = performance(condition.ROCRpred, "tpr", "fpr")
plot(condition.ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))


#Seperate investigation

#Platelets            ***
#Monocytes            ***
#Hemoglobin           *
#Red_blood_cells      *
#Mean_platelet_volume *


# Plotting Seperate sigmoids
plotting.function <- function(var){
  condition.sep.function = paste("SARS_COV2_Result", "~", as.character(var))
  condition.sep.glm = glm(as.formula(condition.function), data = condition.train , family = binomial)
  summary(condition.sep.glm)
  predicted.data <- data.frame(
    probability.of.SARS=condition.glm$fitted.values,
    variable=condition.train[,as.character(var)])
  predicted.data <- predicted.data[
    order(predicted.data$probability.of.SARS, decreasing=FALSE),]
  predicted.data$rank <- 1:nrow(predicted.data)
  ggplot(data=predicted.data, aes(x=rank, y=probability.of.SARS)) +
    geom_point(aes(color=variable), size=3) +
    xlab(as.character(var)) +
    ylab("Probability of SARS CoV-2")
}

plotting.function("Platelets")
plotting.function("Monocytes")
plotting.function("Hemoglobin")
plotting.function("Red_blood_cells")
plotting.function("Mean_platelet_volume")