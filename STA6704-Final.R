
  
  library(ggplot2)
  library(GGally)
  library(stats)
  library(pROC)
  #library(PROC)
  library(openxlsx)
  library(reshape2)
  library(plyr)
  library(scales)
  library(usethis)
  #library(devtools)
  library(grid)
  library(tidyverse)
  library(base)
  library(rstream)
  library(simEd)
  library(readxl)
  library(lattice)
  library(survival)
  library(Formula)
  library(RColorBrewer)
  library(ggcorrplot)
  library(htmltools)
  library(MASS)
  library(klaR)
  #reshape the loading data
  library(Hmisc)
  library(caret)
  library(neuralnet)
  library(resample)
  library(readxl)
    library(pls)
  # if (!require("BiocManager", quietly = TRUE))
  #   install.packages("BiocManager")
  # 
  # BiocManager::install("mixOmics")
  library(MASS)
  library(lattice)
  library(mixOmics)
#########################################################
# Reading and preparation of the dataset
#########################################################
setwd("D:\\Classes\\Spring2023\\STA6704\\HWs\\Final Project")
getwd()
# import the old dataset
final_data<-read_excel("D:\\MyResearch\\Tire\\DATA_MATRIX_2_no_laser_line.xlsx")
dim(final_data)
head(final_data)

#Remove Extraneous Variables
Tires<- na.omit(final_data[, -c(2,3)])
dim(Tires) #340 12281

#Check the NAs after removing them from dataset
sum(is.na.data.frame(Tires))

#Convert Dependent to Factor
Tires$`FDLE code` <- as.factor(Tires$`FDLE code`)
unique(Tires$`FDLE code`) #34 leveles



#Convert X values to numeric
n = dim(Tires)[2]
for(i in 2:n){
  Tires[,i] <- lapply(Tires[,i], as.numeric)
}


write.csv(Tires,"D:\\MyResearch\\Tire\\Tiers_OldSet.csv")
Tiers2<-read.csv("D:\\MyResearch\\Tire\\Tiers_OldSet.csv")
dim(Tiers2)
Tiers<-Tiers2[,-1]
# #Omit NA from Numeric Conversion
# Tires_RS <-data.frame(na.omit(Tires_RS) ) 
# str(Tires_RS)
# dim(Tires_RS)#340 25

#X Values
Tires_X_mat<- data.frame(Tiers[,-1])
str(Tires_X_mat)
dim(Tires_X_mat)
#Y values
Tire_Codes<-Tiers[,1]
Tire_Codes
#summary(Tires_X_mat)# All X values are greater than 0


#+1 and Log Transformation
#Tires_X_scaled_RS+1
Tires_X_mat_trans<- log(Tires_X_mat + 1)
dim(Tires_X_mat_trans)
head(Tires_X_mat_trans)
heat_X_mat=melt(Tires_X_mat_trans)
heat_X_mat
heatma_dat
# Scaled the X values
Tires_X_mat_scaled<-scale(Tires_X_mat_trans,center = TRUE, scale = TRUE)
Tires_X_mat_scaled<-data.frame(Tires_X_mat_scaled)
str(Tires_X_mat_scaled)


#Derive the components
x<-Tires_X_mat_scaled # Predictor matrix

y<-as.factor(Tire_Codes)# Class variable

# Run Plsda function
fit_plsda<-plsda(x,y,ncomp = 340)

plotIndiv(fit_plsda, comp = c(1, 2),X.label = "Comp1: 57% Exp.Var",
          Y.label = "Comp2: 11% Exp.Var")


#plotVar(fit_plsda)


# Components matrix
scores_mat_plsda<-fit_plsda$variates$X
scores_mat_plsda
dim(scores_mat_plsda)
write.csv(scores_mat_plsda,"D:\\MyResearch\\Tire\\scores_mat_plsda.csv")
scores_mat_plsda<-read.csv("D:\\MyResearch\\Tire\\pve_plsda.csv")
scores_mat_plsda
#head(scores_mat_plsda)

# #Corr matrix for components
# corr<-round(cor(score.plsda.rs),2)
# ggcorrplot::ggcorrplot(corr)
# 
# 
# # plsplsda#get the pls's score values
# score.plsda.rs <-as.data.frame(score.plsda.rs)
# # dim(scoresc.plsda.rs)
# head(score.plsda.rs)
# 
# score.plsda.rs


#Get the proportional variance explained by each components from X matrix
pve_plsda<-fit_plsda$prop_expl_var$X
pve_plsda



# pve.plsda.rs<-unname(pve.plsda.rs)
# pve.plsda.rs

#variance explained
varExp_plsda= (100*pve_plsda)
varDF_plsda= data.frame(Dimensions=1:length(varExp_plsda),
                        varExp_plsda=varExp_plsda)

# grid.arrange(gg1,gg2,ncol=2)
#3.Plot the Scores of X values 
write.csv( )
varDF_plsda

gg11<-ggplot(varDF_plsda,aes(x=Dimensions,y=varExp_plsda)) +
  geom_point() + 
  geom_line() + 
  theme_bw() +
  ylim(c(0,100)) + ylab("Proportion of Variance Explained")+
  xlab("PLS-DA Components")+ggtitle("a: Spectra")
gg11



# Calculate the cumulative proportion of variance explained
cum_prop_var <- cumsum(pve_plsda)
cum_prop_var

# Define a threshold for selecting the optimal number of PCs
threshold <- 0.95

# Find the index of the first PC where the cumulative proportion of variance exceeds the threshold
optimal_pc <- which(cum_prop_var >= threshold)[1]
optimal_pc

varExp_cum_plsda=100*cum_prop_var
varExp_cum_plsda
varDF_cum_plsda = data.frame(Dimensions=1:length(varExp_cum_plsda),
                                varExp_cum_plsda=varExp_cum_plsda)
varDF_cum_plsda
write.csv(varDF_cum_plsda,"D:\\Classes\\Spring2023\\STA6704\\HWs\\Final Project\\varDF_cum_plsda.csv")

varDF_cum_plsda=read.csv("varDF_cum_plsda.csv")
optimal_pc=89
p2<-ggplot(varDF_cum_plsda,aes(x=Dimensions,y=varExp_cum_plsda))+
  geom_point() + 
  geom_line() + 
  theme_bw() +
  ylim(c(0,100)) + ylab(" Cumulative Proportion of Variance Explained")+
  xlab("PLS-DA Components")+
  geom_vline(xintercept = optimal_pc, linetype = "dashed", color = "red")

p2

#+ggtitle("a: Resonant")
# Create a data frame for the scree plot
scree_data <- data.frame(component = 1:length(pve_plsda), pve_plsda = pve_plsda)
scree_data
# Create the scree plot using ggplot2
library(ggplot2)
ggplot(scree_data, aes(x = component, y = pve_plsda)) +
  geom_point() +
  geom_line() +
  xlab("PLS-DA Components") +
  ylab(" Proportion of Variance Explained") +
  #ggtitle("Scree Plot") +
  geom_vline(xintercept = optimal_pc, linetype = "dashed", color = "red")

###heat map


Tires_PLSDA_Selected<-scores_mat_plsda[,1:89]

dim(Tires_PLSDA_Selected)
head(Tires_PLSDA_Selected)

heatma_dat=melt(Tires_PLSDA_Selected)
heatma_dat
# Create heatmap using ggplot2
ggplot(heatma_dat) +
  geom_tile(aes(x=Var2, y=Var1, fill=value)) +
  scale_fill_gradient(low = "white", high = "red") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())
#Append Y's and PLS X's 
Tires_PLSDA_Combined<- data.frame(cbind(Tire_Codes, Tires_PLSDA_Selected))# new dataset based on PLSDAs
str(Tires_PLSDA_Combined)

head(Tires_PLSDA_Combined)
#write.csv(Tires_PLSDA_Combined,"Tires_PLSDA_Combined.csv")
Tires_PLSDA_Combined<-read.csv("D:\\Classes\\Fall2023\\STA6366\\HWs\\Final Project\\Code\\project_dataset_encoded.csv")[,-c(1,2)]
colnames(Tires_PLSDA_Combined)[1]<-"V1"
dim(Tires_PLSDA_Combined)
head(Tires_PLSDA_Combined)


#Split the dataset
set.seed(123)
Train_idx=sample(nrow(Tires_PLSDA_Combined),0.8*nrow(Tires_PLSDA_Combined))

TrainDF=Tires_PLSDA_Combined[Train_idx,]
dim(TrainDF)
TrainDF
TestDF=Tires_PLSDA_Combined[-Train_idx,]
TestDF
dim(TestDF)


########## ########################Fit the models
 

# Define models and their possible hyperparameters
models <- c("knn", "rf","nb", "svmRadial", "svmLinear", "lda", "xgbTree")
library(caret)

# Update trainControl for multi-class classification
ctrl <- trainControl(
  method = "cv",  # Using cross-validation
  number = 10,    # Number of folds
  savePredictions = "final", 
  classProbs = TRUE,  # Compute class probabilities
  summaryFunction = defaultSummary  # Default summary function for multi-class
)



params <- list(knn = expand.grid(k = 1:20),
               rf = expand.grid(mtry = seq(1, 10, 1)),
               svmRadial = expand.grid(sigma = c(0.1, 1, 10),
                                       C = c(0.01, 0.1, 1)),
               svmLinear = expand.grid(C = c(0.01, 0.1, 1)),
               lda = NULL,
               nb = expand.grid(fL=c(0,0.5,1.0),
                                usekernel = TRUE,
                                adjust=c(0,0.5,1.0)),
               xgbTree = expand.grid(
                 nrounds = c(50,100),
                 eta = c(0.01, 0.1, 0.3),
                 max_depth = c(2, 3, 5, 10),
                 gamma = c(0, 0.1, 1),
                 colsample_bytree = 1,
                 min_child_weight = 1,
                 subsample = 1
               )
)


results <- list()
# Assuming V1 is your target variable
TrainDF$V1 <- factor(make.names(as.character(TrainDF$V1)))

# Now proceed with your loop for training models
results <- list()
for (model in models) {
  cat("Training", model, "model...\n")
  tune <- train(V1 ~ .,
                data = TrainDF,
                method = model,
                tuneGrid = params[[model]],
                trControl = ctrl,
                preProc = c("center", "scale"),
                metric = "Accuracy")  # Ensure you use a valid metric like "Accuracy"
  results[[model]] <- tune
  print(tune$bestTune)
}

# Extract relevant information from each train object
results_df <- data.frame(
  Model = models,
  Best_Params = sapply(results, function(x) toString(x$bestTune)),
  Accuracy = sapply(results, function(x) max(x$results$Accuracy))
)

# Save to CSV
write.csv(results_df, "D:\\Classes\\Fall2023\\STA6366\\HWs\\Final Project\\Code\\Training_results3.csv", row.names = FALSE)
# Save the model
saveRDS(results, "D:\\Classes\\Fall2023\\STA6366\\HWs\\Final Project\\Code\\Training_results3.rds")

# Load the saved model
fit_loaded <- readRDS("D:\\Classes\\Fall2023\\STA6366\\HWs\\Final Project\\Code\\Training_results3.rds")
fit_loaded
# Evaluate models on test data
TestResults <- list()
fitted_models<-fit_loaded
fitted_models
for (model in models) {

  y_pred <- predict(fitted_models[[model]], newdata = as.data.frame(TestDF))
 
  TestDF$V1<-as.factor(TestDF$V1)

  y_pred <- factor(y_pred, levels = levels(TestDF$V1))
  
  TestResults[[model]] <- confusionMatrix(y_pred , TestDF$V1)
}
#Save test results

saveRDS(TestResults,"D:\\Classes\\Fall2023\\STA6366\\HWs\\Final Project\\Code\\TestResults3.rds")
TestResults_loaded=readRDS("TestResults3.rds")
TestResults_loaded

warnings()
# Print test results
for (model in models) {
  print(model)
  print(TestResults[[model]])
}


#compare the train performance
Models <- c("KNN", "RF","NB", "SvmRadial", "SvmLinear", "LDA", "XgbTree")
TrainSet<-c(0.8173,0.9901,0.8243,0.6016,0.9387,0.9842,0.9482)
TestSet<-c(0.8235,0.9853,0.8235 ,0.4706,0.8971,0.9853,0.9538)
classifiers_plsda<-data.frame(Models,TrainSet,TestSet)
classifiers_plsda_m<-melt(classifiers_plsda,id.vars='Models')
classifiers_plsda_m

p1=ggplot(classifiers_plsda_m,aes(variable,value))+
  geom_bar(aes(fill=Models),position = "dodge",stat = "identity")+
  xlab("Dataset  Type")+
  ylab("Accuracy ")
p1
 
#confusion matrix for LDA
Tires_Train_lda_plsda<- train(as.factor(V1) ~ ., 
                                 data=TrainDF, 
                                 method="lda",
                                 trControl=trainControl(method="cv", 
                                                        number=10, 
                                                        allowParallel=TRUE, 
                                                        verboseIter=TRUE))




#Prediction
lda_Test_plsda<-predict(object=Tires_Train_lda_plsda,
                           newdata=data.frame(TestDF))

cm_lda_test_plsda<-confusionMatrix( factor(lda_Test_plsda) ,
                                       factor(TestDF$V1))
cm_lda_test_plsda$overall[1]

p4<-ggplot(data=data.frame(cm_lda_test_plsda$table)) + 
  geom_tile(aes(x=Reference, y=Prediction, fill=Freq)) +
  xlab('Actual Classe') +
  ylab('Predicted Classe from Model')+ 
  ggtitle("The performance of LDA model")

p4

#confusion matrix for SvmRadial
tuneGrid <- expand.grid(sigma = 0.1, C=1)
Tires_Train_svmr_plsda<- train(as.factor(V1) ~ ., 
                              data=TrainDF, 
                              method="svmRadial",
                              tuneGrid = expand.grid(sigma = 0.1, C=1),
                              trControl=trainControl(method="cv", 
                                                     number=10, 
                                                     allowParallel=TRUE, 
                                                     verboseIter=TRUE))




#Prediction
svmr_Test_plsda<-predict(object=Tires_Train_svmr_plsda,
                        newdata=data.frame(TestDF))

cm_svmr_test_plsda<-confusionMatrix( factor(svmr_Test_plsda) ,
                                    factor(TestDF$V1))
cm_svmr_test_plsda$overall[1]

p5<-ggplot(data=data.frame(cm_svmr_test_plsda$table)) + 
  geom_tile(aes(x=Reference, y=Prediction, fill=Freq)) +
  xlab('Actual Classe') +
  ylab('Predicted Classe from Model')+ 
  ggtitle("The performance of SVM-Kernel model")
p5

