rm(list = ls())
#############################Setting Working Directory##############################################
setwd("C:/Users/admin/Desktop/Analytics Basics/Project/Churn")
#############################Loading the Data#######################################################
Churn_Train = read.csv("Train_data.csv", header = T, na.strings = c(" ", "", "NA"))
Churn_Test = read.csv("Test_data.csv", header = T, na.strings = c(" ", "", "NA"))
#############################Load Libraries#########################################################
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information", "MLmetrics",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees', "gplots", "scales", "psych", "Boruta", "class")


lapply(x, require, character.only = TRUE)
rm(x)
#############################COMBINING TRAIN AND TEST Data##########################################
Churn_Train$train = 1
Churn_Test$train = 2

bind = rbind(Churn_Train,Churn_Test)
bind_back = bind

#############################Processing and Cleaning the Data#######################################
bind$Churn = factor(x = bind$Churn, labels = 0:(length(unique(bind$Churn))-1))
bind$voice.mail.plan = factor(x = bind$voice.mail.plan, labels = 1:length(unique(bind$voice.mail.plan)))
bind$international.plan = factor(x = bind$international.plan, labels = 1:length(unique(bind$international.plan)))
bind$phone.number = NULL

numeric_index = sapply(bind,is.numeric) #Selecting only numeric column
numeric_data = bind[,numeric_index] 

# Correlation Plot 
corrgram(bind[,numeric_index], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")  

bind$total.day.minutes = NULL
bind$total.eve.minutes = NULL
bind$total.night.minutes = NULL
bind$total.intl.minutes = NULL

#############################FEATURE  SELECTION#####################################################
b = Boruta(Churn ~ .-train, data = bind, doTrace=1)
par(mar=c(10.1,4.1,4.1,2.1))
plot(b, las=2)
bb = attStats(b)
bb = bb[bb$decision=='Confirmed', ]
selected_cols = rownames(bb)

bind = bind[, c(selected_cols, "train")]
bind$Churn = bind_back$Churn
bind$Churn = factor(x = bind$Churn, labels = 0:(length(unique(bind$Churn))-1))
#############################OUTLIER ANALYSIS#######################################################
numeric_index = sapply(bind,is.numeric)
numeric_data = bind[,numeric_index]
cnames = colnames(numeric_data)

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i])), data = subset(bind))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i])+
           ggtitle(paste("Box plot of",cnames[i])))
}

gridExtra::grid.arrange(gn1, ncol=1)
gridExtra::grid.arrange(gn2, ncol=1)
gridExtra::grid.arrange(gn3, ncol=1)
gridExtra::grid.arrange(gn4, ncol=1)
gridExtra::grid.arrange(gn5, ncol=1)
gridExtra::grid.arrange(gn6, ncol=1)
gridExtra::grid.arrange(gn7, ncol=1)
gridExtra::grid.arrange(gn8, ncol=1)

#Replace all outliers with NA and impute
for(i in cnames){
  val = bind[,i][bind[,i] %in% boxplot.stats(bind[,i])$out]
  print(length(val))
  bind[,i][bind[,i] %in% val] = NA
}

bind = knnImputation(bind, k = 5)

#############################FEATURE SCALING########################################################
x <- bind$number.vmail.messages #Normalization
h<-hist(x, breaks=10, col="red", xlab="number. vmail.messages per day", 
        main="Histogram with Normal Curve before Scaling") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

x <- bind$total.day.charge #Standardization
h<-hist(x, breaks=10, col="red", xlab="Total day charge", 
        main="Histogram with Normal Curve before Scaling") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

x <- bind$total.eve.charge #Standardization
h<-hist(x, breaks=10, col="red", xlab="Total Evenning charge", 
        main="Histogram with Normal Curve before Scaling") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

x <- bind$total.night.charge #Standardization
h<-hist(x, breaks=10, col="red", xlab="Total Night charge", 
        main="Histogram with Normal Curve before Scaling") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

x <- bind$number.customer.service.calls #Normalization
h<-hist(x, breaks=10, col="red", xlab="Number of Customer Service Call", 
        main="Histogram with Normal Curve before Scaling") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

x <- bind$total.intl.calls #Normalization
h<-hist(x, breaks=10, col="red", xlab="Total International Call", 
        main="Histogram with Normal Curve before Scaling") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

x <- bind$total.intl.charge #Standardization
h<-hist(x, breaks=10, col="red", xlab="Total International charge", 
        main="Histogram with Normal Curve before Scaling") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

#Normalization
for(x in colnames(bind[,c(3,7,9)])){
  bind[, x] = (bind[, x] - min(bind[, x]))/(max(bind[, x])-min(bind[, x]))
}
#Standardization
for(x in colnames(bind[,c(4,5,6,8)])){
  bind[, x] = (bind[, x] - mean(bind[, x]))/(sd(bind[, x]))
}

x <- bind$number.vmail.messages #Normalization
h<-hist(x, breaks=10, col="red", xlab="number. vmail.messages per day", 
        main="Histogram with Normal Curve after Scaling") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

x <- bind$total.day.charge #Standardization
h<-hist(x, breaks=10, col="red", xlab="Total day charge", 
        main="Histogram with Normal Curve after Scaling") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

x <- bind$total.eve.charge #Standardization
h<-hist(x, breaks=10, col="red", xlab="Total Evenning charge", 
        main="Histogram with Normal Curve after Scaling") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

x <- bind$total.night.charge #Standardization
h<-hist(x, breaks=10, col="red", xlab="Total Night charge", 
        main="Histogram with Normal Curve after Scaling") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

x <- bind$number.customer.service.calls #Normalization
h<-hist(x, breaks=10, col="red", xlab="Number of Customer Service Call", 
        main="Histogram with Normal Curve after Scaling") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

x <- bind$total.intl.calls #Normalization
h<-hist(x, breaks=10, col="red", xlab="Total International Call", 
        main="Histogram with Normal Curve after Scaling") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

x <- bind$total.intl.charge #Standardization
h<-hist(x, breaks=10, col="red", xlab="Total International charge", 
        main="Histogram with Normal Curve after Scaling") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

#############################Splitting Data Into TRAIN AND TEST#####################################

Churn_Train = bind[bind$train == 1, ]
Churn_Test = bind[bind$train == 2, ]

Churn_Train$train = NULL
Churn_Test$train = NULL
#############################Logistic Regression####################################################

log = glm(formula = Churn_Train$Churn ~ ., family = 'binomial', data = Churn_Train)

#predict using logistic regression
predicted = predict(log, newdata = Churn_Test[, names(Churn_Test) != "Churn"])

#convert prob
predicted = ifelse(predicted>=0.5, 1, 0)

#Evaluate the performance of classification model
ConfMatrix_LR = table(actual = Churn_Test$Churn, predicted=predicted)


acc_LR = sum(diag(ConfMatrix_LR))/length(predicted) #Accuracy
f1_LR = F1_Score(y_true = Churn_Test$Churn, y_pred = predicted)
#############################Decision Tree##########################################################
#Develop Model on training data
DT = rpart(Churn_Train$Churn ~ ., data = Churn_Train, method = "class")

#Plot Tree
par(mar=c(0,0,0,0))
plot(DT, uniform=TRUE)
text(DT,use.n=TRUE, all=TRUE, cex=.8)

#Predict for test cases
predicted_DT = predict(DT, newdata = Churn_Test[, names(Churn_Test) != "Churn"], type = "class")

#Evaluate the performance of classification model
ConfMatrix_DT = table(actual = Churn_Test$Churn, predicted=predicted_DT)

acc_DT = sum(diag(ConfMatrix_DT))/length(predicted) #Accuracy
f1_DT = F1_Score(y_true = Churn_Test$Churn, y_pred = predicted_DT)

#############################Decision Tree Using C5.0 Alogorithm####################################

C50_model = C5.0(Churn_Train$Churn ~., Churn_Train, trials = 100, rules = TRUE)
plot(C50_model)
#Summary of DT model
summary(C50_model)

#write rules into disk
write(capture.output(summary(C50_model)), "c50Rules.txt")

#Predict for test cases
C50_Predictions = predict(C50_model, Churn_Test[,-10], type = "class")

##Evaluate the performance of classification model
ConfMatrix_C50 = table(Churn_Test$Churn, C50_Predictions)
confusionMatrix(ConfMatrix_C50)
acc_c50 = sum(diag(ConfMatrix_C50))/length(C50_Predictions)
f1_C50 = F1_Score(y_true = test$Churn, y_pred = C50_Predictions)
#############################Random Forest################################################################

RF_model = randomForest(Churn_Train$Churn ~ ., Churn_Train, importance = TRUE, ntree = 500)
plot(RF_model)
#Extract rules fromn random forest
#transform rf object to an inTrees' format
treeList = RF2List(RF_model) 
# 
# #Extract rules
 exec = extractRules(treeList, train[,-10])  # R-executable conditions
# 
# #Visualize some rules
 exec[1:2,]
# 
# #Make rules more readable:
 readableRules = presentRules(exec, colnames(Churn_Train))
 readableRules[1:2,]
# 
# #Get rule metrics
 ruleMetric = getRuleMetric(exec, Churn_Train[,-10], Churn_Train$Churn)  # get rule metrics
# 
# #evaulate few rules
 ruleMetric[1:2,]

#Predict test data using random forest model
RF_Predictions = predict(RF_model, Churn_Test[,-10])

##Evaluate the performance of classification model
ConfMatrix_RF = table(Churn_Test$Churn, RF_Predictions)
confusionMatrix(ConfMatrix_RF)
acc_RF = sum(diag(ConfMatrix_RF))/length(RF_Predictions)
f1_RF = F1_Score(y_true = test$Churn, y_pred = RF_Predictions)
#############################KNN Implementation#############################################################

KNN_Predictions = knn(Churn_Train[, 1:9], Churn_Test[, 1:9], Churn_Train$Churn, k = 7)


Conf_matrix_knn = table(KNN_Predictions, Churn_Test$Churn)
acc_KNN = sum(diag(Conf_matrix_knn))/length(KNN_Predictions)
f1_KNN = F1_Score(y_true = test$Churn, y_pred = KNN_Predictions)
#############################Naive Bayes##############################################################

NB_model = naiveBayes(Churn_Train$Churn ~ ., data = Churn_Train)
                      
NB_Predictions = predict(NB_model, Churn_Test[,1:9], type = 'class')

Conf_matrix_NB = table(observed = Churn_Test[,10], predicted = NB_Predictions)
acc_Naive = sum(diag(Conf_matrix_NB))/length(NB_Predictions)
f1_Naive = F1_Score(y_true = test$Churn, y_pred = NB_Predictions)
#############################Conclusion##############################################################

# From the above summary table we can se that best accuracy, F1 score and False Negative Rate is achieved 
# by Random Forest which is 0.9646071 0.9798978 and 54 respectively.