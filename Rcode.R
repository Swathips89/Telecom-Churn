
install.packages("car")
library(car)
#AIC calculation for variable selection
install.packages("olsrr")
library(olsrr)
library(car) 
#package for imputation
# Install and load the R package mice
install.packages("mice")
library("mice")
#package for ggplots
install.packages("ggplot2")
library(ggplot2)
#package for Skewness
install.packages("moments")
library(moments)
#package for decision tree
install.packages("rpart")
library(rpart)
#for stepaic
install.packages("MASS")
library(MASS)
install.packages("car")
library(car)
install.packages("ggcorrplot")
library(ggcorrplot)
#confusionmatrix package
install.packages("caret")
library(caret)
install.packages("lattice")
library(latice)
#Grid view of graph
install.packages("gridExtra")
library(gridExtra)
install.packages("dplyr")
library(dplyr)
install.packages("e1071")
#ROC curve
install.packages("pROC")
library(pROC)
#correlation plot
install.packages("corrplot")
library(corrplot)

# Import file using IMPORT DATASET command on top right panel #####
Readfile <- read.csv("D:/Business data mining/Term project/Telco-Customer-Churn.csv")



#Analysis of each categorical independent variable against the dependent varaible  
Analysis_Data <- data.frame(Readfile)
ggplot(Analysis_Data, aes(x=gender, fill=Churn)) + geom_bar() + geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
ggplot(Analysis_Data, aes(x=gender, fill=Churn)) + geom_bar() + geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
ggplot(Analysis_Data, aes(x=SeniorCitizen, fill=Churn)) + geom_bar() + geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
ggplot(Analysis_Data, aes(x=Partner, fill=Churn)) + geom_bar() + geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
ggplot(Analysis_Data, aes(x=Dependents, fill=Churn)) + geom_bar() + geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
ggplot(Analysis_Data, aes(x=PhoneService, fill=Churn)) + geom_bar() + geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
ggplot(Analysis_Data, aes(x=MultipleLines, fill=Churn)) + geom_bar() + geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
ggplot(Analysis_Data, aes(x=InternetService, fill=Churn)) + geom_bar() + geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
ggplot(Analysis_Data, aes(x=OnlineSecurity, fill=Churn)) + geom_bar() + geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
ggplot(Analysis_Data, aes(x=OnlineBackup, fill=Churn)) + geom_bar() + geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
ggplot(Analysis_Data, aes(x=DeviceProtection, fill=Churn)) + geom_bar() + geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
ggplot(Analysis_Data, aes(x=TechSupport, fill=Churn)) + geom_bar() + geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
ggplot(Analysis_Data, aes(x=StreamingTV, fill=Churn)) + geom_bar() + geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
ggplot(Analysis_Data, aes(x=StreamingMovies, fill=Churn)) + geom_bar() + geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
ggplot(Analysis_Data, aes(x=Contract, fill=Churn)) + geom_bar() + geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
ggplot(Analysis_Data, aes(x=PaperlessBilling, fill=Churn)) + geom_bar() + geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
ggplot(Analysis_Data, aes(x=PaymentMethod, fill=Churn)) + geom_bar() + geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
ggplot(Analysis_Data, aes(x=Churn, fill=Churn)) + geom_bar() + geom_text(aes(label=..count..),stat="count",position=position_stack(0.5))
#to find the proportion of Churn
prop.table(table(Analysis_Data$Churn))

# to plot the continous variable against the dependent variable

grid.arrange(
  
  ggplot(Analysis_Data,aes(x = MonthlyCharges, color = Churn))+ 
    geom_freqpoly(size=2)+
    theme_minimal(),
  
  ggplot(Analysis_Data,aes(x = TotalCharges, color = Churn))+ 
    geom_freqpoly(size=2)+
    theme_minimal(),
  
  ggplot(Analysis_Data,aes(x = tenure, color = Churn))+ 
    geom_freqpoly(size=2)+
    theme_minimal()
  
)

# to understand the tensure better-Creating tensurcat column to check the frequency 
Analysis_Data <- Analysis_Data  %>% mutate(tenureCat = as.factor(if_else(tenure <= 12, "0-1 Year",
                                                   if_else(tenure <= 24, "1-2 Year",
                                                           if_else(tenure <= 36, "2-3 Year",
                                                                   if_else(tenure <= 48, "3-4 Year",
                                                                           if_else(tenure <= 60, "4-5 Year", "5-6 Year")))))))

ggplot(Analysis_Data, aes(tenureCat, fill = Churn))+
  geom_bar()+
  coord_flip()+
  labs(y = "Frequecny", x = "Tenure Category")+
  scale_fill_ordinal()+
  theme_minimal()

# to find data types of all columns in the data frame
sapply(Analysis_Data, class)

# to find the missing values in the data frame
apply(is.na(Analysis_Data), 2, which)

# Impute missing data
Analysis_Data$TotalCharges[is.na(Analysis_Data$TotalCharges)] <- 0

#check if imputation has been applied successfully
apply(is.na(Analysis_Data), 2, which)

#check the skewness
skewness(Analysis_Data$tenure)
skewness(Analysis_Data$MonthlyCharges)
skewness(Analysis_Data$TotalCharges) 


#Recoding the Categorical data

Analysis_Data$gender <- factor(Analysis_Data$gender, levels=c("Male","Female"), labels=c(0,1))
Analysis_Data$Partner<- factor(Analysis_Data$Partner, levels=c("No","Yes"), labels=c(0,1))
Analysis_Data$Dependents<- factor(Analysis_Data$Dependents, levels=c("No","Yes"), labels=c(0,1))
Analysis_Data$PhoneService<- factor(Analysis_Data$PhoneService, levels=c("No","Yes"), labels=c(0,1))
Analysis_Data$MultipleLines<- factor(Analysis_Data$MultipleLines, levels=c("No","Yes","No phone service"), labels=c(0,1,2))
Analysis_Data$InternetService<- factor(Analysis_Data$InternetService, levels=c("DSL","Fiber optic","No"), labels=c(0,1,2))
Analysis_Data$OnlineSecurity<- factor(Analysis_Data$OnlineSecurity, levels=c("No","Yes","No internet service"), labels=c(0,1,2))
Analysis_Data$OnlineBackup<- factor(Analysis_Data$OnlineBackup, levels=c("No","Yes","No internet service"), labels=c(0,1,2))
Analysis_Data$DeviceProtection<- factor(Analysis_Data$DeviceProtection, levels=c("No","Yes","No internet service"), labels=c(0,1,2))
Analysis_Data$TechSupport<- factor(Analysis_Data$TechSupport, levels=c("No","Yes","No internet service"), labels=c(0,1,2))
Analysis_Data$StreamingTV<- factor(Analysis_Data$StreamingTV, levels=c("No","Yes","No internet service"), labels=c(0,1,2))
Analysis_Data$StreamingMovies<- factor(Analysis_Data$StreamingMovies, levels=c("No","Yes","No internet service"), labels=c(0,1,2))
Analysis_Data$Contract<- factor(Analysis_Data$Contract, levels=c("Month-to-month","One year","Two year"), labels=c(0,1,2))
Analysis_Data$PaperlessBilling<- factor(Analysis_Data$PaperlessBilling, levels=c("No","Yes"), labels=c(0,1))
Analysis_Data$PaymentMethod<- factor(Analysis_Data$PaymentMethod, levels=c("Electronic check","Mailed check","Bank transfer (automatic)","Credit card (automatic)"), labels=c(0,1,2,3))
Analysis_Data$Churn<- factor(Analysis_Data$Churn, levels=c("No","Yes"), labels=c(0,1))

##Applying the categorical variables covereted data into Final_data data frame
Convert_data <- Analysis_Data


# Removing the Customer ID field #####
Final_data <-  Convert_data[,-1] 
Final_data <-  Final_data[,-21]
# Summary of the file #####
summary(Final_data)



#to check the correlation of continous variables

options(repr.plot.width =6, repr.plot.height = 4)
telco_cor <- round(cor(Final_data[,c("tenure", "MonthlyCharges", "TotalCharges")]), 1)
ggcorrplot(telco_cor,  title = "Correlation")+theme(plot.title = element_text(hjust = 0.5))



# decision tree 
Desc_Tree <- rpart(Churn~.,method="class", data=Final_data)
printcp(Desc_Tree) # display the results
plotcp(Desc_Tree) # visualize cross-validation results
summary(Desc_Tree) # detailed summary of splits

# plot tree
plot(Desc_Tree, uniform=TRUE,
     main="Classification Tree for Telco_Churn")
text(Desc_Tree, use.n=TRUE, all=TRUE, cex=.8)

## 70% of the sample size
smp_size <- floor(0.70 * nrow(Final_data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(Final_data)), size = smp_size)

train <- Final_data[train_ind, ]
test <- Final_data[-train_ind, ]

# REGRESSION on train data #######################
model_1=glm(Churn~.,data=train,family = "binomial")
summary(model_1)



#Stepwise Variable selection 
model_2<- stepAIC(model_1, direction="both")
summary(model_2)

#Regression on the variables obtained from step AIC
model_3 <- glm(formula = Churn ~ SeniorCitizen + Dependents + tenure + MultipleLines + 
            InternetService + OnlineSecurity + TechSupport + StreamingTV + 
            StreamingMovies + Contract + PaperlessBilling + PaymentMethod + 
            MonthlyCharges + TotalCharges, family = "binomial", data = train)
summary(model_3)

## create predictions for the test (evaluation) data set
pred <-predict(model_3,newdata=data.frame(test),type="response")
summary(pred)

#annova analysis
anova_pred <- anova(model_3, test="Chisq")
anova_pred

#Assessing the predictive ability of the model
fitted.results <- pred
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Churn)
print(paste('Accuracy',1-misClasificError))

#Confusion Matrix
train_prob <- predict(model_3, data = train, type = "response") 
test_prob <- predict(model_3, newdata = test, type = "response")

train_pred <- factor(ifelse(train_prob >= 0.5, "1", "0"))
train_actual <- factor(ifelse(train$Churn == "1", "1", "0"))
test_pred <- factor(ifelse(test_prob >= 0.5, "1", "0"))
test_actual <- factor(ifelse(test$Churn == "1", "1", "0"))

confusionMatrix(data = train_pred, reference = train_actual)

confusionMatrix(data = test_pred, reference = test_actual)


#AUC
auc(predictor=pred,response=test$Churn) 

#ROC curve
g1 <-roc(predictor=pred,response=test$Churn)
par(pty="s")
plot(g1, xlab = "1 - Specificity")

##rmse (Final_data$Churn,pred$Churn)

# Fetch the actual values from auto validation dataset
obs=Final_data$Churn[-train]

# compute the difference of actual - prediction
diff=obs-pred

# Caluclate the percentage difference
percdiff=abs(diff)/obs

# Calculate performance metrics using the autal - predicted values
me=mean(diff)
rmse=sqrt(sum(diff**2)/n2)
mape=100*(mean(percdiff))
rmse # root mean square error


