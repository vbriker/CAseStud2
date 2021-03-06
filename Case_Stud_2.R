
<<<<<<< HEAD
install.packages("psych")
install.packages("party")
install.packages("ggplot2")
=======

#install.packages("psych")
#install.packages("party")
#install.packages("ggplot2")
#install.packages("dplyr")

>>>>>>> c056b79eca3cdf34b2b34489b5032d35cbc50cc7


library("ggplot2")
library("psych")
library("randomForest")
<<<<<<< HEAD
library("party")
=======
library("dplyr")
>>>>>>> c056b79eca3cdf34b2b34489b5032d35cbc50cc7

df <- read.csv("insurance.csv")
df$charges_log=log(df$charges)

head(df, n = 5)
str(df)
summary(df)

ggplot(data = df,aes(region,charges)) + 
  geom_boxplot(fill = c(2:5)) +
  theme_classic() + 
  ggtitle("Boxplot of Medical Charges per Region")+
  ylab("Charges") + xlab("Region")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = df,aes(smoker,charges)) + 
  geom_boxplot(fill = c(2:3)) +
  theme_classic() + 
  ggtitle("Boxplot of Medical Charges by Smoking Status")+
  ylab("Charges") + xlab("Smoking Status")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = df,aes(sex,charges)) + geom_boxplot(fill = c(2:3)) +
  theme_classic() + ggtitle("Boxplot of Medical Charges by Gender")+
  ylab("Charges") + xlab("Gender")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = df,aes(as.factor(children),charges)) + geom_boxplot(fill = c(2:7)) +
  theme_classic() +  xlab("children") +
  ggtitle("Boxplot of Medical Charges by Number of Children")+
  ylab("Charges") + xlab("Number of Children")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

pairs.panels(df[c("age" ,"bmi", "children","smoker" , "charges","charges_log")])

# linear model 
# charges_log=b0+b1*age+b2*age*smoker
Lin_model <- lm(charges_log ~ age + age * smoker , data = df)
summary(Lin_model)

#creation of train and test data
df_train <- round(0.75 * nrow(df))
train_indices <- sample(1:nrow(df), df_train)
df_train <- df[train_indices, ]
df_test <- df[-train_indices, ]

df_test$prediction <- exp(predict(Lin_model, newdata = df_test))

df_test$residuals <- df_test$charges - df_test$prediction
Lim_rmse <- sqrt(mean(df_test$residuals^2))

### Model Performance

#Charges vs predicted
ggplot(df_test, aes(x = prediction, y = charges)) + 
  geom_point(color = "blue") + 
  geom_abline(color = "red") +
  ggtitle("Prediction vs. Real values")+
  ylab("Charges") + xlab("Predicted Charges")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# residual vs predicted
ggplot(data = df_test, aes(x = prediction, y = residuals)) +
  geom_pointrange(aes(ymin = 0, ymax = residuals), color = "blue", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = 3, color = "red") +
  ggtitle("Residuals vs. Linear model prediction")+
  ylab("Residuals") + xlab("Predicted Charges")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))



train_rf <- randomForest(charges ~ ., df, ntree=100, na.action = na.roughfix)
print(train_rf)
<<<<<<< HEAD

#Call:
  
#  randomForest(formula = charges ~ ., data = df, ntree = 600, na.action = na.roughfix) 

#Type of random forest: regression

#Number of trees: 600

#No. of variables tried at each split: 2


#Mean of squared residuals: 22128352

#% Var explained: 84.9

=======
>>>>>>> c056b79eca3cdf34b2b34489b5032d35cbc50cc7
test_tree <- ctree(exp(charges) ~ ., data=df)
plot(test_tree, type="simple")

