

install.packages("ggplot2")


library("ggplot2")

df <- read.csv("D:\\My_Docs\\univer\\DataScience\\Case_Studies\\Case_Study2\\3\\insurance.csv")

head(df, n = 5)
str(df)
summary(df)

ggplot(data = df,aes(region,charges)) + geom_boxplot(fill = c(2:5)) +
  theme_classic() + ggtitle("Boxplot of Medical Charges per Region")


library(randomForest)

train_rf <- randomForest(charges ~ ., df, ntree=600, na.action = na.roughfix)

print(train_rf)


#Call:
  
#  randomForest(formula = charges ~ ., data = df, ntree = 600, na.action = na.roughfix) 

#Type of random forest: regression

#Number of trees: 600

#No. of variables tried at each split: 2


#Mean of squared residuals: 22128352

#% Var explained: 84.9

install.packages("party")

library("party")

test_tree <- ctree(charges ~ ., data=df)

plot(test_tree, type="simple")
