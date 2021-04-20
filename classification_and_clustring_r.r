# -*- coding: utf-8 -*-
"""

**Question 1**







"""

#1. Load the data
data <- read.csv("/content/19.csv")

#2. View the structure of Data and Dimentionality

str(data)

#3. Attribute names

names(data)

#4. Summary of dataset

summary(data)







"""**Q2: Explain what attributes you are using for clustering and why**
"""# **WE are using complete set of features. Later we will analyze the impact of diferent number of features**"""






**Q3: Apply Clustering on the dataset for several k values 3,4,5**







# **Data cleaning**
"""
#Dataset consist unknown values '?' we omit them from dataset
df1<-data[!(data$Age=="?" | data$Shape=="?" | data$Margin=="?"   | data$Density=="?" |data$Severity=="?"),] #Margin Density 	BI.RADS
df1




"""# **Characters to Numeric**"""
#Dataset consist of character values we converted them into numeric values
df1$Age  <- as.numeric(df1$Age)
df1$Shape  <- as.numeric(df1$Shape)
df1$Margin  <- as.numeric(df1$Margin)
df1$Density  <- as.numeric(df1$Density)
df1$Severity  <- as.numeric(df1$Severity)
str(df1)






"""# **Optimal Number of Clusters**"""
#We are searching optimize numbers of clusters using elbow method
df2=df1[2:6]
wcss <- vector()
for (i in 1:10) wcss[i] <-sum(kmeans(df2,i)$withinss)
plot(1:10,wcss,type = "b",xlab="Numbers of clusters")





"""**Q4: Plot the model and show the centroid for each K**







# **K means with 5 Clusters**
"""

kmeans <- kmeans(df2,5,iter.max=300,nstart=10)
library(cluster)
clusplot(df2,kmeans$cluster,lines=0,shade=TRUE,color=TRUE,labels=2,plotchar=FALSE,span=TRUE)






"""# **K means with 4 Clusters**"""

kmeans <- kmeans(df2,4,iter.max=300,nstart=10)
library(cluster)
clusplot(df2,kmeans$cluster,lines=0,shade=TRUE,color=TRUE,plotchar=FALSE,span=TRUE)






"""# **K means with 3 Clusters**"""

kmeans <- kmeans(df2,3,iter.max=300,nstart=10)
library(cluster)
clusplot(df2,kmeans$cluster,lines=0,shade=TRUE,color=TRUE,plotchar=FALSE,span=TRUE)






"""# **K means with 5 Clusters after scaling**"""

scaled_data=scale(df1[2:6])
kmeans <- kmeans(scaled_data,5,iter.max=300,nstart=10)
library(cluster)
clusplot(scaled_data,kmeans$cluster,lines=0,shade=TRUE,color=TRUE,plotchar=FALSE,span=TRUE)





"""# **K means with 4 Clusters after scaling**"""

kmeans <- kmeans(scaled_data,4,iter.max=300,nstart=10)
library(cluster)
clusplot(scaled_data,kmeans$cluster,lines=0,shade=TRUE,color=TRUE,plotchar=FALSE,span=TRUE)







"""# **K means with 3 Clusters after scaling**"""

kmeans <- kmeans(scaled_data,3,iter.max=300,nstart=10)
library(cluster)
clusplot(scaled_data,kmeans$cluster,lines=0,shade=TRUE,color=TRUE,plotchar=FALSE,span=TRUE)







"""**Q5: Select different attributes and show which ones show good clusters**

# **Select Attributes which shows better clustering results**
"""



#We are clustering the dataset through Age and Shape features to find best possible clusters
selected=scale(df1[2:3])

kmeans <- kmeans(selected,3,iter.max=300,nstart=10)
library(cluster)
clusplot(selected,kmeans$cluster,lines=0,shade=TRUE,color=TRUE,plotchar=FALSE,span=TRUE)







#We are clustering the dataset through Age,Shape,margin and density  features to find best possible clusters
selected=scale(df1[2:5])
kmeans <- kmeans(selected,3,iter.max=300,nstart=10)
library(cluster)
clusplot(selected,kmeans$cluster,lines=0,shade=TRUE,color=TRUE,plotchar=FALSE,span=TRUE)





#We are clustering the dataset through Shape,margin and density  features to find best possible clusters
selected=scale(df1[3:5])
kmeans <- kmeans(selected,3,iter.max=300,nstart=10)
library(cluster)
clusplot(selected,kmeans$cluster,lines=0,shade=TRUE,color=TRUE,plotchar=FALSE,span=TRUE)




#We are clustering the dataset throug margin and density  features to find best possible clusters
selected=scale(df1[4:5])
kmeans <- kmeans(selected,3,iter.max=300,nstart=10)
library(cluster)
clusplot(selected,kmeans$cluster,lines=0,shade=TRUE,color=TRUE,plotchar=FALSE,span=TRUE)






"""**Q6: Find most important features**

# **Important features**
"""





#Visualizing features that have high impact on outcome of the model descending order
number.perfect.splits <- apply(X=df1[1:5], MARGIN = 2, FUN = function(col){
t <- table(df1$Age,col)
sum(t == 0)
})
# Descending order of perfect splits
order <- order(number.perfect.splits,decreasing = TRUE)
number.perfect.splits <- number.perfect.splits[order]
# Plot graph
par(mar=c(10,2,2,2))
barplot(number.perfect.splits,
main="Number of perfect splits vs feature",
xlab="",ylab="Feature",las=2,col="wheat")










"""**7. Apply classification using decision tree on the most 4 important features**

# **Classification**
"""



#Installing packages
require(tree)
#Installing libraries
install.packages('rpart')
install.packages('caret')
install.packages('rpart.plot')
install.packages('rattle')
 








#Loading libraries
library(rpart,quietly = TRUE)
library(caret,quietly = TRUE)
library(rpart.plot,quietly = TRUE)
library(rattle)




#cleaaning dataset and preparing the dataset
df1<-data[!(data$Age=="?" | data$BI.RADS=="?" | data$Shape=="?" | data$Margin=="?"   | data$Density=="?" |data$Severity=="?"),] #Margin Density 	BI.RADS

dataset1=df1[,c("Age","BI.RADS","Margin","Density","Severity")]
#dataset1$Severity= factor(dataset1$Severity, levels = c(0, 1))
dataset=dataset1[1:5]




# Fitting Decision Tree Classification to the Training set
# install.packages('rpart')
tree = rpart(Severity ~ ., data = dataset)
# seat_tree = tree(Sales ~ ., data = Carseats, 
#                  control = tree.control(nobs = nrow(Carseats), minsize = 10))
summary(tree)

plot(tree)
text(tree, pretty = 0)
title(main = "Unpruned Classification Tree")








# Splitting the dataset into the Training set and Test set
set.seed(2)
idx = sample(1:nrow(dataset), 664)
trn = dataset[idx,]
tst = dataset[-idx,]




#Training
tree = rpart(Severity ~ ., data = trn)
# seat_tree = tree(Sales ~ ., data = Carseats, 
#                  control = tree.control(nobs = nrow(Carseats), minsize = 10))
summary(tree)





#Pridictions
trn_pred = predict(tree, trn, type = "class")
tst_pred = predict(tree, tst[(0:50),], type = "class")







# Confusion matrix
table(predicted = tst_pred, actual = tst[(0:50),]$Severity)