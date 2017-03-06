#Jason Laso
#3/6/17
#KNN Tutorial using Prostate Cancer Data


library(class)
library(gmodels)
library(ggplot2)

#set wd and read in data
setwd("C:\\Users\\ja041718\\Downloads")
prc = read.csv("Prostate_Cancer.csv", stringsAsFactors = F)

#check structure of prc
str(prc)

#remove id column since it provides no classification and is unique to all observations
prc = prc[-1]

#turn diagnosis result into a factor
prc$diagnosis = factor(prc$diagnosis_result, levels = c("B", "M"), labels = c("Benign", "Malignant"))

#check proportion of benign to malignant
prop.table(table(prc$diagnosis))

#Since predictors have many different scales, we need to normalize them.
#Create function to normalize a vector
normalize = function(x){
  return ( (x - min(x))/(max(x)-min(x)) )
}

#Create a new DF with normalized values for PRC predictors
prc_n = as.data.frame(lapply(prc[,2:9], normalize))

#add the labels
prc_n$diagnosis = prc$diagnosis

#Check to make sure normalization worked
summary(prc_n)

#set seed to reproduce results
set.seed(2)

# randomly select rows in prc_n set for training and testing. Start by creating random sample
sample = sample(1:nrow(prc_n), replace= F)

#shuffle the prc_n DF based on the random sample
prc_n = prc_n[sample,]

#create a cutoff number between where the training and testing data will separate in the original DF.
#use 65% for training, 35% for testing
cutoff = round(nrow(prc_n)) * .65

#create training and testing sets
prc_train = prc_n[1:cutoff,]
prc_test = prc_n[(cutoff+1):nrow(prc_n),]

#now take the diagnosis labels for all observations
prc_train_labels = prc_n[1:cutoff, 9]
prc_test_labels = prc_n[(cutoff+1) : nrow(prc_n), 9]

#which means we can remove them from the test and train sets so that we won't have to subset them off later
prc_train = prc_train[-9]
prc_test = prc_test[-9]

#train the KNN model on the training set to apply on test. set k to sqrt of # of observations(10)
prc_test_pred = knn(train = prc_train, test=prc_test, cl = prc_train_labels, k=10)

#Produce cross tabulation of results of k=10 model
CrossTable(prc_test_labels, prc_test_pred, prop.chisq = F)

#The results were not great (60%), so let's try to see if there is a better k.

#set seed
set.seed(2)

#initialize vectors to hold values in loop
pred_i=c()
knn_accuracy = c()

#all of the k's we want to try
possible_k = 1:25

for(i in possible_k){
  
  #train a model on k = i
  pred_i = knn(train = prc_train, test=prc_test, cl = prc_train_labels, k= i)
  
  #calculate the total accuracy into a list
  knn_accuracy[i] = sum(pred_i == prc_test_labels) / length(prc_test_labels)
  
}

#create DF to store results of the loop
knn_loop_results = data.frame(k = possible_k, accuracy = knn_accuracy)

#store value of largest possible accuracy
max = knn_loop_results[which.max(knn_loop_results$accuracy),]

#visualize results in scatterplot
ggplot(knn_loop_results, aes(k, accuracy)) + geom_point() +
  #color the maximum accuracy k as red and label it blue
  geom_point(data=max, col="red", size=4) +
  geom_text(data=max, label="Max", vjust=1, col="blue")

#So we can improve the model's accuracy by over 8% by increasing k to 11.

#Set the test predictions for k=11
prc_test_pred = knn(train = prc_train, test=prc_test, cl = prc_train_labels, k=11)

#Create the pretty cross tabulation
CrossTable(prc_test_labels, prc_test_pred, prop.chisq = F)

# Model accurately predicts 4/5 benign obs and 20/30 malignant for a 68.5% accuracy.
# However, note that ir predicts a ratio of 85.7% : 14.3% malignant to benign, which is way off the
# original data's 38% : 62% ratio. Therefore, the model performance is way off.

#we can further see the difficulty in the model's projections by plotting all of the test points and
#seeing the disparity in the data compared to the predictions.
ggplot(prc_test, aes(x=radius, y=texture, col=prc_test_pred)) + geom_point()
