library(class)
library(caret)

#Importing Data
#Ciration for Data Set:
#Little, M.A., McSharry, P.E., Roberts, S.J. et al. Exploiting Nonlinear Recurrence 
#and Fractal Scaling Properties for Voice Disorder Detection. BioMed Eng OnLine 6, 23 (2007).
#https://doi.org/10.1186/1475-925X-6-23
data <- read.table("parkinsons.data", sep = ",", header = TRUE)

#Normalize Data to remove bias
Normalize <- function(x){
  x <- ((x-min(x)) / (max(x) - min(x)))
}

Norm_data <- as.data.frame(lapply(data[,-c(1,18)], FUN = Normalize))

#Testing and Training Sets
set.seed(250)
ratio <- sample(1:nrow(Norm_data), size = nrow(Norm_data)*0.7, replace = FALSE)
train_data <- Norm_data[ratio, ] #70% Training set
test_data <- Norm_data[-ratio, ] #30% Testing set

#Separate DF for status (target feature)
train_data_status <- data[ratio, 18]
test_data_status <- data[-ratio, 18]

#Finds accuracy for each k value to automatically determine most optimal k to use
Opt_K <- matrix(ncol = 1, nrow = ncol(Norm_data))
for(i in 1:ncol(Norm_data)){
  Knn_model <- knn(train = train_data, test = test_data, cl = train_data_status, k = i)
  Opt_K[i,1] <- 100 * sum(test_data_status == Knn_model)/NROW(test_data_status)
}
Opt_K_index <- which.max(Opt_K)
plot(Opt_K) #Plot to visualize the K value vs Accuracy

#KNN model and Accuracy calculation
Knn_opt <- knn(train = train_data, test = test_data, cl = train_data_status, k = Opt_K_index)
Acc_opt <- 100 * sum(test_data_status == Knn_opt)/NROW(test_data_status)

#Analysis of Model
table(Knn_opt, test_data_status)
confusionMatrix(table(Knn_opt, test_data_status))

