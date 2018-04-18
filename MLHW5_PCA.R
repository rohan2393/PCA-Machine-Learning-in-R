mnist<-read.csv("/Users/rohan/Desktop/DS630_MachineLearning/HW5/mnist.csv",sep = ",", header = F)
# Removing the first variable
mnist <-mnist[,-1]
# Cheacking for NA's and cleaning
mnist_clean<-na.omit(mnist)



#-----------------------------------------------------------QUESTION 1
#Employ either the princomp or prcomp function to perform PCA on the 784 columns of the data set which contain the grey scale pixel information.
#Do not include the first column as this column merely indicates the digit 0-9 that a given row encodes.
pca_mnist_model<-prcomp(mnist_clean)
#View(head(pca_mnist_model))
#View(head(pca_mnist_model$x))
#View(head(pca_mnist_model$rotation))
#View(head(pca_mnist_model$center,50))
#View(head(pca_mnist_model$scale))
#summary(pca_mnist_model)
#names(pca_mnist_model)
#----------------------------------------------------------------------------------------------------------------------



#compute standard deviation of each principal component
std_dev <- pca_mnist_model$sdev
#View(head(std_dev))
#compute variance of each principal component
variance <- std_dev^2
#View(head(variance))
#checking variance of first 10 components for drawing some inferences
variance[1:10]
#proportion of variance
proportion_variance <- variance/sum(variance)
proportion_variance[1:20]
#scree plot
plot(proportion_variance, xlab = "Principal Component", ylab = "Proportion of Variance", type = "b")
#cumulative scree plot
plot(cumsum(proportion_variance), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance", type = "b")



#-----------------------------------------------------------QUESTION 2
# Report the number of principal components needed to account for 98% of the variance of the original data set.
# Taking inferences from the graph and then selecting the Principal components
sum(proportion_variance[1:261])
# 261 Principal components account for 0.9801387 of the variance of the original data set
#----------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------QUESTION 3
# For each of the principal components reported in 2. supply the percentage of the variance that each component contributes to the overall variance.
percetage_variance<-100*(proportion_variance[1:261])
View(head(percetage_variance,50))
#----------------------------------------------------------------------------------------------------------------------

#-----------------------------------------------------------QUESTION 4
#Are there pixels in the original data set that consistently appear with significant loadings in the principal components reported in 2.?

significant_loadings1<-pca_mnist_model$rotation[,(1:261)]
#significant_loadings1<-abs(significant_loadings1)
#View(head(significant_loadings1))
significant_loadings2<-rowSums(significant_loadings1!=0)
#View(significant_loadings2)
significant_loadings<-(significant_loadings2[rev(order(significant_loadings2))])
View(significant_loadings)

#-----------------------------------------------------------BONUS
#Graph the first 10 principal components in a 28 × 28 pixel grid.
Bonus<-pca_mnist_model$x[,1:261]%*%t(pca_mnist_model$rotation[,1:261])
#View(head(Bonus))
for (i in 1:10)
{
  principal_components <- as.matrix(Bonus[i,])
  image(matrix(unlist(principal_components), ncol = 28, nrow = 28))
  title(paste("Principal Component", i))
}
#----------------------------------------------------------------------------------------------------------------------