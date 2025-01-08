install.packages("factoextra")
library(factoextra)

###Objective: To perform non-hierarchical (k-means) and hierarchical (Ward) clustering on the USArrests dataset, then compare the results.

## Initial Examination of the Data

# We begin by loading the USArrests dataset and viewing a summary of the data. 

data("USArrests")
summary(USArrests)

#R documentation tells us that the dataset contains data on the rate of arrests per 100,000 of the population for murder, assault, rape as well as the percentage of the population living in urban areas.

# Now we standardise the data and assign the scaled data to a new variable 'df':

df <- scale(USArrests)
summary(df)

# The 'head' function allows us to view the first 6 rows of data:  

head(df)

# The 'pairs' function allows us to quickly see the relationship, via scatterplots, between each pair of variables: 

pairs(df)

# Examining the scatterplots by eye, most of the variables seem to be positively  correlated. The strongest positive correlation appears to be between 'Assault' and  'Murder' or between 'Assault' and 'Rape'.  We can double-check using the function 'cor':

round(cor(df),2)

# As expected, the highest correlation coefficients are between 'Assault' and  'Murder' or between 'Assault' and 'Rape'. There is little correlation between the percentage of the population living in urban areas and the other variables.

##Non Hierarchical Clustering - K-means

#Determining the Optimal Number of Clusters

# The function fviz_nbclust calculates the 'total within sum of squares' against the number of clusters, k, so that we can see the optimal number of clusters to use:

fviz_nbclust(df, kmeans, method = "wss")

# The plot indicates that 4 clusters is optimal. 

# To create a similar plot manually, we calculate the total within sum of squares using from 2 to 8 clusters, then plot the total within sum of squares against the number of clusters:

km2 <- kmeans(df,2,iter.max = 10, nstart = 10)
km3 <- kmeans(df,2,iter.max = 10, nstart = 10)
km4 <- kmeans(df,4,iter.max = 10, nstart = 10)
km5 <- kmeans(df,5, iter.max = 10, nstart = 10) 
km6 <- kmeans(df,6, iter.max = 10, nstart = 10) 
km7 <- kmeans(df,7, iter.max = 10, nstart = 10) 
km8 <- kmeans(df,8, iter.max = 10, nstart = 10) 

plot(c(2,3,4,5,6,7,8),
     c(km2$tot.withinss, km3$tot.withinss, km4$tot.withinss, km5$tot.withinss, km6$tot.withinss, km7$tot.withinss, km8$tot.withinss),
     xlab = "No. of clusters",
     ylab = "Total within deviance",
     col = "blue")

lines(c(2,3,4,5,6,7,8),
      c(km2$tot.withinss, km3$tot.withinss, km4$tot.withinss, km5$tot.withinss, km6$tot.withinss, km7$tot.withinss, km8$tot.withinss),
      col="blue")

points(4,km4$tot.withinss,
       pch=20,
       col="red")

# We can clearly see the 'elbow' at 4 clusters. Adding more clusters does not significantly reduce the total within sum of squares, so we take 4 as the optimal number of clusters.

#Applying K-means Clustering With k = 4 

# We apply the 'kmeans' function to the scaled data with 4 clusters, using 10 iterations and 100 random starts to avoid local minima:

km.opt <- kmeans(df,4, iter.max = 10, nstart = 100)

# To view the cluster assigned to each observation:

km.opt$cluster

# To view the cluster centres:

round(km.opt$centers,2)

# Without more information, these values seem to indicate: 
# The cluster with four positive values seems to have very high assault arrests and rape arrests, and above-average murder arrests. The percentage of the population living in urban areas is more than average as well. 
# The cluster with the highest murder value has a lower than average percentage of the population living in urban areas and more assault arrests than average. 
# The cluster with four negative values seems the safest and is probably the most peaceful! 
# The next safest cluster seems to be where a fair amount of the population lives in urban areas compared to average, yet we see negative values for all types of arrests. 

# To view the within-cluster sum of squares:

km.opt$withinss

# To view the total within-cluster sum of squares (our objective function, which we aim to minimise):

km.opt$tot.withinss

# To view the size of each cluster:

km.opt$size

# To view the number of iterations:

km.opt$iter

# Now we can calculate the 50 x 4 matrix U, which defines which observation belongs to which cluster [X-observations = U X-centroids + E-errors]. We'll view only the first 8 rows for brevity:

U <- matrix(NA, 50, 4)
I <- diag(4)
U <- I[km.opt$cluster,]

U[1:8,]

# Returning to the original data values, we calculate the means of each cluster, so that we can better appreciate the real proportions of arrests in each category: 

round(aggregate(USArrests, by=list(cluster=km.opt$cluster),mean),1)

# The 'cbind' function allows us to create a vector relating the clusters to the original data (here we view only the first 6 rows for brevity). We can therefore compare the arrest rates, state by state, to the means of the cluster they belong to:

dd <- cbind(USArrests, cluster = km.opt$cluster)
head(dd)

#Visualising Clustering Results - Principal Component Analysis 

# Next, we plot the clusters using the 'fviz_cluster' function from the factoextra package in order to visualise the clusters against the first two principal components: 

library(factoextra)
fviz_cluster(km.opt,df) 

# We can see each cluster relative to the first and second principal components, along with the cluster centres and the percentage of variance explained by each component (total explained variance 86.7%). Using the principal components allows us to visualise the clusters in a 2D space and see how well they are separated, but makes interpretation of the clusters more difficult.

##Hierarchical Clustering - Ward Method

# We can also approach the problem using hierarchical clustering, then compare our solution to the solution obtained using K-means. 
# We begin by calculating the distance matrix using the 'dist' function. 

dist_mat <- dist(df, method = 'euclidean')

# The 'hclust' hierarchical clustering function is applied to the distance matrix and we can select the 'ward.D2' method which considers the distance squared (NOT "ward.D").

Hier_cl_w <- hclust(dist_mat, method = "ward.D2")
Hier_cl_w

# The 'plot' function allows us to visualise the dendrogram. The dendrogram can be cut at h = 5 to create 4 clusters. The 'rect.hclust' function allows us to draw a rectangle around every cluster.

plot(Hier_cl_w)
abline(h=6, col="green")
rect.hclust(Hier_cl_w, k = 4, border = "red")

# The 1st cluster contains 7 states (Al, Ls, etc), the 2nd 12 states (NY, etc), the 3rd 19 states (Iw, etc) and the 4th 12 states (Ut, Ws, etc). 

#Hierarchical Clustering Solution - Ward Method

# The 'cutree' function allows us to see, data point by data point, which cluster they belong to. Here we use the 'table' function to summarise the number of data points in each cluster found using the Ward method. 

fit_w <- cutree(Hier_cl_w, k = 4)
fit_w
table(fit_w)

##Comparison of Hierarchical and K-means Clustering Solutions

# We can then use the 'table' function to compare our Ward method solution to the K-means solution.

table(fit_w, km.opt$cluster)

# The Ward method cluster numbers are listed vertically and the K-means cluster numbers horizontally. This table shows that the hierarchical clustering and K-means solutions are very similar, but differ in the assignment of 3 states (see the row of fit_w with 16, 1, 1, 1). In the Ward clustering solution, these states are all assigned to a single cluster, while in the K-means method, 16 are assigned to one cluster but the remaining three states are assigned to each of the other three clusters.

##Conclusion

# Although similar, the solutions are different because the hierarchical clustering method is based on the distance between the data points, while the K-means method is based on the sum of squares. The hierarchical clustering method is more robust to outliers and noise, but the K-means method is faster and more scalable, although a poor initial choice of centroids can lead to longer convergence times. 
