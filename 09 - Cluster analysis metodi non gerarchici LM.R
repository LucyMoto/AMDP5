# Install packages: cluster, factoextra and ClusterR
# Load libraries: cluster, factoextra and ClusterR

install.packages("cluster")
library(cluster)
install.packages("factoextra",type = "binary")
library(factoextra)
install.packages("ClusterR")
library(ClusterR)

# View the head of dataset iris:

head(iris)

# Assign all but the 5th column (species name) to the variable 'df':

df <- iris[,-5]

# Center and scale the data:

df <- scale(df, center = TRUE, scale = TRUE)

# Check the scaled data now has a mean of 0:

summary(df)

# Before we begin our analysis, looking at the iris dataset: 

table(iris[,5])

# We can see we have 50 of each type of species, so, in theory, our data should be 
# grouped into three distinct clusters.

# The kmeans function takes: data matrix, number of clusters/centres, the maximum no. of 
# iterations allowed and 'nstart', the number of random starts.
# We assign the output of the kmeans function to the variable "km3":

km3 <- kmeans(df, 3, iter.max = 10, nstart = 10)
km3

# Looking at how the three species are distributed with respect to the three clusters:

table (iris[,5],km3$cluster)

# ...we see that kmeans has been able to allocate all of the setosa species into cluster
# 2, but the versicolour and virginica species are distributed between clusters 1 & 3. 

# Let's increase the nstart value by a factor of 10 to see if the result improves:

km.opt <- kmeans(df,3,iter.max = 10, nstart = 100)

table(iris[,5],km.opt$cluster)

# Although the cluster numbers have changed, the result is equivalent to that already
# obtained with nstart set to 10. 

# To see the list of clusters to which each point is allocated:

km.opt$cluster

# To see positions of the centroids of the three clusters: 

km.opt$centers

# To see the within cluster sum of squares:

km.opt$withinss

# We could then perform sum(km.opt$withinss) or find the total directly as below, to 
# see the result of the minimisation |X - U Xbar|^2:

km.opt$tot.withinss

# To see the number of points in each cluster:

km.opt$size

# The clusters are similar in size, although "perfection" would be three clusters of 50.

# To construct the matrix "U", first we set up a null 150 x 3 matrix: 

U <- matrix(NA,150,3)

# Then we set up an identity diagonal 3 x 3 matrix "I":

I <- diag(3)

# Then we transform the cluster data here:

km.opt$cluster

# into a 150 x 3 matrix U (here we only view the first 8 rows):

U <- I[km.opt$cluster,]
U[1:8,1:3]

# Compare with the tabled totals:

table(iris[, 5],km.opt$cluster)

# We can now plot the scaled datapoints (1st and 2nd columns, sepal width against sepal
# length only):

plot(df[,1:2],
     col=km.opt$cluster,
     main= "K-means with 3 clusters")

# To view the positions of the centres (sepal width and sepal length only):

km.opt$centers[,1:2]

# Now we add on the centroids:
# The function points plots coordinates onto our plot
# cex controls the font size
# pch controls the character to use (4 = X)
points(km.opt$centers[,1:2],
        col= 1:3,
        pch = 4,
        cex = 2)

# We can make a cluster plot relative to the first two principal components, 
# using the function fviz_cluster, which takes arguments kmeans object, data: 

fviz_cluster(km.opt,df)

# This plot allows us to avoid having to pick two of our four variables (sepal length, 
# sepal width, petal length, petal width) for a 2D scatterplot, so that we can instead 
# see the clusters relative to the first two principal components and see how much of 
# the variability is accounted for. In this case, the division between the three different 
# clusters is clearer. 

# For comparision, we can try specifying 2 clusters:

km.opt2 <- kmeans(df, 2, iter.max = 10, nstart = 100)
fviz_cluster(km.opt2, df)
table(km.opt2$cluster, iris[, 5])

# We can see that the 2nd and 3rd clusters of the initial kmeans calculation have now
# been joined into one cluster. 

# To understand the optimal number of clusters to be used, we use the function fviz_nbclust,
# which takes the arguments data frame, partitioning function, method. "wss" refers 
# to the total within sum of squares : 

fviz_nbclust(df, kmeans, method = "wss")

# We can see that the optimal number of clusters is 3 (after this, little is to be gained
# by adding further clusters). See the 'elbow' at 3 clusters.

gap_stat <- clusGap(df,
                    FUN = kmeans,
                    nstart = 25,
                    K.max = 10,
                    B = 50)

# Now we can plot the gap statistic against the number of clusters. 

fviz_gap_stat(gap_stat) 

# As 3 clusters is the point at which the graph rises then falls, we again find the 
# optimal number of clusters is 3. 




