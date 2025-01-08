# First we install the 'dplyr' package & load the 'dplyr' library: 

install.packages("dplyr")
library(dplyr)

# To view the first 5 rows of the 'mtcars' dataset:

head(mtcars)

# Using the 'dist' function, method Euclidean, we calculate the matrix of distances
# between all of the datapoints:

distance_mat <- dist(mtcars, method = 'euclidean')
distance_mat

# We can see Mazda RX4 Wag and Mazda RX4 are very close together.
# We can see Duster 360 and Datsun 710 are very far apart (most values seem 0-200, from
# those we can see at the start of the matrix). 

# Out of curiosity, let's see the spread of distances:

round(summary(distance_mat),1)

# The 'hclust' function takes arguments: distance matrix (a dissimilarity structure 
# as produced by dist), bond method: 	the agglomeration method to be used. This should
# be (an unambiguous abbreviation of) one of "ward.D", "ward.D2", "single", "complete", 
# "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).
# We begin with the average bond method:

Hierar_cl_a <- hclust(distance_mat, method = "average")
Hierar_cl_a

# We can then plot the cluster dendogram:

plot(Hierar_cl_a)

# Adding in a horizontal line at h = 110, we can see three distinct clusters and an outlier:

abline(h = 110, col = "green")

# The function 'cutree' cuts a tree, e.g., as resulting from hclust, into several groups 
# either by specifying the desired number(s) of groups or the cut height(s).
# Its arguments: tree, k (no. of groups), h (height where the tree should be cut).
# In this case we could have used h=160 rather than k=3.

fit_a <- cutree(Hierar_cl_a, k = 3)
fit_a

# We can see the cluster number for each row of data. NB Maserati Bora is on its own
# in cluster number 3.  It's easier to see the frequencies in each cluster using  'table':

table(fit_a)

# We can add 3 rectangles to the dendogram to outline the 3 clusters:

rect.hclust(Hierar_cl_a, k = 3, border = "red")

# Now we repeat the clustering with the single bond method (minimum distance between each
# new datapoint and the nearest datapoint already in a cluster):

Hierar_cl_s <- hclust(distance_mat, method = "single")
Hierar_cl_s

plot(Hierar_cl_s)
abline(h = 60, col = "green")

fit_s <- cutree(Hierar_cl_s, k = 3 )
fit_s
table(fit_s)

rect.hclust(Hierar_cl_s, k = 3, border = "red")

# NB Maserati Bora is on its own in cluster number 3 again.  This time, there is a 
# small cluster of 3 vehicles and a large cluster containing the remaining datapoints.

# Now we repeat the clustering with the bond method complete (maximum distance between 
# each new datapoint and the furthest datapoint already in a cluster):

Hierar_cl_c <- hclust(distance_mat, method = "complete")
Hierar_cl_c

plot(Hierar_cl_c)
abline(h = 110, col = "green")

fit_c <- cutree(Hierar_cl_c, k = 3 )
fit_c
table(fit_c)

rect.hclust(Hierar_cl_c, k = 3, border = "red")

# The results of 'complete' seem more similar to 'average' and the clusters have similar
# frequencies.

# Finally we repeat the clustering with the Ward bond method (minimising the total 
# variance from the centroid of the new group):

Hierar_cl_w <- hclust(distance_mat, method = "ward.D2")
Hierar_cl_w

plot(Hierar_cl_w)
abline(h = 110, col = "green")

fit_w <- cutree(Hierar_cl_w, k = 3 )
fit_w
table(fit_w)

rect.hclust(Hierar_cl_w, k = 3, border = "red")

# In this case the distances between the various clusters seem greater, making each
# cluster more distinct from the others.

# Comparison tables of two methods 
table(fit_a,fit_s)
table(fit_a,fit_c)
table(fit_w,fit_c)

# All methods are able to group the 16 datapoints in cluster 1. 

# Complete and Ward give identical solutions (as far as frequencies are concerned).

# Single forms one huge cluster, a small cluster and an outlier, whereas average 
# splits the large cluster into two more equal clusters.

# Complete forms a large 16 point cluster, then two similar sized clusters, whereas 
# average forms two similar clusters and one outlier. 
# Given the variety of solutions, we can conclude that the bond method is very important. 
# The 'appropriate' bond method will depend on the presence and position of outliers, 
# the type of solution required, etc. 