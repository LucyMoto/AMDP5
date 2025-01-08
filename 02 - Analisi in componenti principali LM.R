# Examine the STRucture of the 'iris' dataset

str(iris)

# There are 5 variables, of which the fifth is species (factor with 3 levels). 
# We eliminate this column from the dataset and assign the data to the variable "x":

x <- iris[,-5]

# Centre the data around the mean and assign the scaled data
# (divided by the standard deviation) to the variable "scaled.x"

scaled.x <- scale(x, center=TRUE, scale=TRUE)

# Assign the correlation of "x" to the variable "corr.x". The values in the matrix 
# are symmetrical about the leading diagonal of '1's.

corr.x <- cor(x)

# Assign the correlation of "scaled.x" to the variable "corr.sx". 

corr.sx <- cor(scaled.x)

corr.x
corr.sx

# "corr.sx" is identical to "corr.x" as the transformation from "x" to "scaled.x" 
# is linear and therefore does not affect the correlation. 

# Assign the principal components analysis of the scaled matrix to variable "mod.pca"

mod.pca <- prcomp(scaled.x)

# "mod.pca" now gives us a list of 5 items: sdev, rotation, center, scale & x

#   sdev:      standard deviations of the 4 principal components
#   rotation:  the matrix of variable loadings
#   center:    the centering used, 0 (or very near zero)
#   scale:     the scaling used, that is, the importance of each variable
#   x:         the value of the rotated data

# View mod.pca to see rotation n x k matrix = matrix A of loadings 
# that is, how each variable contributes to each component 

mod.pca

# 'scale' tells us the importance of each variable

mod.pca$scale

# 'x' gives the matrix Y of components for every observation 

mod.pca$x

# View summary gives the relative importance of each component

summary(mod.pca)

# See the standard deviation values for the 4 components:
# In this case only PC1 has standard deviation > 1 and is more important than the 
# standardised variables.
# The proportion of variance and cumulative variances are also given. 

# Now view the correlations with each component:

cor(scaled.x,mod.pca$x) 

# Note the positive and negative correlations, particularly the highest values

# Now we plot the biplot of correlations with PC1 and PC2:

biplot(mod.pca,col=c("red","blue"))

# The blue lines give the relative importance of each variable
# The red numbers are each and every observation 

lambdas = mod.pca$sdev^2/sum(mod.pca$sdev^2)
lambdas
# These are the proportion of variances that we saw in summary(mod.pca)

barplot(lambdas, xlab="Componente", col="blue", names=c("C1","c2","C3","C4"),
        ylab="Varianza Spiegata", main="Quota di Varianza per Componente")

