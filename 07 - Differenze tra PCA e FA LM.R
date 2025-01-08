install.packages("devtools")
library(devtools)
install.packages("vqv/ggbiplot")
install.packages("psych")
library(psych)
install.packages("GPArotation")
library("GPArotation")

#We will compare PCA and FA analyses on the food dataset.

#Let's look at the structure of the data:

food <- read.csv("food-texture.csv", row.names = "X")
str(food)

# Looking at the correlations between variables can give us a feel for what is happening.

round(cor(food),2)

# We can see a strong negative correlation between 'crispy' and 'fracture' (-0.84)
# We can see a strong negative correlation between 'oil' and 'density' (-0.75)
# We can see a weaker positive correlation between 'oil' and 'crispy', and between 
# 'fracture' and 'density'.

# We use the function fa.parallel this time to complete the factor analysis; this
# time the function works out the best number of factors to use (rather than us needing
# to specify a number)

parallel <- fa.parallel(food, fm="minres", fa='fa')
parallel

# The function has found 2 factors to be optimal:

parallel$nfact

# We now calculate the factor analysis, but using rotation 'oblimin' which can lead to some 
# correlation between factors as they are no longer orthagonal

food.fa <- fa(food, nfactors=2, rotate='oblimin', fm='minres')
print(food.fa)

# There is a correlation of -0.26 between the factors

food.scaled <- scale(food, center = TRUE, scale = TRUE)
food.pca = prcomp(food.scaled)

round(food.pca$rotation, 2)

# The first component includes all variables except 'hardness', the second is mostly
# influenced by 'hardness'

par(mfrow = c(1,2))

plot(food.fa$scores[,1],food.fa$scores[,2])
plot(food.pca$x[,1],food.pca$x[,2])

# The plots of all 50 observations against the first and second factors using factor
# analysis and then principal component analysis distinctly show two separate solutions.

# We can also plot the variables against both factors:  

par(mfrow = c(1,2))

plot(food.fa$loadings[,1], 
     food.fa$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Oblimin rotation")
abline(v=0,h=0)
text(food.fa$loadings[,1]-0.1,
     food.fa$loadings[,2]+0.1,
     colnames(food),
     col="blue")

plot(food.pca$rotation[,1], 
     food.pca$rotation[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "PCA")
abline(v=0,h=0)
text(food.fa$loadings[,1]-0.1,
     food.fa$loadings[,2]+0.1,
     colnames(food),
     col="blue")

# We can see the loadings of each method in detail: 

round(food.fa$loadings[,1],2)

round(food.pca$rotation[,1],2)

# We can see the two different solutions more clearly; this is to be expected as we are 
# trying to satisfy two different objectives. 

