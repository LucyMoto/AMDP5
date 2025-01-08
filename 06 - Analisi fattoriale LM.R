#Examine the raw data

read.csv("food-texture.csv", header = TRUE)

# As the variable 'X' has no missing items and no duplicates, we set 'X' = row names.
# We assign the 'reduced' dataset (now purely numerical) to the variable 'food'

food <- read.csv("food-texture.csv", row.names = "X")

#Examine the structure of the dataset:

str(food)

# factanal(data matrix, factors, score, rotation) gives maximum-likelihood factor analysis 
# Scores: none (default), "regression" or "Bartlett" 
# Rotation: "varimax" (default), none or "Bartlett"

food.fa <- factanal(food, factors = 2) 

# We can view the factorial analysis results:

food.fa

# To see the 'specificità' or uniqueness values, these are the values on the diagonal 
# of the Psi matrix, that is, the part that is not explained by the factorial analysis: 

food.fa$uniquenesses  

# 'Crispy' has a very low value, 'Hardness' has a much higher value

# The commonality (= loadings ^ 2)

cumunalità <- apply(food.fa$loadings^2,1,sum)

# Uniqueness = 1 - commonality

1-cumunalità 

# Residual Matrix A
Lambda <- food.fa$loadings
Lambda

Psi <- diag(food.fa$uniquenesses)
Psi

# Sigma is the model's calculation of 'S'

Sigma <- Lambda %*% t(Lambda) + Psi
Sigma

S <- food.fa$correlation 
S

# Now we can check the differences between S and Sigma

round(S - Sigma, 3)

# Comparison of the three types of rotation:

food.fa.none <- factanal(food, factors = 2, rotation = "none")

food.fa.varimax <- factanal(food, factors = 2, rotation = "varimax")

food.fa.promax <- factanal(food, factors = 2, rotation = "promax")

# The par command sets graphical parameters. par(mfrow) is a setup for plotting 
# multiple figures(in a rectangular layout) on one page.

par(mfrow = c(1,3))

# Now we plot the first 2 factors with no rotation:

plot(food.fa.none$loadings[,1], 
     food.fa.none$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "No rotation")

# Add in the horizontal and vertical lines at 0:
abline(h = 0, v = 0)

# Next, we plot the first 2 factors with 'varimax' rotation:

plot(food.fa.varimax$loadings[,1], 
     food.fa.varimax$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2", 
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Varimax rotation")

# Add in the horizontal and vertical lines at 0:

abline(h = 0, v = 0)

# Add in labels for each of the loadings: 

text(food.fa.varimax$loadings[,1]-0.08, 
     food.fa.varimax$loadings[,2]+0.08,
     colnames(food),
     col="blue")

# Finally, we plot the first 2 factors with 'promax' rotation:

plot(food.fa.promax$loadings[,1], 
     food.fa.promax$loadings[,2],
     xlab = "Factor 1", 
     ylab = "Factor 2",
     ylim = c(-1,1),
     xlim = c(-1,1),
     main = "Promax rotation")

# Again we add in the horizontal and vertical lines at 0:

abline(h = 0, v = 0)

# From the plots, we can see that the solutions are fairly similar to each other. 
# The values obtained in psi for each will be a little different from each other as 
# the factor analysis does not change with rotations.  

