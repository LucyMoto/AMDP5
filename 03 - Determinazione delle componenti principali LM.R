# We begin by examining the structure of the dataset mtcars:

str(mtcars)

#  Columns 1-7: mpg, cyl, disp, hp, drat, wt, qsec
#  Columns 8-9: vs, am   - this is data of only zeros and ones - ignore
#  Columns 10-11: gear, carb

# We extract columns 1-7 & 10-11 and assign the resulting dataset to the variable "x":

x <- mtcars[,c(1:7,10:11)]

# Centre the data around the mean of 0, scale the data (divide by std dev), and
# assign the data to the variable "scale.x", now a 32x9 matrix.

scale.x <- scale(x, center = TRUE, scale=TRUE)

# Assign the correlation of "x" to the variable "corr.x"
# The matrix is symmetrical about the leading diagonal of 1s

corr.x <- cor(x) 

# Assign the correlation of "scale.x" to the variable "corr.sx"
# This actually gives an identical matrix to "corr.x"

corr.sx <- cor(scale.x) 

# Assign the principal components analysis of the scaled matrix to the variable "mod.pca"

mod.pca <- prcomp(scale.x) 

# We view the summary of the PCA to see the relative importance of each component

summary(mod.pca)

# Viewing 'rotation' from the PCA allows us to view the 9x9 matrix A of loadings:

mod.pca$rotation 

# Viewing 'x' from the PCA gives us the 32x9 matrix Y of components for each & every observation:

mod.pca$x 

# Now we view the correlations of the scaled x values with the PCA x values:

cor(scale.x,mod.pca$x) 

# Biplot the PCA of correlations with PC1 & PC2:

biplot(mod.pca,col=c("grey","blue"))

# Our lambdas = the proportion of variance (check by eye with the summary table), 
# which can be plotted on a barchart:

lambdas = mod.pca$sdev^2/sum(mod.pca$sdev^2)

barplot(lambdas, 
        xlab = "Componente",
        ylab = "Varianza Spiegata",
        main = "Quota di Varianza per Componente",
        col = "white",
        names = c("C1","C2","C3","C4","C5","C6","C7","C8","C9")
        )

# From our barchart, we can see that taking the first 2 components would be a good choice
# to minimise the number of components, without losing too much variance.

# Now, we define a new function, which finds the product of its two inputs: 
# var.loadings & comp.sdev:

var_cor_func <- function(var.loadings, comp.sdev){
                var.loadings*comp.sdev
                } 

# We can use this inside the 'apply' function to calculate the coordinates of the
# circle of correlations.

# apply(X, MARGIN, FUN, ..., simplify = TRUE)
# $rotation = the first array, MARGIN 1 = rows, FUN = function applied, $sdev = the second array  

var.coord <- var.cor <- t(apply(mod.pca$rotation, 1, var_cor_func, mod.pca$sdev)) 

# We plot the circle first by assigning a sequence of 100 numbers from 0 to 2pi to "a"
# then calculating x = cos(a), y = sin(a):

a <- seq(0, 2*pi, length = 100)
plot(cos(a), sin(a), 
     type = 'l', 
     col="gray",
     xlab = "PC1",  
     ylab = "PC2")

# We add in the horizontal and vertical axis lines at 0

abline(h = 0, v = 0, lty = 2)

# We add in arrows indicating the coordinates of the circle of correlations:

arrows(0, 0, var.coord[, 1], var.coord[, 2], 
       length = 0.1, angle = 15, code = 2)

# We also add in text so that each arrow is labelled:

text(var.coord, labels=rownames(var.coord), cex = .7, adj=1)

# Alternatively, we could have saved ourselves all this work with this command:

library(factoextra)

fviz_pca_var(mod.pca)

