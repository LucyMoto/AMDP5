# Load the data from a .csv file, with the variable names defined in the first row (header):

dati_raw <- read.csv("dati.csv", header = T)

# Remove the 1st of 9 columns, X: name of athlete & country, as this data is character-based
# The reduced data set (25 observations of 8 variables) is assigned to the variable 'dati'

dati <- dati_raw[,-1]

# The column X is used instead as labels for each row:

rownames(dati) <- dati_raw[,1]

# The variable in the 8th column 'score' is a pre-calculated measure synthesising the data, so we 
# separate it into a vector with name 'last.var' 

last.var = dati[,8] 

# We remove that 8th column 'score' from 'dati' so it does not unfairly influence our PCA

dati = dati[,-8]

# So that all variables are defined as 'bigger is better', we take those measured with time and 
# reverse the sign.
dati$hurdles = -dati$hurdles
dati$run200m = -dati$run200m
dati$run800m = -dati$run800m

# "Summary" allows us to examine: min, Q1, median, mean, Q3, max for each of the 7 sports:

summary(dati)

# As each variable is on its own scale (e.g. javelin 35-48, high jump 1.5-1.9) we need 
# to rescale each and every variable around a mean of 0 and variance 1:

dati.scaled = scale(dati, center = T, scale = T)

# Examining the correlation of the data:
cor(dati)

# and the correlation of the scaled data:
cor(dati.scaled)

# we see that the correlation values are the same in both cases

# Now we calculate the PCA and assign it to the variable 'mod.pca':

mod.pca = prcomp(dati.scaled)

# To view the contents of 'mod.pca':

names(mod.pca)

# To view the relative importance of each component (sdev, prop of variance & cuml prop):

summary(mod.pca)

# To view the loadings of each component for each variable (7 sport):

mod.pca$rotation

# We see that PC1 is negative for every sport. This means the more able the athlete, 
# the lower PC1, hence PC1 is measuring 'non-ability'.

# To view the score of each component for each observation (25 obs):

mod.pca$x

# First athlete low score (negative) PC1 => good athlete
# Last athlete high score (positive) PC1 => poor athlete
# However, we can't really interpret these values beyond putting them in order with
# respect to each other.

# To view only the standard deviations of each component. 

mod.pca$sdev

# Only PC1 & PC2 > 1, so we should only consider PC1 & PC2 (Kaiser rule), unless other
# priorities are specified.

# We can look at the correlations of each of PC1-PC7 with each sport:

cor(mod.pca$x, dati.scaled)

# We can see that PC1 is heavily influenced by hurdles & longjump.
# We can see that PC2 is heavily influenced by the javelin.

# We should check that the sum of the lambdas is 1:

lambdas = mod.pca$sdev^2/sum(mod.pca$sdev^2)
sum(lambdas)

# It does!

# Now we can do a barplot of our lambdas (scale from 0 to 0.65) and the standard deviations 
# (scale from 0 to 2.15) - they are almost the same but with different scales. 

barplot(lambdas,
        main = "Quota di Varianza per Componente",
        xlab = "Componente",
        ylab = "Varianza Spiegata",
        col = "white",
        names = c("C1","C2","C3","C4","C5","C6","C7"),
        )

barplot(mod.pca$sdev, 
        main="Deviazione standard per componente",
        xlab="Componente",
        ylab="Deviazione standard",
        col="grey",
        names=c("C1","C2","C3","C4","C5","C6","C7"),
        )

# A biplot can help us visualise the relative importance of each component:

biplot(mod.pca,col=c("grey","blue"))

# We can use the factoextra library visualization function:

library(factoextra)
fviz_pca_var(mod.pca)

# Finally, let's examine the correlation between the dataset's own 'summary' of performance, 
# "score", and our PC1-PC7:

cor(mod.pca$x,last.var)

# We can see that PC1 and PC2 are most important, as expected