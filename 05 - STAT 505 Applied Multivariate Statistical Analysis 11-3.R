# Import dataset

dati_raw <- read.csv("places.csv", header=T)

# Remove 10th column (ID number) & assign the dataset (9 attributes) to the variable 'data'
# and set the ID number as the row name

dati <- dati_raw[,-10]
rownames(dati) <- dati_raw[,10]

# Standardise the data

dati_scaled <- scale(dati, center = T, scale = T)

# Check the scaled data; the mean should always be zero & the leading diagonal of the 
# correlation of the scaled data should be all 1s:

summary(dati_scaled)
cor(dati_scaled)

# Calculate PCA & assign it to the variable 'PCA':

PCA <- prcomp(dati_scaled)

# View the summary of the PCA:

summary(PCA)

# Using the Kaiser rule, the standard deviations of PC1, PC2 & PC3 > 1
# However, this takes us up to 64% of the total variance.

# The lambdas are given by the 'proportion of variance' row, and total 1

# To view the loadings of each component for each variable:

PCA$rotation

# Interpretation of the principal components is based on which variables are most 
# strongly correlated with each component. In other words, we need to decide which 
# numbers are large within each column. 

# We can see that, for example: 
# PC1 = 0.206 x Zclimate
#     + 0.357 x Zhousing  *
#     + 0.460 x Zhealth   ***
#     + 0.281 x Zcrime
#     + 0.351 x Ztrans    *
#     + 0.275 x Zeducate 
#     + 0.463 x Zarts     *** 
#     + 0.328 x Zrecreate *
#     + 0.135 x Zecon

# For PC1, Health & Arts are most important and tend to increase together
# & to some extent poor Housing (high is bad), good Transportation, and good Recreation facilities.

# PC2 = 0.218 x Zclimate
#     + 0.251 x Zhousing  
#     - 0.299 x Zhealth   
#     + 0.355 x Zcrime    *
#     - 0.179 x Ztrans    
#     - 0.483 x Zeducate  ***
#     - 0.195 x Zarts      
#     + 0.384 x Zrecreate *
#     + 0.471 x Zecon     ***

# PC2 measures good economy and poor education as the most important factors 
# & to some extent poor crime rates and good recreation facilities.

# PC3 = -0.690 x Zclimate  ***
#      - 0.208 x Zhousing  *  
#      - 0.007 x Zhealth   
#      + 0.185 x Zcrime
#      + 0.146 x Ztrans    
#      + 0.230 x Zeducate  *
#      - 0.026 x Zarts      
#      - 0.051 x Zrecreate 
#      + 0.607 x Zecon     ***

# PC3 measures poor climate & good economy as the most important factors 
# & to some extent good education and good housing (low is good)

# A biplot can help us visualise the relative importance of each factor:

library(factoextra)
fviz_pca_var(PCA)

# One method of deciding how many components to include is to choose only those that 
# give unambiguous results, i.e., where no variable appears in two different columns 
# as a significant contribution.  In this example, it is not really possible without 
# reducing the number of principal components and losing a significant amount of 
# information (variance).  

# You have to decide what is important in the context of the problem at hand. 

# PC1, PC2 and PC3 are independent of each other. 

# One of the problems with this analysis is that the analysis is not as 'clean' as one 
# would like with all of the numbers involved. For example, in looking at the second 
# and third components, the economy is considered to be significant for both of those 
# components. As you can see, this will lead to an ambiguous interpretation in our analysis.
