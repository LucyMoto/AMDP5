# Install & load the package "devtools" + an external package from github: "ggbiplot"

install.packages("devtools")
library(devtools)

install.packages("remotes")
remotes::install_github("vqv/ggbiplot", force=TRUE)

library(ggbiplot)

# Scale, then do Principal Components Analysis of mtcars dataset 

mtcars.scaled <- scale(mtcars[,c(1:7,10,11)], center=TRUE, scale = TRUE)

mtcars.pca <- prcomp(mtcars.scaled)

# With a biplot, adding in labels so the individual cars can be seen, we see where each 
# car is placed with respect to the others:

ggbiplot(mtcars.pca)
ggbiplot(mtcars.pca, labels = rownames(mtcars))

# We add in a vector containing the 32 countries of origin, and assign this vector
# to the variable "mtcars.country":

mtcars.country <- c(rep("Japan", 3), 
                    rep("US",4), 
                    rep("Europe", 7),
                    rep("US",3), "Europe", 
                    rep("Japan", 3), 
                    rep("US",4), 
                    rep("Europe", 3), "US", 
                    rep("Europe", 3))

# Our biplot can include ellipses which encompass groups of vehicles, country by country:

ggbiplot(mtcars.pca, 
         ellipse=TRUE, 
         labels=rownames(mtcars), 
         groups=mtcars.country)

# Our biplot can be altered to consider the 3rd & 4th principal components, rather
# than the 1st & 2nd. Little is clarified in this diagram, in this particular case. 

ggbiplot(mtcars.pca,
         ellipse=TRUE, 
         choices=c(3,4), 
         labels=rownames(mtcars), 
         groups=mtcars.country)

# Returning to the 1st & 2nd PC, we can also add in a circle to see which cars are 
# outside the main circle, that is, which cars are unusual in comparison to the others.

ggbiplot(mtcars.pca,
         ellipse=TRUE,
         circle=TRUE, 
         labels=rownames(mtcars), 
         groups=mtcars.country)

# We can alter the scale to be more uniform:

ggbiplot(mtcars.pca,
         ellipse=TRUE,
         obs.scale = 1, var.scale = 1,  
         labels=rownames(mtcars), 
         groups=mtcars.country)

# Removing the axes, we are left with the points plotted:

ggbiplot(mtcars.pca,
         ellipse=TRUE,
         obs.scale = 1, var.scale = 1,
         var.axes=FALSE,
         labels=rownames(mtcars), 
         groups=mtcars.country)

# Adding a title, legend, colours, etc:

ggbiplot(mtcars.pca,
         ellipse=TRUE,
         obs.scale = 1, var.scale = 1,  
         labels=rownames(mtcars), 
         groups=mtcars.country) +
    scale_colour_manual(name="Origin", 
                        values= c("forest green", "red3", "dark blue"))+
    ggtitle("PCA of mtcars dataset")+
    theme_minimal()+
    theme(legend.position = "bottom")

# We can add in a new observation with data in columns 1:11:

spacecar <- c(1000,60,50,500,0,0.5,2.5,0,1,0,0)

# The new observation is added to the mtcars data and assigned to "mtcarsplus":

mtcarsplus <- rbind(mtcars, spacecar)

# We add in the new car's country of origin:

mtcars.countryplus <- c(mtcars.country, "Jupiter")

mtcarsplus.scaled <-scale(mtcarsplus[,c(1:7,10,11)], center=TRUE, scale=TRUE)

mtcarsplus.pca <- prcomp(mtcarsplus.scaled)

# Replotting the biplot, 'spacecar' is way off to the top right due to its unique 
# characteristics.

ggbiplot(mtcarsplus.pca, 
         obs.scale = 1, var.scale = 1, 
         ellipse = TRUE, circle = FALSE, 
         var.axes=TRUE, 
         labels=c(rownames(mtcars), "spacecar"), 
         groups=mtcars.countryplus)+
  scale_colour_manual(name="Origin", 
                      values= c("forest green", "red3", "violet", "dark blue"))+
  ggtitle("PCA of mtcars dataset, with extra sample added")+
  theme_minimal()+
  theme(legend.position = "bottom")

# NB This single value has altered the explained variance, the positions of the arrows,
# etc, essentially REDEFINING THE ENTIRE PLOT! 

# Rescaling, so that "spacecar" is scaled to lie at (0,0):  

s.sc <- scale(t(spacecar[c(1:7,10,11)]), center= mtcars.pca$center)
s.pred <- s.sc %*% mtcars.pca$rotation

mtcars.plusproj.pca <- mtcars.pca
mtcars.plusproj.pca$x <- rbind(mtcars.plusproj.pca$x, s.pred)

ggbiplot(mtcars.plusproj.pca, 
         obs.scale = 1, var.scale = 1, 
         ellipse = TRUE, circle = FALSE, 
         var.axes=TRUE, 
         labels=c(rownames(mtcars), "spacecar"), 
         groups=mtcars.countryplus)+
  scale_colour_manual(name="Origin", 
                      values= c("forest green", "red3", "violet", "dark blue"))+
  ggtitle("PCA of mtcars dataset, with extra sample projected")+
  theme_minimal()+
  theme(legend.position = "bottom")

