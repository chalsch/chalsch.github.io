#############################################
#############   EIGENNONSENSE   #############
#############################################

#Author: Chris Halsch
#Date: 10/6/2023

#Here is the code I will use for the presentation.
#Feel free to shoot me any corrections or questions to cahalsch@gmail.com

library(dplyr)
library(ggplot2)

#Import data from the "iris" dataset that is built into R.
#I am only using one plant species to make this more simple.
dat <- iris %>% 
  filter(Species == "versicolor") %>% 
  select(-Species)

pairs(dat) #variables are correlated

#############################
##########   PCA   ##########
#############################

#This block will run a PCA on 4 correlation plant traits using the prcomp function

#This will scale each column in the data individually
dat <- data.frame(apply(dat, 2, scale))

#This runs a PCA on the data. Pretty easy
pc <- prcomp(dat)

#This calls ths eigenvectors (also called loadings)
pc$rotation

#This calls the eigenvalues. Note that you need to square the std_dev to get the 
#eigen values. prcomp gives the std_dev of each pc. std is the square root of variance 
pc$sdev^2

#Makes a basic screeplot
screeplot(pc)

#Plots the PCA scores
pca_dat <- data.frame(pc1 = pc$x[,1], pc2 = pc$x[,2])

ggplot(data = pca_dat) +
  geom_point(aes(pc1, pc2), pch = 21, fill = "darkblue", alpha = 0.5, size = 3) +
  theme_classic()


#######################################
##########   PCA (by hand)   ##########
#######################################

#This block will run a PCA on 4 correlated plant traits "by hand"
#(by using the eigen function)


#Create var/cov matrix of the data
mat <- var(dat)
mat

#Generate eigenvalues from matrix
e <- eigen(mat)

#View the eigenvectors (same as above)
e$vectors

#View the eigenvalues (same as above)
e$values

#Create a screeplot from scratch
scree_dat <- data.frame(type = c(rep("PC1", round(e$values*100)[1]), 
                                 rep("PC2", round(e$values*100)[2]),
                                 rep("PC3", round(e$values*100)[3]), 
                                 rep("PC4", round(e$values*100)[4])))

ggplot(data = scree_dat) +
  geom_bar(aes(x=type), color="black", fill = "gray80", linewidth = 0.5) +
  theme_classic()

#This next section will calculate the PCA scores. Here you multiple the original data
#by the eigenvectors using matrix multiplication

#This is how matrix multiplication works by hand... just dont
pc1 <- dat[,1] * e$vectors[1,1] + dat[,2] * e$vectors[2,1] + dat[,3] * e$vectors[3,1] + dat[,4] * e$vectors[4,1]
pc2 <- dat[,1] * e$vectors[1,2] + dat[,2] * e$vectors[2,2] + dat[,3] * e$vectors[3,2] + dat[,4] * e$vectors[4,2]
pc3 <- dat[,1] * e$vectors[1,3] + dat[,2] * e$vectors[2,3] + dat[,3] * e$vectors[3,3] + dat[,4] * e$vectors[4,3]
pc4 <- dat[,1] * e$vectors[1,4] + dat[,2] * e$vectors[2,4] + dat[,3] * e$vectors[3,4] + dat[,4] * e$vectors[4,4]

out <- data.frame(X1= pc1, 
                  X2 = pc2,
                  X3 = pc3,
                  X4 = pc4)

#or do it the easy way using matrix multiplication
out1 <- data.frame(as.matrix(dat) %*% as.matrix(e$vectors))

#Note that the plot may be flipped but the relative positions of points remains the same
ggplot(data = out1) +
  geom_point(aes(X1, X2), pch = 21, fill = "darkblue", alpha = 0.5, size = 3) +
  theme_classic()







