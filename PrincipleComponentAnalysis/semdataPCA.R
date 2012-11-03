## Set working directory
setwd('R/semdata/')

## Read in data from CSV
data1 <- read.csv('semdata/semdataFemale.csv', header=TRUE)

## Subset the data
data2 <- cbind(data1$Height,
		   data1$Age,
		   data1$Weight,
		   data1$Breast,
		   data1$Waist,
		   data1$Hips,
		   data1$Thigh,
		   data1$Calf,
		   data1$Body,
		   data1$Shoulder,
		   data1$Leg
		   )
#Rename columns 
colnames(data2) <- c('Height', 'Age', 'Weight', 'Breast', 'Waist', 'Hips', 'Thigh', 'Calf', 'BodyHeight', 'LegLength')

#Pairwise scatter plots
pairs(data2)

dev.print(device=postscript, "Female.eps", onefile=FALSE, horizontal=FALSE)

#Perform PCA
arc.pca1 <- princomp(data2, score=TRUE, cor=TRUE)
summary(arc.pca1)

#Print PCA
plot(arc.pca1)

#Print the biploy
biplot(arc.pca1)

#Print the loadings
arc.pca1$loadings

## Print the scores (projections)
arc.pca1$scores