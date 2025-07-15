#rm(list = ls())
# Load necessary libraries (for visualization)
#install.packages("ggplot2")
library(ggplot2)


#Load a dataset
#Step 1: The dataset used is the iris dataset, which contains 150 observations of iris flowers, 
#with 4 numeric features (sepal length, sepal width, petal length, petal width) and a species label.
# Using the built-in Iris dataset for this example
data1 <- read.csv("C:/Users/Dilshan/Desktop/Portfolio Projects/classification/penguins.csv", header = TRUE)
data1_clean <- na.omit(data1)
X <- data1_clean[, 3:6]  # Extract only the numeric columns (features)


#Standardize the data
#Step 2: We standardize the data using the scale() 
#function to ensure that each feature has mean 0 and standard deviation 1.
X_scaled <- scale(X)


#Perform PCA
#Step 3: We perform PCA using the prcomp() function with the arguments center = TRUE and scale.
#= TRUE (these options are for centering and scaling, but we already
pca_result <- prcomp(X_scaled, center = TRUE, scale. = TRUE)


# View the summary of PCA results
# Step 4: The summary(pca_result) shows the proportion of variance explained by each principal component.
summary(pca_result)


#Eigenvalues (variances explained by each principal component)
# Step 5: We compute the eigenvalues and explained variance manually to show 
#how much variance each principal component captures.
eigenvalues <- pca_result$sdev^2
explained_variance <- eigenvalues / sum(eigenvalues)


# Print explained variance
print("Explained Variance by each Principal Component:")
print(explained_variance)


#Biplot of the first two principal components
#Step 6: The biplot() function visualizes the first two 
#principal components along with the loadings (contributions of each feature to the components).
biplot(pca_result, scale = 0)


#Scree plot to visualize the explained variance
#Step 7: A scree plot is drawn to visualize the proportion of variance explained by each principal component.
plot(explained_variance, type = "b", xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     main = "Scree Plot")


#Project data onto the first two principal components
#Step 8: We project the original data into the 2D space spanned by the first two principal components.
X_reduced <- pca_result$x[, c(2, 3)]


#Visualize the projection (2D plot)
#Step 9: A 2D scatter plot is generated using ggplot2, with points colored by species.


# Add species labels for coloring
X_reduced_df <- data.frame(X_reduced, Species = data1_clean$species)
ggplot(X_reduced_df, aes(PC2, PC3, color = Species)) + 
  geom_point(size = 2) + 
  ggtitle("PCA: Second and Third Principal Components") + 
  theme_minimal()

