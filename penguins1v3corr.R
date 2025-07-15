#rm(list = ls())

# Load necessary library
library(ggplot2)


# Step 1: Set seed for reproducibility
set.seed(123)


# Step 2: Generate 1000 samples
n <- 1000


# Step 3: Create dimensions with correlations between the first 3
dimension_1 <- rnorm(n, mean = 50, sd = 10)  # Random normal data
dimension_2 <- dimension_1 * 0.9 + rnorm(n, mean = 0, sd = 5)  # Highly correlated with dimension_1
dimension_3 <- dimension_1 * 0.7 + rnorm(n, mean = 0, sd = 7)  # Moderately correlated with dimension_1


# Step 4: Generate other dimensions with no strong correlations
dimension_4 <- rnorm(n, mean = 30, sd = 8)
dimension_5 <- rnorm(n, mean = 70, sd = 12)
dimension_6 <- rnorm(n, mean = 20, sd = 4)
dimension_7 <- rnorm(n, mean = 15, sd = 6)
dimension_8 <- rnorm(n, mean = 40, sd = 9)
dimension_9 <- rnorm(n, mean = 100, sd = 15)
dimension_10 <- rnorm(n, mean = 25, sd = 5)


# Step 5: Combine the dimensions into a data frame
dataset <- data.frame(dimension_1, dimension_2, dimension_3, dimension_4, dimension_5, 
                      dimension_6, dimension_7, dimension_8, dimension_9, dimension_10)


# Step 6: Compute the correlation matrix to verify correlations
correlation_matrix <- cor(dataset)
print("Correlation Matrix:")
print(correlation_matrix)


# Step 7: Standardize the data for PCA
dataset_scaled <- scale(dataset)


# Step 8: Perform PCA
pca_result <- prcomp(dataset_scaled, center = TRUE, scale. = TRUE)


# Step 9: View the PCA summary
summary(pca_result)


# Step 10: Scree plot to visualize explained variance
eigenvalues <- pca_result$sdev^2
explained_variance <- eigenvalues / sum(eigenvalues)
plot(explained_variance, type = "b", xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     main = "Scree Plot")


# Step 11: Project data onto the first two principal components
X_reduced <- pca_result$x[, c(1, 3)]


# Step 12: Visualize the projection using a scatter plot
X_reduced_df <- data.frame(X_reduced)
ggplot(X_reduced_df, aes(PC1, PC3)) +
  geom_point(size = 2, color = "blue") +
  ggtitle("PCA: Projection onto the First and Third Principal Components") +
  theme_minimal()
