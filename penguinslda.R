#rm(list = ls())

# Load necessary library
library(MASS)  # Contains LDA function
library(ggplot2)


# Step 1: Use the famous iris dataset, 
#which has 3 classes of flowers and 
#4 features (sepal length, sepal width, petal length, and petal width).
data1 <- read.csv("C:/Users/Dilshan/Desktop/Portfolio Projects/classification/penguins.csv", header = TRUE)
data1_clean <- na.omit(data1)



# Step 2: LDA is performed using the lda() function from the MASS package. 
#The formula Species ~ . tells R to use the Species column as the target and all other columns as
lda_model <- lda(species ~ ., data = data1_clean)


#Step 3: Prints LDA results, which include the coefficients of linear 
#discriminants and the proportion of variance explained by each discriminant.
print(lda_model)


#Step 4: Use the LDA model to predict the classes of the data points. 
#The predict() function returns the linear discriminants and predicted class labels.
lda_predictions <- predict(lda_model)


# Step 5: Visualize the results
lda_data <- data.frame(lda_predictions$x, Species = data1_clean$species)


# 2D Scatter plot of the first two linear discriminants
ggplot(lda_data, aes(LD1, LD2, color = Species)) +
  geom_point(size = 2) +
  ggtitle("LDA: Linear Discriminants Plot") +
  theme_minimal()

