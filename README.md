# Classification-for-hospital
Logistic regression model for diabet
CLASIFICATION 
library(caTools) 

# Spliting the data into training and test sets
set.seed(123)  # Setting seed for reproducibility
split <- sample.split(a1$CLASS, SplitRatio = 0.75)
training_set <- subset(a1, split == TRUE)
test_set <- subset(a1, split == FALSE)

# Training the logistic regression model
mod <- glm(formula = CLASS ~ ., family = binomial, data = training_set)

# Predicting probabilities on the test set
prob_pred <- predict(mod, type = 'response', newdata = test_set)

# Converting probabilities to binary predictions
y_pred <- ifelse(prob_pred > 0.5, 1, 0)

# Create a confusion matrix
conf_matrix <- table(Actual = test_set$CLASS, Predicted = as.factor(y_pred))

# Displaying the confusion matrix
