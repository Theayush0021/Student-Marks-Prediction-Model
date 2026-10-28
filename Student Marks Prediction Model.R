# 1. Create dataset
students <- data.frame(
  hours = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  marks = c(20, 25, 35, 40, 50, 55, 65, 70, 80, 90)
)

# 2. Split data into training and testing
set.seed(123)

train_index <- sample(
  seq_len(nrow(students)),
  size = floor(0.8 * nrow(students))
)

train_data <- students[train_index, ]
test_data <- students[-train_index, ]

# 3. Train model
model <- lm(marks ~ hours, data = train_data)

# 4. Predict test marks
predictions <- predict(model, newdata = test_data)

# 5. Evaluate model
mae <- mean(abs(test_data$marks - predictions))

print(test_data)
print(predictions)
print(mae)

# 6. Predict for a new student studying 7.5 hours
new_student <- data.frame(hours = 7.5)

predict(model, newdata = new_student)


# 7. Actual vs Predicted Marks Graph

comparison <- data.frame(
  Actual = test_data$marks,
  Predicted = as.numeric(predictions)
)

plot(
  comparison$Actual,
  comparison$Predicted,
  main = "Actual vs Predicted Marks",
  xlab = "Actual Marks",
  ylab = "Predicted Marks",
  pch = 19,
  col = "blue"
)

# Perfect prediction reference line
abline(a = 0, b = 1, col = "red", lwd = 2)

# 8. Display comparison table
print(comparison)

# 9. Predict marks for a new student
new_student <- data.frame(hours = 7.5)

new_prediction <- predict(model, newdata = new_student)

cat("Study Hours:", new_student$hours, "\n")
cat("Predicted Marks:", round(new_prediction, 2), "\n")
cat("Mean Absolute Error (MAE):", round(mae, 2), "\n")
