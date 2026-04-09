# k_fold_cross_validation for linear models
# initially a teaching aid for ANTHRBIO 463

# K-Fold cross-validation for linear models
# @param data:     Data frame
# @param formula:  Model formula
# @param k:         Number of folds
# @return a vector of mean squared errors (MSE)

k_fold_cross_validation <- function(data, formula, k = 5) {
  # Shuffle the data
  data <- data[sample(nrow(data)), ]
  folds <- cut(seq(1, nrow(data)), breaks = k, labels = FALSE)
  
  mse_results <- numeric(k)
  
  for(i in 1:k) {
    test_idx <- which(folds == i)
    train_set <- data[-test_idx, ]
    test_set  <- data[test_idx, ]
    
    # Fit model on training, predict on testing
    fit <- lm(formula, data = train_set)
    preds <- predict(fit, newdata = test_set)
    
    # Calculate MSE
    mse_results[i] <- mean((test_set[[as.character(formula[[2]])]] - preds)^2)
  }
  
  return(list(fold_mse = mse_results, mean_mse = mean(mse_results)))
}

# Example: k_fold_cross_validation(mtcars, mpg ~ hp + wt, k = 5)