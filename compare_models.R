# compare_models
# Compare multiple linear models
# returns R-squared, Adj R-squared, and AIC
# initially a teaching aid for ANTHRBIO 463

# Compare multiple linear models
# @param ... Any number of lm objects
# @return    A matrix comparing R-squared, Adj R-squared, and AIC

compare_models <- function(...) {
  mods <- list(...)
  names(mods) <- as.character(match.call()[-1])
  
  # Extract metrics using a loop-free 'sapply'
  metrics <- sapply(mods, function(m) {
    s <- summary(m)
    c(R2 = s$r.squared, 
      AdjR2 = s$adj.r.squared, 
      AIC = AIC(m),
      Sigma = s$sigma)
  })
  
  return(round(t(metrics), 4))
}

# e.g.,
# m1 <- lm(mpg ~ wt, data = mtcars)
# m2 <- lm(mpg ~ wt + hp, data = mtcars)
# compare_models(m1, m2)