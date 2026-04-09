# run_stratified_models
# run multiple linear models across categories of a dataset simultaneously
# and returns a clean, "tidy" dataframe of results

#  Run stratified linear models
# @param data:	     a dataframe
# @param group_var:  the column to split the data by (quoted)
# @param formula:	 a formula object for the regression
# @return A nested tibble with model coefficients and statistics

run_stratified_models <- function(data, group_var, formula) {
  require(dplyr)
  require(purrr)
  require(broom)
  require(tidyr)

  data %>%
    group_by(across(all_of(group_var))) %>%
    nest() %>%
    mutate(
      model = map(data, ~lm(formula, data = .x)),
      tidied = map(model, tidy),
      glanced = map(model, glance)
    ) %>%
    unnest(tidied) %>%
    select(-data, -model)
}

# e.g.,
# run_stratified_models(mtcars, "cyl", mpg ~ wt + hp)