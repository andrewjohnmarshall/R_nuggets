# clean_column_names
# simple function to make syntactically valid column names

clean_column_names <- function(x) {
  stopifnot(is.character(x))
  x <- tolower(x)
  x <- gsub("\\s+", "_", x)
  x <- gsub("[^a-z0-9_]", "", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}
