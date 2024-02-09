## Part 1
by_type <- function(x, sort = FALSE) {
  int_vec <- numeric(0)
  double_vec <- double(0)
  char_vec <- character(0)
  na_vec <- c()
  for (idx in x) {
    suppressWarnings(int_val <- as.integer(idx))
    suppressWarnings(double_val <- as.numeric(idx))
    suppressWarnings(char_val <- as.character(idx))
    # idx <- grepl(",", "", idx)
    # int_val <- as.integer(idx)
    # double_val <- as.numeric(idx)
    # char_val <- as.character(idx)
    # If valid integer, concatenate onto integer vector -- vice versa for other types
    if (is.na(idx)) {
      na_vec <- c(na_vec, idx)
    } else if (!is.na(int_val) && int_val == double_val && !is.na(char_val)) {
      int_vec <- c(int_vec, int_val)
    } else if (!is.na(double_val)) {
      double_vec <- c(double_vec, double_val)
    } else {
      char_vec <- c(char_vec, char_val)
    }
  }
  # If sort arg is TRUE, sort each list component
  if (sort) {
    int_vec <- sort(int_vec)
    double_vec <- sort(double_vec)
    char_vec <- sort(char_vec)
  }
  # Create output list with the three specified types
  out <- list(integers = int_vec, doubles = double_vec, character = char_vec)
  out
}
# x <- c("a", "1", "2.2", "house", "3.4", "6")
# by_type(x)
# by_type(x, sort = TRUE)

## Part 2
prime_factor <- function(x) {
  # Validate input x
  if (x < 2 || !is.numeric(x) || x != as.integer(x)) {
    stop("Invalid value!")
  } 
  # Initializers
  factors <- numeric(0)
  div <- 2
  # Check for prime w/o built-in is.prime() - starting with 2
  while (x > 1) {
    while (x %% div == 0) {
      # Add to vector of prime factors
      factors <- c(factors, div) 
      # Divide x by current factor
      x <- x / div
    }
  # Find next prime factor
  div <- div + 1
  }
  # Output resulting prime factors
  factors
}
# print(prime_factor(171))
# print(prime_factor(364))

## Part 3
# read.table("month_names.txt", sep = "\t", header = TRUE)
month_names <- read.delim("month_names.txt", encoding = "UTF-8", row.names = 1)
### --- NOTES --- ###
# head(month_names)
# names(month_names)
# month_names["English", ]
# rownames(month_names)
# colnames(month_names)
# month_idx <- which(month_names["English", ] == as.character("March"))
# month_idx
# month_names["English", month_idx]
### ------------ ###

month_convert <- function(x, from_lang, to_lang) {
  # Initializer
  translated <- character(length(x))
  # Validate input language
  if (!(from_lang %in% rownames(month_names)) || !(to_lang %in% rownames(month_names))) {
    if (!(from_lang %in% rownames(month_names))) {
      stop("Invalid language name to convert from!")
    } else {
      stop("Invalid language name to convert to!")
    }
    stop("Invalid languages!")
  }
  # Iterate through each element in x arg
  for (i in seq_along(x)) {
    # Find from_lang corresponding numeric month
    month_idx <- which(month_names[from_lang, ] == as.character(x[i]))
    if (length(month_idx) == 1) {
      # Get number and then find to_lang corresponding name to number
      translated[i] <- as.character(month_names[to_lang, month_idx])
    } else {
      translated[i] <- NA
    }
  }
  translated <- factor(translated)
  translated
}
# x <- factor(c("March", "March", "February", "June"))
# month_convert(x, "English", "Spanish")
# x <- factor(c("March", "March", "February", "June", "Jaly"))
# month_convert(x, "English", "Spanish")
# x <- factor(c("August", "April", "December", "April"))
# x
# month_convert(x, "English", "Italian")

