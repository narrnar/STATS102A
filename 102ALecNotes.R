# Week 1

## Wednesday Jan 10th, 2024 ##
# Cmd + Shift + f10 --> restart r session
# cmd + shift + k --> knit current file
# cmd + l --> clear the console
# --- Basic Data Structures --- #
# a list(or generic vector) is an ordered collection of objects
# Dimension     Homogenous    Heterogenous
# 1 - dim       Atomic Vector List(generic vector)
# 2 - dim       Matrix        Data Frame
# n - dim       Array
## --- Attributes --- ##
head(trees)
attributes(trees)
attr(trees, "info") <- "This data frame is about trees!!"
attributes(trees)

## --- Matrices --- ##
M <- 1:10
M
class(M)
attr(M, "dim") <- c(2, 5) # I set dimension attributes
M
attributes(M) # there's only one attribute: dim
class(M) # class is smart eough to figure out that it's a matrix
attr(M, "dim")
M
class(M)
## --- Arrays --- ##
A <- 1:12
attr(A, "dim") <- c(2, 3, 2)
A

## --- Data Frame --- ##
head(trees, 4)
class(trees)
typeof(trees)

## --- Factors --- ##
gender <- c("M", "F", "F", "X", "M", "F")
gender_fac <- factor(gender)
gender_fac
levels(gender_fac)
typeof(gender_fac)
gender_fac
as.integer(gender_fac)
attributes(gender_fac)
x <- c(0, 1, 10, 5)
x_fac <- factor(x)
x_fac
mean(x_fac) # we try to take the mean but it doesn't work
# so we coerce to numeric, but the result doesnt make sense
mean(as.numeric(x_fac)) # the mean of 0, 1, 10, 5 should be 4
as.numeric(x_fac) # internally, they are stored as integers
x_fac
mean(as.numeric(as.character(x_fac))) # this works
gender_fac[2] <- "male"
gender_fac

## --- Coercion --- ##
l <- c(TRUE, FALSE)
i <- 1L
d <- c(5, 6, 7)
ch <- c("a", "b")
# Atomic vectors in R can only contain one data type --> coerced into a single type
typeof(c(l, i, d))
typeof(c(l, d, ch))
c(l, i, d)
c(l, i, ch)

## --- Implicit Conversion --- ##
trials <- c(FALSE, FALSE, TRUE)
as.numeric(trials)
sum(trials)
mean(trials)

## --- Explicit Coercion --- ##
as.character(trials)
as.logical(c(0, 1))
as.numeric("dog")
# anything numeric that is not 0 becomes TRUE except NaN becomes NA
as.logical(c(0, 1, -1, 0.1, 2, -Inf, 2.2e-308, NaN))
# accepted spellings of logical values
as.logical(c("F", "FALSE", "False", "false", "T", "TRUE", "True", "true"))
as.logical(c("f","t", "cat", 0, 1)) # other characters are not coerced

## --- Special Values --- ##
# NA is used to represent missing or unknown values. There are NA for each type
# NULL is used to represent an empty or nonexistent value. NULL s its own type
# NaN is type double and is used to represent inderterinate forms in mathematics (such as 0/0 or -Inf + Inf)
NA == NA # bad
is.na(NA) # good
typeof(NULL)
is.null(NULL)
is.na(NULL)
is.logical(NULL)
c(4, 5, NULL, 3)
NULL == NULL # bad

## --- Vector Arithmetic --- ##
x <- c(1, 2, 3)
y <- c(100, 200, 300)
x + y
x * y

## --- Vector Recycling --- ##
c(1, 2, 3) + c(100, 200, 300, 400, 500, 600)
c(1, 2, 3) + c(100, 200, 300, 400, 500)
M <- rbind(c(1, 2, 3,),
           c(4, 5, 6),
           c(7, 8, 9),
           c(10, 11, 12))
attr(M, "dim") <- c(3, 4) # I set dimension attributes
print(M)


## Friday Jan 12th, 2024 ##
# --- Subsetting: CH 4 in Textbook --- #
## --- Subsetting with Positive Integers --- ##
x <- c(2.1, 4.2, 3.3, 5.4)
x[c(3, 1)]
x[c(1, 1)]
x[c(2.1, 2.9)] # Real numbers are silently truncated to integers
## --- Subsetting with Negative Integers --- ##
x[-c(3, 1)]
x[c(-1, 2)] # Doesn't work
## --- Subsetting with Logical Vectors --- ##
x[c(T, T, F, F)]
x[x>3]
x[c(TRUE, NA)] # Recycle
## --- Special Case --- ##
x[] # returns original vector
x[0] # zero-length vector
## --- Subsetting with Character Vectors --- ##
y <- setNames(x, letters[1:4])
y[c("d", "c", "a")]
z <- c(abc = 1, def = 2)
z[c("a", "d")] # When subsetting with names, must be exact match
## --- Useful application: Lookup tables (chracter subsetting) --- ##
x <- c("m", "f", "u", "f", "f", "m", "m")
lookup <- c(m = "Male", f = "Female", u = NA)
lookup[x]
## --- Subsetting operators --- ##
x <- list(1:3, "a", 4:6)
x
x[1]
x[[1]]
## --- Double Square Brackets --- ##
x <- list(a = 1:3, b = "a", c = 4:6)
x
x[[1]]
x[["a"]]
x[c("a", "b", "c")]
x[[c("a", "b")]] # Does not work bc subscirpt out of bounds
## --- Sidenote: Recursive Subsetting(Not recommended) --- ##
x <- list(a = list(a = "a", b = "b"), b = 1:3)
x
x[["a"]][["b"]]
x[[c("a", "b")]]
d <- list(
  a = c(1, 2, 3),
  b = c(TRUE, TRUE, FALSE),
  c = c("a")
)
str(d)
d[[1]]
l1 <- list(
  1:8, 
  letters[1:4],
  5:1
)
str(l1)
l1[1]
l1[[1]]
l1[[1]][2] # Second element in first component
l1[[c(1, 2)]] # Recursive subsetting equal to the above
## ---- A list inside a list (A train inside a train car) --- ##
l2 <- list(l1, c(10, 20, 30), LETTERS[4:9])
str(l2)
l2[[1]] # gets l1
str(l2[[1]])
l2[[1]][1]
l2[[1]][[1]]
l2[[1]][[1]][2]
## --- Double square brackets on atomic vectors --- ##
x <- c("a" = 1, "b" = 2, "c" = 3)
x[1] # preserve name
x[[1]] # drops name
x <- 1:3
x[5]
x[[5]]
x[[NULL]]
## --- Single vs Double brackets for OOB and NULL indices --- ##
l1
l1[4]
l1[[4]] # common error in for loops
l1[NULL] # NULL or 0 returns no train cars
l1[[NULL]]

# --- Section 3 --- #
## --- Subsetting Matrices and arrays --- ##
# subset higher dimensional structures
#   - multiple vectors (most common method)
#   - single vector
#   - with a matrix (least common)
a <- matrix(1:9, nrow = 3)
colnames(a) <- c("A", "B", "C")
a
a[1:2, ] # first and second rows
a[0, -2] # no rows, all but the second column
## --- Subsetting a matrix simplifies by default -- ##
is.vector(a) # matrix is not a vector
a[1,] # no longer a matrix
is.vector(a[1, ]) # subset row is a vector
## --- Matrices are atomic vectors --- ##
vals <- matrix(LETTERS[1:25], nrow = 5)
vals[c(8, 9)]
select <- matrix(ncol = 2, byrow = TRUE, c(
  1, 5, ## select the values at the coordinates (1, 5)
  3, 1, ## value at coord (3, 1), thrid row, 1st col
  2, 3,
  1, 1
))
vals[select]
## --- Subset a 3d array with a matrix with 3 columns --- ##
ar <- array(1:12, c(2, 3, 2))
ar
select_ar <- matrix(ncol = 3, byrow = TRUE, c(
  1, 2, 1,
  2, 3, 2
))
ar[select_ar]

# --- Subsetting Data Frames --- #
## --- Data Frames --- ##
df <- data.frame(x = 1:4, y = 4:1, z = letters[1:4])
df
df[df$y %% 2 == 0, ] # choose the rows where this logical statement is true
df[c("x", "z")] # like a list
df[, c("x", "z")] # like a matrix
df[c(1, 3), ] # for rows
df[c(1, 3)]
## --- Data Frames with named columns --- ##
str(df["x"]) # preserves: remains a data frame
str(df[, "x"]) # simplifies: becomes a vector
str(df$x) # dollar sign always simplifies: becomes a vector
str(df[["x"]]) # simplifies: becomes a vector
## --- Simplifying vs. preserving --- ##
x <- c(a = 1, b = 2)
x[1] # preserve
x[[1]] # simplify
y <- list(a = 1, b = 2)
str(y[1]) # preserving
str(y[[1]]) # simplifying
## --- Simplifying vs. preserving: Factors --- ##
z <- factor(c("a", "b"))
z[1] # preserve
z[1, drop = TRUE] #simplify
## --- Examples of subsetting --- ##
head(mtcars, 10)
mtcars[mtcars$cyl <= 5]
mtcars[mtcars$cyl <= 5, ]
mtcars[mtcars$cyl == 4 | 6, ] # wrong output bc | 6 coerces any num not 0 into TRUE in logical vector
mtcars[mtcars$cyl == 4 | mtcars$cyl == 6, ]

### --- Week 2 --- ##
# --- Section 1: Operators that produce logical values --- #
# element-wise comparison
x | y
x & y
!x
xor(x,y)
## --- Non-Vectorized Logical operators --- ##
x || y
x && y
## --- Logical operators with NA --- ##
TRUE | NA
# TRUE
TRUE & NA
# NA
FALSE | NA
# NA
FALSE & NA
# FALSE
## --- Exclusive Or --- ##
# xor() indicates elementwise exclusive OR
x <- c(TRUE, TRUE, FALSE, FALSE, NA, NA, NA)
y <- c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, NA)
xor(x, y)
## --- any() and all() --- ##
u <- c(TRUE, TRUE, TRUE)
c(any(u), all(u))
v <- c(TRUE, TRUE, FALSE)
c(any(v), all(v))
# if adding logical(0), nothing should change
## --- is functions --- ##
is.na()
is.null()
is.atomic()
is.logical()
is.integer()
is.double()
is.numeric()
is.character()
is.list()
is.matrix()
is.array()
is.data.frame()
is.factor()
is.function()

# --- Section 2: Conditional Statements --- #
# if(cond) the condition in an if() statement must be a length-one logical evctor that is not NA
# if condtion results n NA --> "missing value where TRUE/FALSE needed"
# if condition is length 0 --> "argument is of length zero"
## --- Nesting Conditionals --- ##
x <- 3
if (x < 0) {
  print ("Negative")
} else if(x > 0){
  print("Positive")
} else {
  print("Zero")
}
## --- if() is not vectorized, ifelse() is vectorized --- ##
if (x == 5) {
  n <- "yes"
} else {
  n <- "no"
}
# same as
n <- ifelse(x ==5, "yes", "no")

# --- Section 3: Loops --- #
## --- for loops --- ##
library(datasets)
state.name[1:5]
for (state in state.name[1:5]) {
  cat(state, "has", nchar(state), "letters in its name. \n")
}
## --- Loops - Storing results --- ##
# Good
res <- rep(NA, 10^7) # create an object to store results
system.time(
  for (x in seq_along(res)) {
    res[x] <- x^2
  }
)
# Slower
res <- 0
system.time(
  for (x in 1:10^7) {
    res[x] <- x^2
  }
)
# Slowest
res <- c()
system.time(
  for (x in 1:10^5) {
    res <- c(res, x^2)
  }
)
## --- while loops --- ##
i <- 1
res <- rep(NA, 10)
while (i <= 10) {
  res[i] <- i^2
  i <- i + 1
}
res
## --- repeat loops --- ##
i <- 1
res <- rep(NA, 10)
repeat {
  res[i] <- i^2
  i <- i + 1
  if (i > 10) {
    break
  }
}
res
## --- Special keywords - break and next --- ##
# break - ends the current (inner-most) loop
# next - ends the current iteration and starts the next iteration
for(i in 1:10) {
  if (i %% 2 == 0) {
    break
  }
  cat(i, "")
}
for(i in 1:10) {
  if (i %% 2 == 0) {
    next
  }
  cat(i, "")
}
# seq_len() and seq_along() avoids issues of length-0 vectors produced by length()

# --- Section 4: Vectorizing Code --- #
f <- function(x) {
  if (x <= 0) {
    value <- -x^3
  } else if (x <= 1) { # note: I do not need to check if x > 0
    value <- x^2
  } else {
    value <- sqrt(x) 
  }
  return(value)
}
## --- Using a non-vector function --- ##
x <- seq(-2, 2, by = 0.01)
plot_values <- f(x) # Error in if (x<=0)
plot(x, plot_values, type = "i") # error in eval(expr, envir, enclos)
plot_values <- rep(0, length(x)) 
# f is not vectorized and requires a loop to evaluate each values in x separately
for (i in seq_along(x)) {
  plot_values[i] <- f(x[i])
}
plot(x, plot_values, type = "l")
## --- Vectorize with ifelse --- ##
f2 <- function(x) {
  ifelse(x <= 0,
         -x^3,
         ifelse(x <= 1,
                x^2,
                sqrt(x)
           
         )
  )
}
f2 <- function(x) {
  ifelse(x <= 0, -x^3, ifelse(x <= 1, x^2, sqrt(x)))
}
# rowSums and colSums faster than apply

# --- W2 Friday Jan 19th, 2024 --- #
# --- Functions Writing Strategies --- #
## --- Functions Basics --- ##
name <- function(argument_1, argument_2, ...) {
  expression_1
  expression_2
  ...
  return(output)
}
# If the function has only one line the curly braces can be left off(not recommended)
f <- function(x) x * x
list(f)
typeof(f)
mode(f)
f <- function(x) {
  return(x * x) # explicit
}
f <- function(x) {
  x * x # implicit
}
# functions can only return one object
f <- function(x) {
  c(x, x^2, x^3)
}
f(2)
f <- function(x) {
  list(x = x, square = x^2, cube = x^3)
}
f(2)
# If you do not specify arg names, R assumes you are putting them in order
f <- function(x, y, z, w) {
  paste0("x = ", x, "y = ", y, "z = ", z)
}
f(x = 1, y = 2, w = 4) # Error bc argument z is missing with no default
# Values created inside a function only exist inside the function
x <- 10
f <- function(x) {
  x <- x + 55
  x
}
f(x) # adds 55
x # unchanged
x <- f(x) # to assign new value
# It is better to change code in one location than to try to change copies of the code everywhere
## --- Function Writing Strategies --- ##
# Bubble sort is a simple (but not very efficient) way to sort a vector
# First pass:
# (5 1 8 2 4) --> (1 5 8 2 4) first two elements
# (1 5 8 2 4) --> (1 5 8 2 4) no swap bc 5 < 8
# (1 5 8 2 4) --> (1 5 2 8 4) swap bc 8 > 2
# (1 5 2 8 4) --> (1 5 2 4 8) swap bc 8 > 4
# ...
# Big tasks can be broken down into smaller tasks
# ...
bubble_sort <- function(x) {
  locked <- 0
  for (s in seq_len(length(x) - 1 - locked)) {
    for (i in seq_len(length(x) - 1 - locked)) {
      if (x[i] > x[i + 1]) {
        temp <- x[i]
        x[i] <- x[i + 1]
        x[i + 1] <- temp
      }
    }
    locked <- locked + 1
  }
  x
}
x <- sample(100)
bubble_sort(x)
# Recap
# 1. Break down the task into smaller steps
# 2. Don't start building your function right away
# 3. Work in the global environment
# 4. Write your code
# 5. When something breaks, check the values in the global environment
# 6. Clear your environment each time you change something
# 7. Add to your code incrementally
# 8. Test each change thoroughly before moving on
# When drafting your code, work with a script file
# Use rm(list = ls()) at the top of script so it clears the global environment each time you source it
# Make changes ad source them with Cmd + Shift + S
# Investigate errors directly in the console


# --- Week 3 --- #
# --- Monday Jan 22nd, 2024 --- #
# --- Environments and Scoping Rules --- #
## --- Section 1: Building and Scoping --- ##
# objects in the local environment will take precedence over objects in the global environment. In particular, a global object will be masked by a local object with the same name: The object name within the function will reference the local object instead of the global one.
x <- c(1, 2, 3)
y <- x
z <- c(1, 2, 3) # points to a different copy of c(1, 2, 3)
y[[3]] <- 4 # does not modify x
y # 1 2 4 instead of 1 2 3
y <- 1
g <- function(x) {
  y <- 2
  x + y # uses y in current environment
}
g(3) # 5 and not 3 bc y inside function is 2
# lexical scoping(main scoping rule): If R cannot find a variable in the function body's scope, it will look for it in the next higher scope, and so on
# The exception is the super assignment (<<-) operator which searches a higher scope and the global environment:: Dangerous and do not use regularly
x <- 1
y <- 1
z <- 1
f <- function() {
  y <- 2
  g <- function() {
    z <- 3
    return(x + y + z)
  }
  return(g())
}
f() # returns 6
c(x, y, z) # x y z still 1 bc global env instead of local env
## --- Section 2: Environments --- #
# the environment is the data structure that powers scoping
# job of an environment is to associate, or bind, a set of names to a set of values
e <- new.env() # create new environemnt
e$a <- FALSE
e$b <- "a"
e$c <- 2.3
e$d <- 1:3
typeof(e) # environemnet
e[["d"]]
e[[1]] # does not work because not ordered so numeric index produces error
ls(e)
ls.str(e)
# Unlike lists
# - The single square bracket [] does not work for environments
# - Setting an object to NULL does not remove the object
# - Removing objects can be done using the rm() function
e$d <- NULL
ls(e)
rm(d, envir = e) # properly removes d from environment
ls(e)
# Every environment has a parent, another environment
# The parent is used to implement lexical scoping: If a name is not found in an environment then R will look in its paren (and so on)
# - only the empty environment does not have a parent
# - globalenv(), or global environment, also called the workspace. Parent is last package attached with library() or require()
# - baseenv(), or base environment, is the environment of the base package. Its parent is empty envir
# - emptyenv(), or empty environment, is the ultimate ancestor of all environments and without a parent envir
# - environment() is the current environment
# seach() funcion lists all parents of the global envir
search()
# each time a new package is loaded with library(), the package environment is inserted between the global envir and the package that was previously at the top of the search path
# the enclosing envir is the envir where the function was created. Function only has one. Others may have 0, 1, or many associated with each func:
# - binding a function to a name with the assignment <- operator defines a binding envir
# - calling a function creates a temporary execution envir that stores variables created during execution
# - every execution is associated with a calling envir, which tells you where the function was called
# In most scenarios, the enclosing envir and binding envir are the same
y <- 1
f <- function(x) {
  x + y
}
environment(f) # global envir
# namespace is the internal envir where the package functions are defined
# the package envir is the external interface to the package. Parent is determined by the search path
# double colo :: operator can be used to access a fuction directly from the package envir
h <- function(x) { 
  a <- 2
  x + a
}
y <- h(1)
y # output 3 bc x defined as 1 within function
# R also supports dynamic scoping where you can look for values in the calling envir. Achieved using the function get()
# Ex1:
x <- 0
y <- 10
f <- function() {
  x <- 2
  y <- 100
  h <- function() {
    x <- 50
    x + y # returns this val bc h() called when f() called using x <- 50 and y <- 100
  }
  h()
}
f() # 150
# Ex2:
x <- 0
y <- 10
g <- function() {
  x <- 2
  y <- 100
  h() # only calling function within g
}
h <- function() { # defined in global
  x <- 3
  x + y
}
h() # 13 bc h() x <- 3 and global y <- 10
g() # 13 bc h() called in global and not within g() so y <- 100 not used
# Dynamic Scoping Ex:
x <- 0
y <- 10
g <- function() {
  x <- 2
  y <- 100
  h()
}
h <- function() {
  x <- 3
  y <- get("y", envir = parent.frame()) # dynamic scoping
  x + y
}
h() # 13 bc gets y from global
g() # 103 gets y from parent frame with y <- 100
## --- Section 3: Super Assignment --- ##
# The super assigment <<- operator never creates a variable in the current environemnt but instead modifies an existing variable found in the parent envir
# WARNING: If (<<-) does not find an exisitng variable in the parent environemtn, it will climb the scope ladder until its finds teh variable it is looking for. If not found, it will be created in the global envir
# SUPER ASSIGNMENTS SHOULD GENERALLY BE AVOIDED
x <- 1; y <- 1; z <- 1
f <- function() {
  y <<- 3
  return(y)
}
f() # 3
c(x, y, z) # y is now 3 in the global
x <- 1; y <- 1; z <- 1
f <- function() {
  y <<- 2 # modifies global
  g <- function() {
    z <<- 3 # attempts to modify z in func f but not possible --> global
    return(x + y + z)
  }
  return(g())
}
f() # 6 bc x = 1, y = 2, z = 3
c(x, y, z) # modified through super assignment

# BAD:
add_sq_bad <- function(foo, x) {
  foo <<- c(foo, x^2) # combines foo with x^2 and super assings back to foo
}
foo <- 2
add_sq_bad(foo, 5)
foo # 2 25 seems to work
bar <- 10
add_sq_bad(bar, 6)
bar # unchanged
foo # foo changed to 10 36 which is bad
# Better method
add_sq_good <- function(baz, x) {
  c(baz, x^2)
}
foo <- 2
foo <- add_sq_good(foo, 5)
foo # 2 25
bar <- 10
bar <- add_sq_good(bar, 5)
bar # 10 25

# --- Wednesday Jan 24th, 2024 --- #
# Importing / Exporting / Webscraping
## --- Section 1: Importing / Exporting --- ##
readline() # input from terminal
read.table()
read.csv()
# Use stringAsFactors = FALSE
# install.packages(below)
library(readr) # general file reader
library(readxl) # importing excel
library(haven) # importing SAS, SPSS, STATA
library(data.table) # large tables
### --- Package readxl --- ###
read_excel("datasets.xls") # will read in the first worksheet
### --- Package downloader --- ###
# install.packages("downloader")
library(downloader)
url <- "https://raw.githubusercontent.com/smileschen/playground/master/iris.csv"
download(url, file = "iris.csv") # files will get saved to your working directory
iris <- read_csv("iris.csv")
### --- Saving and Exporting Data --- ###
save(object1, object2, ... , file = "object.RData") # native .RData format
write(x, "file.txt", ncol = 1) # saves atomic vector as plain text
write.csv(df, file = "df.csv") # saves a data.frame to csv file
write.csv(df, file = "df.csv", row.names = FALSE) # removes row names
### --- Package lubridate --- ###
# makes getting date info into R much easier
# install.packages("lubridate")
library("lubridate")
sdates <- c("January 15, 1999", "12-15-2001", "03/18/2002")
mdy(sdates) # parse a vector of dates with four different formats
# requires to be written in the sam order
### --- Package rvest --- ###
# install.packages("rvest")
library(rvest)
# great for fairly static webpages with well defined html tags
# not good for sites generated via JS, i.e. infinite scroll or log-in sites
# selector gadget works by looking at the CSS tags and selecting that specific tag
old_reddit <- read_html("https://old.reddit.com/")
comment_counts <- old_reddit %>%
  html_nodes(".comments") %>%
  html_text()
comment_counts
### --- Using your scraped text --- ###
# extract values using regex
library(stringr)
as.numeric(str_extract(comment_counts, "\\d+"))
# Works best for fairly static, public pages
# The css selector
old_reddit <- read_html("https://old.reddit.com/")
old_reddit %>%
  html_nodes(".title.may-blank") %>%
  html_text()
### --- Other Webscraping Options --- ###
# to work well with JS, use RSelenium --> steeper learning curve
# * Python Beautiful Soup - very easy to learn
# * Python Scrapy - more complex, can crawl across many pages
# * Python Selenium - supports JS generated pages
## --- Section 2: rvest demo --- ##
library(polite) # will access website slowly and more human-like
library(rvest)
library(stringr)

# Store web url
lego_movie < read.html("https://www.imdb.com/title/tt1490017/")

# Scrape the website for the movie rating

#Scrape the cast for the movie
cast <- lego_movie %>%
  html_node()
  html_text()
cast

# to create polite session
session <- bow("https://www.imdb.com/title/tt1490017/")

# --- Friday, Jan 26th, 2024 ---  #
# --- Tidy Data --- #
## --- Section 1: The tidyverse --- ##
install.packages("tidyverse")
library(tidyverse)
## --- Section 2: Tibbles --- ##
# data frames but tweak some older behaviors to make use easier
# refined print method that shows only the first 10 rows
rand_vals <- runif(1e3)
x <- tibble(
  a = lubridate::now() + rand_vals * 86400,
  b = lubridate::today() + rand_vals * 30,
  c = 1:1e3,
  d = rand_vals,
  e = sample(letters, 1e3, replace = TRUE)
)
print(x)
tb <- tibble(x = 1:3, y = 3:1)
tb[, 1] # subset first col remains a tibble
tb[, 1, drop = TRUE] # simplify after subsetting
tb[, 1] %>% pull
# Can create a tibble the same way as a data frame
tib <- tibble(
  a = sample(5),
  b = letters[sample(5)],
  c = rnorm(5)
)
# Can use it on exisitng data
mtcar_tib <- tibble(mtcars)
mtcar_tib
# Creation row-wise with tribble and ~colName
tib_r <- tribble(
  ~colA, ~colB,
  "a", 1,
  "b", 2,
  "c", 3
)
print(tib_r)
options(pillar.sigfig = 5) # will display 5 significant figures
tibble(col_a = 9.8765432, col_b = 1.1234567)
## --- Section 3: Pivoting Data --- ##
# Converts between long and wider forms
storms <- read_csv("https://raw.githubusercontent.com/rstudio/EDAWR/master/data-raw/storms.csv")
cases <- read_csv("https://raw.githubusercontent.com/rstudio/EDAWR/master/data-raw/cases.csv")
pollution <- read_csv("https://raw.githubusercontent.com/rstudio/EDAWR/master/data-raw/pollution.csv")
storms$ratio <- storms$pressure / storms$wind
# 1007 / 110 = 9.15, 1009 / 45 = 22.4, etc.
storms
cases
pollution
# pivot cases data to have cols going through years rhtat than each year being its own col
pivot_longer(cases,
             cols = "2011":"2013",
             names_to = "year",
             values_to = "cases")
# pivot_longer() function args
  # data is the name of the data.frame or tibble that we will pivot
  # cols are the names of the columns that will be pivoted
  # names_to is a character string with what you want to call the resulting columns of names
  # values_to is a character string with what you want to call the resulting col of values
# names are arbitrary
pivot_longer(cases,
             cols = "2011":"2013",
             names_to = "when it happened",
             values_to = "how many")
# separate year 2011 into a col
pivot_longer(cases,
             cols = "2012":"2013",
             names_to = "year",
             values_to = "cases")
# "country":"2013" in cols arg
pivot_longer(cases,
             cols = "country":"2013",
             names_to = "name",
             values_to = "value")
# convert everything into char
cases2 <- data.frame(lapply(cases[,1:4], as.character))
names(cases2) <- c("country", "2011", "2012", "2013")
pivot_longer(cases2,
             cols = "country":"2013",
             names_to = "name",
             values_to = "value")
pollution
pivot_wider(pollution,
            names_from = "size",
            values_from = "amount")
# pivot_wider() function args
  # data is the anme of data.frame or tibble
  # names_from is the name of the col that has the names that will become co headers
  # values_from is the name of the col that has the values
pollution2 <- pollution
# Name sensitive
pollution2[1,1] <- "NYC"
pollution2
pivot_wider(pollution2,
            names_from = "size",
            values_from = "amount")
# If NA not wanted specify a fill value with values_fill arg
pivot_wider(pollution2,
            names_from = "size",
            values_from = "amount",
            values_fill = 0)
pollution2 <- pollution
pollution2[1,2] <- "LARGE"
pollution2
pivot_wider(pollution2,
            names_from = "size",
            values_from = "amount")
# pivot_longer() and pivot_wider() are inverse operations
pollution
w <- pivot_wider(pollution, names_from = "size", values_from = "amount")
w
pivot_longer(w, cols = "large":"small", names_to = "size", values_to = "amount")
cases
l <- pivot_longer(cases, cols = "2011":"2013", names_to = "year", values_to = "count")
l
pivot_wider(l, names_from = "year", values_from = "count")
# other important operations include "rectalngling" 

### --- Week 4 --- #
# --- Data Wrangling with dplyr --- #

## --- Section 1: dplyr --- #
# dplyr provides simple verbs that correspond to the most comon data manioulation tasks
# select() - picks variable based on their names
# filter() picks cases based on their values
# mutate() adds new variables that are functions of existing variables
# arrange() changes the ordering of the rows
# summarise() reduces multiple values down to a single summary
# group_by() allows to perform any operaton 'by group'
# https://github.com/rstudio/cheatsheets/blob/master/data-transformation.pdf

## --- Section 2: dplyr examples --- ##
starwars
select(starwars, name, homeworld, species, films)
# pipe %>% takes the result of what is in front of the pipe and inserts it as the first arg
# equivalent to above
starwars %>%
  select(name, homeworld, species, films)
# |> native pipe
# equivalent to above
starwars |>
  select(name, homeworld, species, films)
# Shorcut to insert pipe: CTRL(CMD) + SHIFT + M
# Use negative sign to deselect columns
starwars %>%
  select(-name, -eye_color, -birth_year) %>%
  head(3)
# Use colon notation to select a range of cols
starwars %>%
  select(name:eye_color) %>%
  head(3)
# dplyr has special selection functions
  # contains() - select col that contains a char string
  # starts_with() - cols start with char string
  # ends_with() - col tat ends with string
  # matches() - col that match regex
  # everything() - all col
  # num_range() - col named smth like x1, x2, x3, x4, x5, etc
  # one_of(name_vector) sel col where names are stored in vector
starwars %>%
  select(name, ends_with("color")) %>% # selects name and columns ending with color
  head(3)
starwars %>%
  select(name, matches("s$")) %>%
  head(3)
# select with vector of names
vars <- c("name", "mass", "height")
starwars %>% select(all_of(vars))
starwars %>%
  filter(name == "R2-D2")
# multiple conditions using , which acts as &
starwars %>%
  filter(species %in% c("Human", "Droid"), height < 175)
starwars %>%
  filter(str_detect(name, "ˆF")) # the name starts with F
# use | for 'OR'
starwars %>%
  filter(hair_color == "none" | eye_color == "black") %>%
  select(name, species, homeworld, hair_color, eye_color)
# desc() to put things in descending order
starwars %>%
  select(name, birth_year, height, mass) %>%
  arrange(desc(birth_year), mass)
# slice() lets you select rows based on their locations
starwars %>% slice(5:10)
# slice_sample() lets you randomly select rows which can be useful to get a peek at portions of the entire tibble rather than just he head
starwars %>% slice_sample(n = 5)
# slice_min() and slice_max() select row with min max vals in a variable
starwars %>% slice_max(mass, n = 3)
# mutate() create new variables based on existing variables
starwars %>% 
  mutate(height_in = height / 2.54) %>% head(1)
starwars %>%
  mutate(height_in = height / 2.54) %>%
  select(name, height, height_in) %>% head(1)
# IMPORTANT: mutate() adds a new col to data set so variable we are creating must have the sam enumber of values as rows in the data set
starwars %>%
  select(name, mass) %>%
  mutate(cumulative_mean = cummean(mass))
# useful functions for mutate()
  #pmin(), pmax() - element-wise min and max
  # cummin(), cummax() - cumulative min and max
  # cumsum(), cumprod() - cumulative sum and product
  # between() - are values between a and b?
  # cummean() - cumulative mean
  # lead(), lag() - copy values with offset
  # ntile() - bin vector into n buckets
starwars %>%
  select(name, mass, birth_year) %>%
  mutate(
    cummin_mass = cummin(mass), # cummin gives the min value seen so far
    ratio = mass / mean(mass, na.rm = TRUE), # we divide mass/by the col mean
    massyear_pmin = pmin(mass, birth_year), # pmin gives the element-wise min
    lag2 = lag(massyear_pmin, 2)) # lag offsets the column values
# summarize() - summarise with a single value
starwars %>%
  select(height, mass) %>%
  summarise(
    avg_height = mean(height, na.rm = TRUE),
    var_height = var(height, na.rm = TRUE),
    avg_mass = mean(mass, na.rm = TRUE),
    min_height = min(height, na.rm = TRUE),
    max_mass = max(mass, na.rm = TRUE),
    count = n())
# group_by() combined with summarise()
starwars %>%
  group_by(species) %>%
  select(name, height, mass, species) %>%
  summarise(
    mean_ht = mean(height, na.rm = TRUE),
    sd_ht = sd(height, na.rm = TRUE),
    mean_mass = mean(mass, na.rm = TRUE),
    sd_mass = sd(mass, na.rm = TRUE),
    count = n())
starwars %>%
  group_by(species) %>%
  select(name, height, mass, species) %>%
  summarise(
    mean_ht = mean(height, na.rm = TRUE),
    sd_ht = sd(height, na.rm = TRUE),
    mean_mass = mean(mass, na.rm = TRUE),
    sd_mass = sd(mass, na.rm = TRUE),
    count = n()
  ) %>%
  filter(count > 1) %>%
  arrange(desc(count)) %>%
  head()
# group_by() + mutate()
starwars %>%
  filter(species %in% c("Human","Droid") |
           is.na(species)) %>%
  select(name, species, height) %>%
  group_by(species) %>%
  mutate(z_height = (height - mean(height, na.rm = TRUE))/sd(height, na.rm = TRUE)) %>%
  head()
# below average when compared to all species rather than just droids like above
starwars %>%
  filter(species %in% c("Human","Droid") |
           is.na(species)) %>%
  select(name, species, height) %>%
  # group_by(species) %>%
  mutate(z_height = (height - mean(height, na.rm = TRUE))/sd(height, na.rm = TRUE)) %>%
  head()
# multiple group_by()
toy_cases <- read_csv("https://raw.githubusercontent.com/rstudio/EDAWR/master/data-raw/toyb.csv")
print(toy_cases)
# multiple group_by() + summarise()
summary1 <- toy_cases %>% group_by(country, year) %>%
  summarise(cases = sum(cases))
print(summary1)
summary2 <- summary1 %>% summarise(cases = sum(cases))
summary2
summary3 <- summary2 %>% summarise(cases = sum(cases))
summary3
# Changing the order of the multiple group_by()
summary_a <- toy_cases %>% group_by(year, country) %>%
  summarise(cases = sum(cases))
print(summary_a)
summary_b <- summary_a %>% summarise(cases = sum(cases))
summary_b
summary_c <- summary_b %>% summarise(cases = sum(cases))
summary_c

## --- Section 3: Two-table verbs --- ##
# mutating joins add new variables to one table from matching rows in another
# filtering joins filter observations from one table based on whter or not they match an observation in the other
# set operations, which combine the observations in the data sets as if they were set elements
people <- tibble(
  name = c("Adam", "Betty", "Carl", "Doug"),
  state = c("CA", "CA", "NY", "TX")
)
states <- tibble(
  abbreviation = c("CA", "NY", "WA"),
  state_name = c("California", "New York", "Washington")
)
# left_join - takes all vals in left table and adds variales from right table by matching values using a col that exists in both tables
people %>% left_join(states, by = c("state" = "abbreviation"))
# right_join - similar but oppostite direction 
people %>% right_join(states, by = c("state" = "abbreviation"))
# inner_join() - keeps only rows tat have values that exist in both tables -- intersection
people %>% inner_join(states, by = c("state" = "abbreviation"))
# full_join() - keeps all rows from both tables -- union
people %>% full_join(states, by = c("state" = "abbreviation"))

# --- Wednesday Jan 31st, 2024 --- #
# --- Working with Strings in R --- #
## --- Section 1: Characters in R --- ##
x <- "Hello World"
x
typeof(x)
character(5)
# paste(..., sep = " ", collapse = NULL) - converts into character then concatenates them to form several char strings
# paste0() == paste(..., sep = "", collapse)
paste("I ate some", pi, "and it was delicious.")
# print() - for generic printing
# cat() - for concatenation
# format() is for pretty printing with special formatting
print(x, quote = FALSE)
noquote(x)
cat(x, "Hello Universe", sep = ", ")
cat(x, "Hello Universe", sep = ", ", file = "hello.txt")
# format() args
  # width - min width of strings produced
  # trim - where there should be padding with spaces(TRUE)
  # justify - controls how paddings takes place for strings - left, right, centre, or none
  # digits - specifies sig fig
  # nsmall - min num of digits to the right
  # scientific - specify scientific notation
format(1 / (1:5), digits = 2)
format(1 / (1:5), digits = 2, scientific = TRUE)
format(c("Hello", "world", "Hello", "Universe"),
       width = 10, justify = "left"
)

## --- Section 2: Basic String Manipulation --- ##
# nchar() - num of chars
# tolower() - lower case
# toupper() - to upper case
# chartr() - translates chars
# substr() - extract substring of char vec
# strsplit() - splits strings into substrings
y <- c("Hello", "World", "Hello", "Universe")
nchar(y)
tolower(y)
toupper(y)
chartr("H", "y", y)
chartr("elio", "31!0", y)
substr(x, 2, 9)
strsplit(x, split = " ")
strsplit(x, split = "o")

## --- Section 3: stringr --- ##
library(tidyverse)
# str_c() - string concantenation
# str_length() - num of characters
# str_sub() - extract substrings similar to substr()
# str_dup() - duplicates characters
# str_trim() - removes leading and trailing white
# str_pad() - pad a string 
# str_wrap() - wraps a string paragraph
# str_trim() - trims a string
paste(x, NULL, character(0), c("Hello Universe"),
      sep = ", "
)
# str_c() automatically removes NULL and charracter(0) args
str_c(x, NULL, character(0), c("Hello Universe"),
      sep = ", "
)
nchar(factor(month.name))
# str_length() works with factors
str_length(factor(month.name))

## --- Section 4: Regular Expression --- ##
# regex is a set of symbols that describes a pattern
# str_detect(str, pattern) - detect presence of a pattern and returns TRUE if found
# str_locate(str, pattern) - locate 1st position of a pattern and return a matrix with start & end
# str_extract(str, pattern) - extracts text corresponding first match
# str_match(str, pattern) - extracts capture groups formed by () from the first match
# str_split(str, pattern) - splits string into pieces and returns a list of character vectors
# literal characters which are characters that match themselves
str_locate("I love stats", "stat") # 8 11
str_locate("I love Stats", "stat") # NA NA bc case sensitive
love_stats <- "I love statistics, so I am a stats major."
str_locate(love_stats, "stat") # 8 11
str_locate_all(love_stats, "stat") # 8 11 \ 30 33
### --- Metacharacters --- ###
# any character that is not a literal character is a metacharacter
# . ˆ $ * + ? { } [ ] \ | ( )
# dot(period) . is called the wild metacharacter used to match ANY (single) character except a newline
not <- c("not", "note", "knot", "nut")
str_detect(not, "n.t") # TRUE TRUE TRUE TRUE
str_detect(not, "no.") # T T T F
fives <- c("5.00", "5100", "5-00", "5 00")
str_detect(fives,"5.00") # T T T T
# \\. to match literal '.'
str_detect(fives, "5\\.00") # T F F F
pnx <-
  c("pan", "pen", "pin", "p0n", "p.n", "paun", "pwn3d", "happiness")
str_detect(pnx, "p[aeiou]n") # [] character set
# [a-z] for all lowercase, [a-zA-Z] for all letters, [0-7] for 0 to 7
triplets <- c("123", "abc", "ABC", ":-)", "ab12a", "a8908ab")
str_detect(triplets, "[0-9][0-9][0-9]") # T F F F F T
# negative charcater set: by matching any one character that is not in the set using '^'
# i.e. [^aeiou] 
basic <- c("1", "a", "A", "&", "-", "ˆ")
str_detect(basic, "[ˆA-Z]") # F F T F F T
str_detect(basic, "[A-Zˆ]") # make sure '^' first in character set
str_detect(basic, "[ˆ\\ˆ]") # or ^^
str_detect(basic, "[ˆ\\ˆ]") # T T T T T F
str_detect(pnx, "p[ae.iou]n")
# Shortcuts
  # \\d - any digit == [0-9]
  # \\D - any non-digit ==  [^0-9]
  # \\w - any word char == [a-zA-Z0-9_]
  # \\W - any non-word char == [^a-zA-Z0-9_]
  # \\s - any whitespace character == [\f\n\r\t\v]
  # \\S - any non-whitespace char == [^\f\n\r\t\v]
    # \f - form feed (page break)
    # \n - line feed (new line)
    # \r - carriage return
    # \t - tab
    # \v - vertical tab
str_detect(pnx, "p\\d") # p followed by digit
pnx <-
  c("pan", "pen", "pin", "p0n", "p.n", "paun", "pwn3d")
str_detect(pnx, "p\\D") # p followed by non-digit
str_detect(pnx, "p\\W") # p followed by non-word character
# Class Description Same as
# [:alnum:] Any letter or digit [a-zA-Z0-9]
# [:alpha:] Any letter [a-zA-Z]
# [:digit:] Any digit [0-9]
# [:lower:] Any lower case letter [a-z]
# [:upper:] Any upper case letter [A-Z]
# [:space:] Any whitespace, inluding space [\f\n\r\t\v ]
# [:punct:] Any punctuation symbol
# [:print:] Any printable character
# [:graph:] Any printable character excluding space
# [:xdigit:] Any hexadecimal digit [a-fA-F0-9]
# [:cntrl:] ASCII control characters
str_detect(pnx, "[[:alpha:]]") # has any letter
str_detect(pnx, "[[:digit:]]") # has any digit
# anchor - a pattern that does not match a character but rather a position before, after, or between characters ->match a certain pos
# Pattern --- Meaning
# ˆ or \A --- Start of string
# $ or \Z --- End of string
# \b      --- Word boundary (i.e., the edge of a word)
# \B      --- Not a word boundary
text <- "the quick brown fox jumps over the lazy dog dog"
str_replace_all(text, "the", "-") # 'the' anywhere
str_replace_all(text, "ˆthe", "-") # 'the' only at the start
str_replace_all(text, "\\Athe", "-") # same thing
str_replace_all(text, "the$", "-") # 'the' only at the end
str_replace_all(text, "dog", "-") # 'dog' anywhere
str_replace_all(text, "dog$", "-") # 'dog' only at the end
text <- "words jump jumping umpire pump umpteenth lumps"
str_replace_all(text, "(\\b.|.\\b)", "-") # word boundaries
str_replace_all(text, "\\B.\\B", "-") # non-word-boundaries
str_replace_all(text, "\\bump", "-") # 'ump' at the beginning of a word
str_replace_all(text, "\\Bump", "-") # 'ump' not at the beginning of a word
str_replace_all(text, "ump\\b", "-") # 'ump' at the end of a word
str_replace_all(text, "ump\\B", "-") # 'ump' not at the end of a word
# ^[0-9] - anchor so matches strings that begin with a digit
# [^0-9] - matches a character that is not a digit
# [0-9^] - literal caret character
# Quanitifiers - can be attaches to literal characters, character classes, or groups to match repeats
# Pattern | Meaning
# *       | Match 0 or more (is greedy)
# +       | Match 1 or more (is greedy)
# ?       | Match 0 or 1
# {3}     | Match Exactly 3
# {3,}    | Match 3 or more
# {3,5}   | Match 3, 4 or 5
text <- "words or numbers 9,876 and combos123 like password_1234"
str_replace_all(text, "\\s", "-") # any whitespace
str_replace_all(text, "\\S", "-") # anything but whitespace
str_replace_all(text, "\\S+", "-") # one or more non-whitespace
str_replace_all(text, "\\w+", "-") # one or more word characters
str_replace_all(text, "\\d", "-") # any digit
str_replace_all(text, "\\D", "-") # any non-digit
str_replace_all(text, "\\d+", "-") # one or more digits
str_replace_all(text, "\\D+", "-") # one or more nondigits
text <- "year 1996 area code 310 combo123 password_1234 singledigit5"
str_replace_all(text, "\\d{3}", "-") # 3 adjacent digits. reads left to right
str_replace_all(text, "\\d{2,4}", "-") # 2 to 4 adjacent digits
text <- c("Momma","Mama","Mamma","Mommy","Mom","Mother")
str_match(text, "M[ao]m{1,2}[ay]") |> as.vector() # [ay] required as last character
str_match(text, "M[ao]m{1,2}[ay]?") |> as.vector() #? makes [ay] optional as last character
# Quanitifiers are by default greedy in the sense that they will return the longest match
  # adding '?' to a quantifier will make it ungreedy (or lazy), so it will return the shortest match
text <- "Peter Piper picked a peck of pickled peppers"
str_extract(text, "P.*r") # 'P' to 'r' anything in between greedy
str_extract(text, "P.*?r") # 'P' to 'r' anything in between ungreedy
str_extract_all(text, "P.*?r") # ungreedy
str_extract_all(text, "[Pp].*?r") # ungreedy
# Grouping and Capturing
  # parantheses () define a group that groups together parts of regular expression
  # besides groupig part of a regex together, parantheses also create a numbered capturing group: any matches to the part of the pattern defined by the parantheses cna be referenced by group number, either for modification or replacement
  # by including '?': after the opening parathesis, the group becomes a non-capturing group
# Pattern --------- Meaning
# a(bc)d        --- Match the text abcd, capture the text in the group bc
# (?:abc)       --- Non-capturing group
# (abc)def(ghi) --- Match abcdefghi, group abc and ghi
# (Mrs|Ms|Mr)   --- Mrs or Ms or Mr (preference in the order given)
# \1, \2, etc.  --- The first, second, etc. matched group (for str_replace())
pattern <- "(bc)(def)(?:ghi)" # 'ghi' must be present but do not capture 'ghi'
str_match("abcdefghijkl", pattern)
str_match("abcdefghI", pattern)
text <- "Mr. Smith, Mrs. Lee, Ms. Garcia"
pattern <- "(Mrs|Ms|Mr)" # match one of 'Mrs' or 'Ms' or 'Mr'
str_match_all(text, pattern)
# because Mr is listed before Mrs, it will match Mr and give preference to it
wrong_order <- "(Mr|Mrs|Ms)"
str_match_all(text, wrong_order)
text <- "Mr. Smith, Mrs. Lee, Ms. Garcia"
short_pattern <- "(Mr?s?)"
str_match_all(text, short_pattern)
text <- "Mr. Smith, Mrs. Lee, Ms. Garcia, Andy Hope"
capture <- "(Mrs|Ms|Mr)\\. (\\w+)"
str_match_all(text, capture)
text <- "Mr. Smith, Mrs. Lee, Ms. Garcia, Andy Hope"
non_capture <- "(?:Mrs|Ms|Mr)\\. (\\w+)"
str_match_all(text, non_capture)
text = 'George Washington, John Adams, Thomas Jefferson'
pattern <- "(\\w+) (\\w+),?" # first group becomes \\1, second becomes \\2
str_match_all(text, pattern)
str_replace_all(text, pattern, "\\2, \\1;")
# Grouping and Capturing Examples: Backreferences
text = 'the quick brown fox jumps over the the lazy dog'
pattern <- "\\b(\\w+)\\s+\\1\\b"
# The pattern says:
  # Word boundary
  # followed by a capture group of one or more word characters
  # followed by one or more spaces
  # followed by the group of text that was captured earlier
  # followed by a word boundary
str_match_all(text, pattern)
# To extract phone numbers
phone_pattern <- "\\(?([2-9]\\d{2})\\)?[- .]?([2-9]\\d{2}[- .]?\\d{4})"
text <- c("apple", "1-800-786-1000", "(310) 209-1626", "310.208.0448",
          "3108258430", "Work: 323 224 2611; Home: (323)224-2621", "123-456-7890")
str_extract(text, phone_pattern)
str_extract_all(text, phone_pattern)
str_match(text, phone_pattern)
str_match_all(text, phone_pattern)
# turn into something woorkable:
phone_list <- str_match_all(text, phone_pattern)
phone_matrix <- matrix(nrow = 0, ncol = 3)
for(i in seq_len(length(phone_list))){
  phone_matrix <- rbind(phone_matrix, phone_list[[i]])
}
phone_matrix
phone_matrix[, 3]
phone_pattern <- "(\\d{3})[- .]?(\\d{4})"
replace_pattern <- "\\1-\\2"
str_replace(phone_matrix[,3], phone_pattern, replace_pattern)
# Lookarounds
# Pattern  --- Name
# (?=...)  --- Positive lookahead --> looks ahead of the current match to ensure that the subpattern matches
# (?!...)  --- Negative lookahead --> looks ahead of current match to ensure that subpattern does not match
# (?<=...) --- Positive lookbehind --> looks behind the current pos to ensure that the subpattern immediately precedes the current match
# (?<!...) --- Negative lookbehind --> looks behind the current pos to ensure that the subpattern does NOT immediately preced the current match
text <- "I put a grey hat on my grey greyhound."
pattern <- "grey(?=hound)"
str_locate_all(text, pattern)
pattern <- "grey(?!hound)"
str_locate_all(text, pattern)
text <- "I withdrew 100 $1 bills, 20 $5 bills, and 5 $20 bills."
pattern <- "(?<=\\$)[[:digit:]]+"
str_extract_all(text, pattern) # 1 5 20
pattern <- "(?<!\\$)[[:digit:]]+"
str_extract_all(text, pattern) # 100 20 5 0




# --- Week 5 --- #
# --- Mon Feb 5, 2024 --- #
# --- R's Object Oriented Programming System: S3 --- #
## --- Section 1: Object Oriented Programming --- ##
# OOP - style of programming that focuses on defining different types of ojects and functions that can be applie do tthose objects
# - S3(most important) and R6
# S3 and S4 use generic function OOP
# R6 and RC use encapsulated OOP
# all object oriented behaviors are done in the S3 system
## --- Ex of a Polymorphic Function
library(ggplot2)
summary(diamonds$carat)
summary(diamonds$cut)
# most common polymorphic functions are print(), summary(), and plot()
# a class is a definition of an object - typically a class contains several fields that are used to hold class-specific information
# a method is an implementation (or function) for a specific class
# inheritance - classes are usually organized in a hierarchy, so if a method does not exist for a child, then its parent's method is used
# method dispatch - process of finding the correct method given a class
# generic function(generics) - are used to determining the appropriate method
## --- OOP Systems
# S3 is R's first, simplest, and most flexible object-oriented system. 
  # only used in base and stats package and most commonly used in CRAN packages
# S4 - is a formalization of S3 that has much stricter implementation for defining classes, methods, and generic functions
  # implemented in the base *methods* package
# RC - short for reference classes, implements "message-passing" OOP
  # methods belong to classes, not functions
# library(R6) to implement message-passing and encapsulated OOP
  # considered simpler and more elegant RC
# Base types - internal C-level types that are the basis fo the other object-oriented systems
  # the *struct* includes the contents of the object, the information needed for memory management, and most importantly for this section, a type
  # can determine an object's base type with typeof()
typeof(mean) # "closure"
is.function(mean) # TRUE
is.primitive(mean) # FALSE
typeof(sum) # "builtin"
is.primitive(sum) # TRUE
# is.object(x) to check if an object is a pure base type

## --- Section 2: The S3 Object-Oriented System --- ##
# Unfortunately, there is no simple way to ttest if an object is an S3 object in base R
  # closest is is.object(x) & !isS4(x)
df <- data.frame(x = 1:10, y = letters[1:10])
is.object(df) # TRUE
isS4(df) # FALSE
is.object(df$x)
is.object(df$y)
# S3 is generic -- UseMethod() which is the function that figures out the correct method to call, the process of *method dispatch*
# Functions that do method dispatch in C code are called *internal generics* and are documented in ?"internal generic"
  # can recognize methods by their names, which look like *generic.class()*
  # factor method for print() is called print.factor or mean.Date()
install.packages("pryr")
library(pryr)
is_s3_generic("t.test")
methods(t.test) # two versions of t.test
is_s3_generic("t.data.frame")
is_s3_method("t.data.frame")
methods(t)
x <- matrix(1:12, nrow = 4)
t(x)
df <- data.frame(a = 1:4, b = 5:8, c = 9:12)
t(df)
t.default
t.data.frame # converts df int matrix then applies the net available method for the object
methods("mean")
methods("t.test")
methods(class = "ts") # methods available for time series objects

## --- Section 3: Defining S3 Classes --- ##
# structure() or <- class() to make an object an instance of a class
# Create and assign class in one step
x <- structure(list("apple"), class = "fruit")
# Create, then set class
y <- list("banana")
class(y) <- "fruit"
class(x)
inherits(x, "fruit")
class(diamonds)
class(diamonds$cut)
inherits(diamonds$cut, "factor") # returns a single value
class(diamonds$cut) == "factor" # returns a vector bc class has length 2
# Constructors function that creates a new objects with the correct structure
  # have one argument fo the base object, and one for each attribute
  # check the type of the base object and the types of each attribute
fruit <- function(x) {
  stopifnot(is.character(x)) # checks to see if x is a character vector
  structure(list(x), class = "fruit")
}
# in use:
z <- fruit("pineapple")
z
# Create a linear model
lm_mtcars <- lm(log(mpg) ~ log(disp), data = mtcars)
class(lm_mtcars)
print(lm_mtcars)
# Change the class to data.frame
class(lm_mtcars) <- "data.frame"
print(lm_mtcars) # No longer prints properly
# But the data is still inside
lm_mtcars$coefficients
# lack of built-in validation of classes has the potential to be prblematic, but it rarely causes issues in practice

## --- Section 4: Creating S3 Methods and Generics --- ##
# to create a new generic, use UseMethod() -- two args, name of generic function, and argument to use for method dispatch
quotation <- function(x) {
  UseMethod("quotation")
}
quotation.fruit <- function(x) {
  "Fruits are an important part of a balanced diet."
}
x <- structure(list("banana"), class = "fruit")
class(x)
quotation(x)
mean.fruit <- function(x) {
  5
}
mean(x)
# the "default" class makes it possible to set up a fallback methods for otherwise unknown classes -- special pseudo-class but can define methods for it to use for inputs iwth unkown classes
quotation <- function(x) {
  UseMethod("quotation")
}
quotation.fruit <- function(x) {
  "Fruits are an important part of a balanced diet."
}
quotation.apple <- function(x) {
  "An apple a day keeps the doctor away."
}
quotation.default <- function(x) {
  "The default quotation: Let food be thy medicine and medicine be thy food."
}
# Dispatches method for apple class
a <- structure(list("Fuji"), class = c("apple", "fruit"))
quotation(a)
# No method for banana class, so uses method for fruit class
b <- structure(list("Chiquita"), class = c("banana", "fruit"))
quotation(b)
# No method for donut class, so falls back to default
c <- structure(list("Dunkin"), class = "donut")
quotation(c)
# Force R to call the wrong method
quotation.apple(b)
# If there is no defualt method and you attempt to use a function on an object for which no method exists, R will throw an error
rm(quotation.default) # we remove the default method
c <- structure(list("Dunkin"), class = "donut")
quotation(c) # we call the function on an object with class donut

## --- Section 3: Inheritance in S3 Classes --- ##
# S3 classes can share behavior through *inheritance*
  # the class of an object can be a character vector
  # if a method is not found for the class in the first element of the vector, R looks for a method for the second class(and so on)
  # a method can delegate work by calling *NextMethod()*
# s3_dispatch() function in the sloop package inputs a function call and outputs te list of all possible function names that are considered for method dispatch
  # lack of symbol means method does not exist
  # the => arrow means method exists and is found by UseMethod()
  # -> arrow means the method exists and is used through NextMethod()
  # * means method existss but is not used
install.packages("sloop")
library(sloop)
s3_dispatch(print(factor(letters, ordered = TRUE)))
# ordered class is said to be a subclass of a factor bc it always aooears before it in class vector
# conversely, the factor class is a superclass of ordered

## --- Section 6: Method Dispatch Self-Quiz --- ##
rm(list = ls())
f <- function(x) UseMethod("f") # the generic f function
f.j <- function(x) x + 2 # the f method for class j
f.k <- function(x) x + 10 # the f method for class k
k <- 1
f(k) # Error
s3_dispatch(f(k)) # full details of the result
class(k) <- "j" # object k has a class of "j"
f(k)
s3_dispatch(f(k))
# Self Quiz Q1
f <- function(x) UseMethod("f") # the generic f function
f.j <- function(x) x + 2 # the f method for class j
f.k <- function(x) x + 10
k <- 1
class(k) <- "k" # object k has a class of "k"
f(k)
s3_dispatch(f(k))
# Q2
class(k) <- "k"
f.default <- function(x) x + 100
f(k)
s3_dispatch(f(k)) # full details of the result
# Q3
f <- function(x) UseMethod("f") # the generic f function
f.j <- function(x) x + 2 # the f method for class j
f.k <- function(x) x + 10
f.default <- function(x) x + 100
k <- 1
class(k) <- c("a","b")
f(k)
s3_dispatch(f(k)) # full details of the result
# Q4
f <- function(x) UseMethod("f") # the generic f function
f.j <- function(x) x + 2 # the f method for class j
f.k <- function(x) x + 10
f.default <- function(x) x + 100
k <- 1
class(k) <- NULL
f(k)
s3_dispatch(f(k))
# Q5
f <- function(x) UseMethod("f") # the generic f function
f.j <- function(x) x + 2 # the f method for class j
f.k <- function(x) x + 10
f.default <- function(x) x + 100
f.l <- function(x) x + 50
l <- structure(10, class = c("k", "l"))
f(l)
s3_dispatch(f(l))
# Q6
f <- function(x) UseMethod("f") # the generic f function
f.j <- function(x) x + 2 # the f method for class j
f.k <- function(x) x + 10
f.default <- function(x) x + 100
f.l <- function(x) x + 50
l <- 10
class(l) <- c("m","l")
f(l)
s3_dispatch(f(l))
# Q7
f <- function(x) UseMethod("f") # the generic f function
f.j <- function(x) x + 2 # the f method for class j
f.k <- function(x) x + 10
f.default <- function(x) x + 100
f.l <- function(x) x + 50
l <- 10
class(l) <- c("m","n")
f(l)
s3_dispatch(f(l))
# Q8
f <- function(x) UseMethod("f") # the generic f function
f.j <- function(x) x + 2 # the f method for class j
f.k <- function(x) x + 10
f.default <- function(x) x + 100
f.l <- function(x) x + 50
l <- 10
class(l) <- c("m","n")
f.j(l)
#Q9
f <- function(x) UseMethod("f") # the generic f function
f.j <- function(x) x + 2 # the f method for class j
f.k <- function(x) x + 10
f.default <- function(x) x + 100
f.l <- function(x) x + 50
f(7)
s3_dispatch(f(7))
# Q10
f <- function(x) UseMethod("f") # the generic f function
f.j <- function(x) x + 2 # the f method for class j
f.k <- function(x) x + 10
f.default <- function(x) x + 100
f.l <- function(x) x + 50
f.j(7)
# Q11
f <- function(x) UseMethod("f") # the generic f function
f.j <- function(x) x + 2 # the f method for class j
f.k <- function(x) x + 10
f.default <- function(x) x + 100
f.l <- function(x) x + 50
f.integer <- function(x) 100 * x
f(7)
s3_dispatch(f(7))
# Q12
f <- function(x) UseMethod("f") # the generic f function
f.j <- function(x) x + 2 # the f method for class j
f.k <- function(x) x + 10
f.default <- function(x) x + 100
f.l <- function(x) x + 50
f.integer <- function(x) 100 * x
f(7L)
s3_dispatch(f(7L))
# Q13
# Note the name in UseMethod is "g" instead of "f". You should never do this.
f <- function(x) UseMethod("g")
f.j <- function(x) x + 2
g.j <- function(x) -1 * (x + 2)
f.default <- function(x) x + 100
g.default <- function(x) -1 * (x + 100)
k <- structure(10, class = "j")
f(k)
# Q14
# Note the name in UseMethod is "g" instead of "f". You should never do this.
f <- function(x) UseMethod("g")
f.j <- function(x) x + 2
g.j <- function(x) -1 * (x + 2)
f.default <- function(x) x + 100
g.default <- function(x) -1 * (x + 100)
m <- 10
f(m)

# Wednesday, Feb 7th, 2024 #
# --- R6 Object Oriented System --- #
## --- Section 1: R6 Object Oriented System --- ##
# objects are mutable meaning they are modified in place and have reference semantics
# install.packages("R6")
library(R6)
## --- Section 2: Classes and Methods --- ##
# classname is UpperCamelCase
# public is snake_case
Accumulator <- R6Class(
  classname = "Accumulator",
  public = list(
    sum = 0,
    add = function(x = 1) {
      self$sum <- self$sum + x
      invisible(self)
    }
  ))
Accumulator
# create new objects by calling the $new() method
x <- Accumulator$new()
x
x$sum
x$add() # deafult value of 1
x$sum
x$add(3)
x$sum
# invisible() means that ene though object is returned, it is not printed out
x$sum
x$add(10)$add(10)$sum # Chained methods
x$
  add(10)$
  add(10)$
  sum
# $initialize() methods will override default behavior of $new
Person <- R6Class(
  classname = "Person",
  public = list(
    name = NULL,
    age = NA,
    initialize = function(name, age = NA) {
      stopifnot(is.character(name), length(name) == 1)
      stopifnot(is.numeric(age), length(age) == 1)
      self$name <- name
      self$age <- age
    }
  ))
hadley <- Person$new("Hadley", age = "thirty-eight") # error
hadley <- Person$new("Hadley", age = 38)
hadley
# print() methods
Person <- R6Class(
  classname = "Person",
  public = list(
    name = NULL,
    age = NA,
    initialize = function(name, age = NA) {
      stopifnot(is.character(name), length(name) == 1)
      stopifnot(is.numeric(age), length(age) == 1)
      self$name <- name
      self$age <- age
    },
    print = function(...) {
      cat("Person: \n")
      cat(" Name:", self$name, "\n")
      cat(" Age:", self$age, "\n")
      invisible(self)
    }
  ))
hadley2 <- Person$new("Hadley", 38)
hadley2
# use $set() method to modify fields of an existing class
Accumulator <- R6Class("Accumulator")
Accumulator$set("public", "sum", 0)
Accumulator$set("public", "add", function(x = 1) {
  self$sum <- self$sum + x
  invisible(self)
})
AccumulatorChatty <- R6Class("AccumulatorChatty",
                             inherit = Accumulator,
                             public = list(
                               add = function(x = 1) {
                                 cat("Adding ", x, "\n", sep = "")
                                 super$add(x = x)
                               }))
x2 <- AccumulatorChatty$new()
x2$add(10)$add(1)$sum
class(hadley)
class(hadley2)
class(x2)
names(hadley) # old Person class does not have $print()
names(hadley2)
names(x2)
## --- Section 3: Reference Semantics --- ##
# objects are copied when they are modified
a <- c(1, 2)
b <- a
b[1] <- b[1] + 10
a
b
# R6 objects have reference semantics
x <- Accumulator$new()
y <- x
y$add(10)
x$sum
y$sum
# explicitly createa c opy using $clone()
x <- Accumulator$new()
y <- x$clone()
y$add(10)
x$sum
y$sum
# Ref Semantics can be tricky
x <- list(a = 1)
y <- list(b = 2)
z <- f(x, y) # error
x <- List(a = 1) # R6 "List" object
y <- List(b = 2)
z <- f(x, y)
# R6 objects should either return a value but not modify the input R6 object
# modify the input R6 object but not return a value
x <- Accumulator$new()
add5tosum <- function(accum){ # modifies x
  accum$add(5)
}
add5tosum(x) # x is modified in place. There is no assignment operator
x$sum
sumtimes10 <- function(accum){ # returns a value but does not modify
  accum$sum * 10
}
y <- sumtimes10(x) # returned values must be assigned somewhere to be used
y
## --- Section 4: Public, Private, Active --- ##
# when you define a class with R6Class(), you generally provide a list of fields and methods into the argument public
hadley2$age # we can see the public fields directly
hadley2$age <- 25 # public fields can be modified directly
hadley2$age
Person <- R6Class("Person",
                  public = list(
                    initialize = function(name, age = NA) {
                      private$name <- name
                      private$age <- age
                    },
                    print = function(...) {
                      cat("Person: \n")
                      cat(" Name: ", private$name, "\n", sep = "")
                      cat(" Age: ", private$age, "\n", sep = "")
                    }
                  ),
                  private = list(
                    age = NA,
                    name = NULL))
hadley3 <- Person$new("Hadley", age = 38)
hadley3$age # private fields are not directly visible from the outside
hadley3$age <- 25 # private fields cannot be modified directly
hadley3 # the values do exist and made visible in the $print() method
# private fields cannot be accessed and modified directly








