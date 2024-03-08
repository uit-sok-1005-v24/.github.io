# Clear the workspace of previous objects
rm(list=ls())

# Load the tidyverse package for data manipulation and visualization
library(tidyverse)

# Define a function f that squares its input
f <- function(x) {x^2}

# Test the function with a single value
f(2)

# Test the function with a vector of values
f(1:4)

# Use map to apply function f to a single value; map always returns a list
map(2, f)

# Define a vector x
x <- 1:4

# Apply the function f to each element of x using map; result is a list
# This is an example of functional programming using map from the tidyverse
map(x, f)

# Convert the list result of map into a vector using as_vector
map(x, f) %>% as_vector()

# Store the vectorized result of map in variable y
y <- map(x, f) %>% as_vector()

# Combine x and y into a two-column tibble for easy viewing
as_tibble(cbind(x,y))

# Initialize an empty numeric vector for storing results of loop
y.loop <- numeric()

# For loop to apply function f to each element of x the traditional way
for (i in 1:length(x)) {
  # Append the result of f(i) to y.loop vector
  y.loop <- c(y.loop, f(i))
}

# Display the result of applying f in a loop
y.loop

# If x changes, but still with the same length
x <- c(1,2,4,3)

# Applying map function to the new x
map(x, f)

# Creating a list containing the vector x
my_list <- list(x)

# Show the list
my_list

# Loop over a list (my_list containing x) and apply function f
for (item in my_list) {
  # Apply function f to the entire list item (here item is the entire vector x)
  y.list <- f(item)
}

# Display the result of applying f to the list (applies f to the whole vector at once)
y.list

# Q: Create a data frame of x, then add a new column with the result of applying f to x


# Two lists
z <- list(a = 1:10, b = 11:20, c = 21:30)
z

l1 <- list(x = c("a", "b", "c"), y = c("c", "d", "e"))
l1

# Apply a function to each element of a list or vector, and return a list.
map(z, sort, decreasing = TRUE)

map(l1, sort, decreasing = TRUE)

# Q: Calculate the mean of each list element in z


# Given list of names
names_list <- list(first = "Alice", second = "Bob", third = "Charlie")
names_list

# Apply toupper function to each element, expecting character output
map_chr(names_list, toupper)

# Given list of numbers
numbers_list <- list(a = 2, b = 3, c = 4)

# Apply a function to check if each number is even
map_lgl(numbers_list, ~ .x %% 2 == 0)

# Given list of messages
messages_list <- list("Hello, world!", "This is the purrr package.", "We're using walk() function.")

# Use walk to print each message
walk(messages_list, print)

# Two lists
# map2(.x, .y, .f, …) Apply a function to pairs of elements from two lists or vectors, return a list.

w <- list(1,2,3)
w
q <- list(4,5,6)
q

l2 <- list(x = "term", y = "group")
l2

# Apply a function to pairs of elements from two lists or vectors
map2_dbl(w, q, ~ .x / .y)

# or
map2_dbl(w, q, `/`)

map2_dbl(w, q, `+`)
map2_dbl(w, q, `-`)
map2_dbl(w, q, `*`)

map2_chr(l1, l2, paste,
         collapse = ",", sep = ":")

# Many lists, pmap

# Modify 
z
modify(z, ~. + 100)

modify_at(z, "b", ~.+ 100)

modify_if(z, is.numeric,~.+2)

z <- list(a = 1:10, b = 11:20, c = 21:30, x = c("a", "b", "c"), y = c("c", "d", "e"))
z

modify_if(z, is.numeric, ~. + 2)

pluck(z, "b")
z %>% pluck("b")

# Concatenate
x1 <- list(a = 1, b = 2, c = 3)
x2 <- list(
  a = data.frame(x = 1:2),
  b = data.frame(y = "a")
)

x1
x2

str(x1)
str(x2)

# Combines elements into a vector by concatenating them together
list_c(x1)

# list_rbind(x) Combines elements into a data frame by row-binding them together
list_rbind(x2)

# list_cbind(x) Combines elements into a data frame by column-binding them together
list_cbind(x2)

# list_flatten(.x) Remove a level of indexes from a list
x <- list(1, list(2, 3), list(4, list(5)))
x
x %>% list_flatten() %>% str()
x %>% list_flatten() %>% list_flatten() %>% str()

# Use list_simplify() to remove list structure
x %>% list_flatten() %>% list_flatten() %>% list_simplify() %>% str()


# Flat lists are left as is
list(1, 2, 3, 4, 5) %>% list_flatten() %>% str()

# Empty lists will disappear
list(1, list(), 2, list(3)) %>% list_flatten() %>% str()

# Another way to see this is that it reduces the depth of the list
x <- list(
  list(),
  list(list())
)
x %>% pluck_depth()
x %>% list_flatten() %>% pluck_depth()

# Use name_spec to control how inner and outer names are combined
x <- list(x = list(a = 1, b = 2), y = list(c = 1, d = 2))
x %>% list_flatten() %>% names()
x %>% list_flatten(name_spec = "{outer}") %>% names()
x %>% list_flatten(name_spec = "{inner}") %>% names()
x %>% list_flatten(name_spec = "{inner}_{outer}") %>% names()

