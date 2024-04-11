# Simple Math ----
1 + 2
'+'(1, 2) # (i.e. functional)
# handy as some libraries define new operators (e.g. %>%, %in%, %between%, etc.)

log(12)
log10(12)
sqrt(2)
2 ^ (1 / 2)
5 / 2 # division
5 %/% 2 # Quotient
5 %% 2 # Rest
# etc. etc.

# Assignment ----
a <- 15
a = 15 # Equivalent to "<-". I tolerate it but I try to avoid it
12 -> a

# Variable types ----
a <- TRUE           # Logical
b <- 3.15           # Numeric
c <- 3
c <- 3L             # Integer same as as.integer(3)
d <- "Hello"        # Character
e <- 1 + 2i         # Complex
f <- factor("Red")  # Factor

# Working Directory ----
setwd("e:/OneDriveSiemens/FH-Krems/Lectures/")
getwd()
if (rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

class(a)
str(a)
# R does not require declaring a variable. The type is inferred depending on the value. 
# DYNAMICALLY TYPED LANGUAGE

# Data types ---- Vectors, Matrices (or Array), data.frames, and lists
# Vector
v <- c(25, 3, 18)
v[1]
v[2:3]
v[c(TRUE, TRUE, FALSE)]
v > 10
v[v > 10]

# Matrix
v <- 1:6
m <- matrix(v, ncol = 3, nrow = 2)
m[1, ]
m[, 2]
m[1, 2]

# Data.frame
df <- data.frame(x = 1:3, y = c("Mark", "John", "Laura"), z = c("m", "m", "f"))
# check structure
str(df)
class(df)
# Select a column
df[, "x"]
df$x
# Select a line
df[1, ]
# filter
df[df$z == "m", ]

df$z == "m"
df[df$z == "m", ]
df[, df$z == "m"]
df[df$z == "m"]
# Line 70 and 71 are the same

# Lists
list <- list(
  Mark = c("gender" = "m", "income" = 20000),
  John = c("gender" = "m", "income" = 25000),
  Lara = c("gender" = "f", "income" = 28000))
# list can also contain different data types
list <- list(
  Period = "Quarter1",
  Orders = matrix(1:6, ncol = 3, nrow = 2),
  KPIs = list(growth = 0.2, Customers = 54))
list["Orders"] # returns a list
list[["Orders"]] # returns the raw element of the list
list[2] # can also use indexes

# If-condition ----
# if (condition) {
#   DO SOMETHING...
# } else {
#   DO SOMETHING ELSE...
# }
# ifelse(condition,{do_if_TRUE},{do_if_FALSE})
b=9
c <- ifelse(b == 10, "equal to ten", "not equal to ten")

# For-loop ----
# for(i in 1:n){
#   DO SOMETHING...
# }
#
# VERY RARELY USED IN R! More common to exploit vectorization
# e.g.
# normal_dist[normal_dist>15]
# vs.
# for (i in 1:length(normal_dist)) {
#     if(normal_dist[i]>15) print(normal_dist[i])
# }


# Libraries ----
## Loading libraries (and libraries I always use) ----
library(data.table) # Super-fast data wrangling
library(dplyr) # Tidy-R. I am not a fan, but feel free to use
require(lubridate) # Date manipulation
library(plotly) # Wonderful plots
require(caret) # ML framework
#library(mlr3) # ML framework
## Installing libraries ----
install.packages(c("mlr3", "caret"))
install.packages("caret")
??mlr3 # help about a package
?data.table # help about a function
library(help="mlr3") # Info about the package
??lubridate
vignette("lubridate") # Vignette for a package

# misc ----
ls() # list the variable in the workspace
rm(a) # Remove one variable
rm(list = ls()) # clear the workspace
gc() # calls the garbage collector


normal_dist <- rnorm(1E3, 0, 10)
unif_dist <- runif(10, 0, 10)
categories <- sample(LETTERS[1:3], size = 1E3, replace = T)
normal.dt <- data.table(x = categories, y = normal_dist, stringsAsFactors = T)
class(normal.dt)
str(normal.dt)
summary(normal.dt)
fivenum(normal.dt[, y])
head(normal.dt)
nrow(normal.dt)
ncol(normal.dt)

# *apply functions ----
lapply(normal.dt, fivenum) # apply fivenum to each column of normal.dt and returns a list
sapply(normal.dt, fivenum) # apply fivenum to each column of normal.dt and returns a data.frame
# vapply, mapply

# R-base vs. tidy-R vs. data.table----
## Example Grouping----
# R-base
aggregate(normal.dt$y, by = list(normal.dt$x), mean)
# Tidy-R
normal.dt %>% group_by(x) %>% summarise(Mean = mean(y), Median = median(y))
# Data.table
normal.dt[, .(Mean = mean(y), Median = median(y)), x] # dt[i, j, by] i.e., dt[where, select, group by]

## Example adding a column----
normal.dt <- normal.dt %>% mutate(y2 = y*2) # memcopy
normal.dt[, y2 := y * 2] # in-place addition