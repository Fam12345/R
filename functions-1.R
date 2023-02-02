######          ###     ###    ###     ###     ###


# pdf file:  Functions.pdf 
# Project: Data Science

#_________________________________________________________

# FUNCTION ###############################################

 ## Create Function_NAME_ARGUMENTS_BODY-----------
### First Attempt ----------------------------------------
# We want to write a function that rescale a vector
# so that its elements lie within  0 and 1

# We name the function "rescale01"

# The function has only one argument and it is a vector.


rescale00 <- function(v) {
  v1 <- v - min(v, na.rm = TRUE)
  d <-  max(v, na.rm = TRUE) - min(v, na.rm = TRUE)
  
   u <- v1/d
}

### Let us try to see if it works ===================

# First let us rescale vector (1, 2, 3). 
# Before we calculate it by our function,
# We know from lecture note "function.pdf" 
# we expect (0, 0.5,1)

v <- c(1, 2, 3)
u <- rescale00(v)
u

# How about (1, 2, 3, NA, 5)

w <- c(1, 2, 3, NA, 5)
u <- rescale00(w)
u

# How about (1:10, Inf)

z <- c(1:10, Inf)
(rescale00(z))

 Inf/Inf

# It seems it failed. Why? Because it is dividing by Inf
# and we know as denominator gets larer the fraction 
# approaches to zero.


#_________________________________________________________
### Second Attempt ----------------------------------------

# How to fix this?

# We need to make sure Inf (or -Inf) 
# does not return as max (or min)
# How? we use range function. 


# we define 
# ___   rng <- range(v, na.rm = TRUE, finite = TRUE)

# The argument finite = TRUE remove the Inf.

# range function returns a vector as (min, max) 

# now _____min is rng[1] ____ max is rng[2] __________
# Thus no need to use min and max function any more!!!


# Second Attempt
rescale01 <- function(v) {

  rng <- range(v, na.rm = TRUE, finite = TRUE) 

  (v - rng[1])/(rng[2] - rng[1])
}



# Let us try to see if error has been solved

z <- c(1:10, Inf)
(rescale01(z))


#### EXAMPLE IN THE BOOK ##########################

# If you notice that we haven't install and load any
# package for this session. 
# Here we would like to create a tibble (or data frame),
# But tibble is in tibble package and we have not have it
# here.
# ______________  package::function
# The command :: helps us to get exact function from
# one specific package

df <- tibble::tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10)
)

View(df)

# What does the $ sign do? It helps you to access
# one variable in your data. For instance
#_______ df$a___ Allows you to have access to only column a.
 ####### Try it
df$a 

# Remember we would like to rescale a, b ,c and d values

# If you want rescale a we do as follow 

df$a <- (df$a - min(df$a, na.rm = TRUE)) / 
  (max(df$a, na.rm = TRUE) - min(df$a, na.rm = TRUE))
df$a

# If we want to do this for b, c and d, 
# then we must write as follow

(df$b <- (df$b - min(df$b, na.rm = TRUE)) / 
  (max(df$b, na.rm = TRUE) - min(df$a, na.rm = TRUE))) #

(df$c <- (df$c - min(df$c, na.rm = TRUE)) / 
  (max(df$c, na.rm = TRUE) - min(df$c, na.rm = TRUE)))

(df$d <- (df$d - min(df$d, na.rm = TRUE)) / 
  (max(df$d, na.rm = TRUE) - min(df$d, na.rm = TRUE)))

# We are repeating same set of operation again and again
# And it is time consuming, and also we may make a mistake
# And the worst part is we may never notice that an
# error was there. Since we didn't get any error message.
# Yes, there is an error/typo for df$b
# in the second line inside of min() we should have 
# df$b, instead of df$a.



(df$b <- (df$b - min(df$b, na.rm = TRUE)) / 
    (max(df$b, na.rm = TRUE) - min(df$a, na.rm = TRUE))) #



# With the function that we have written,
# then we could easily do as follow

(df$a <- rescale01(df$a)  )
(df$b <- rescale01(df$b)  )
(df$c <- rescale01(df$c)  )
(df$d <- rescale01(df$d)  )

#_______________________________________________________
### EXERCISES #############################################

# is TRUE not a parameter to rescale01()? 

# The na.rm and finite both are inside of function and given
# as TRUE__________na.rm __ NA ReMove_________

#______ The default is FALSE
rescale01 <- function(v) {
  
  rng <- range(v, na.rm = TRUE, finite = TRUE) 
  
  (v - rng[1])/(rng[2] - rng[1])
}

x <- c(1:4, NA)
(rescale01(x))  # Use the default




# What would happen if x contained a single missing value,
# and na.rm was FALSE?

#______ The default is FALSE
rescale011 <- function(v, na.rm = FALSE) {
  
  rng <- range(v, na.rm = na.rm, finite = TRUE) 
  
  (v - rng[1])/(rng[2] - rng[1])
}

x <- c(1:4, NA)
(rescale011(x))  # Use the default
           # function still return the missing value

(rescale011(x, TRUE))
          # function still return the missing value

#_______ ___________________

# If add both argument in or function-- na.rm and finite

rescale012 <- function(v, na.rm = FALSE, finite = FALSE) {
rng <- range(v, na.rm = na.rm, finite = finite) 

(v - rng[1])/(rng[2] - rng[1])
}

x <- c(1:4, NA)

range(x)
(rescale012(x))  # Use the default --Both FALSE
# If finite is FALSE then range willnot drop non-finite
# so we get NAs


NA * 5
NA + 5
NA - 5
NA / 5
NA  + NA



(rescale012(x, finite = TRUE, na.rm = FALSE))

# If finite is TRUE then range will drop all non-finite
# NA is a non-finite


####_________________________NOTE ____________________###
# Arithmetic operation involve NA will return NA
# That is why you see NA, Even if we set to remove it.

#_______________________________________________________ 


# In the second variant of rescale01( ), infinite values
# are left unchanged. Rewrite rescale01( )
# so that -Inf is mapped to 0, and Inf is mapped to 1.

rescale01.2 <- function(v) {
  
  rng <- range(v, na.rm = TRUE, finite = T) 
  
  u <- (v - rng[1])/(rng[2] - rng[1])
  u
}

x <- c(1:5, Inf, NA, -Inf)
(rescale01.2(x))


# This is not what we are lookingfor

rescale01.2 <- function(v) {
  
  rng <- range(v, na.rm = TRUE, finite = TRUE) 
  
   u <- (v - rng[1])/(rng[2] - rng[1])
                      
                     u[u == Inf] <- 1
                     u[u == -Inf] <- 0
                     
                     
}

x <- c(1:5, Inf, NA, -Inf)
(rescale01.2(x))

# Can you tell me why we get result only 0?

# Remember function returns the last line. The last line is 
# U[....] is zero
# 
# We want to return u

rescale01.2 <- function(v) {
  
  rng <- range(v, na.rm = TRUE, finite = TRUE) 
  
  u <- (v - rng[1])/(rng[2] - rng[1])
  
  u[u == Inf] <- 1
  u[u == -Inf] <- 0
  u
  
}

x <- c(0,1:5, Inf, NA, -Inf)
(rescale01.2(x))


#________________________________________________________

# Practice turning the following code snippets into 
# functions. Think about what each function does. 

x
is.na(x)

mean(is.na(x))   

sum(x, na.rm = TRUE)

x / sum(x, na.rm = TRUE)

sd(x, na.rm = TRUE) / mean(x, na.rm = TRUE)
# Coefficient of Variation (CV). It is used to compare the
# degree of variation from one data series to another, 
# even if the means are drastically different from one another.

# ______________is.na(x)________
# This function look at elements of vector x. 
#____ If its value is NA then it returns TRUE
#____ Otherwise it returns FALSE

x <- c(1, 0, NA, Inf)

is.na(x)

# What do you get from following code

x[is.na(x)]

#__________ 
!is.na(x)

# What do you get from following code

x[!is.na(x)]

#____________________

x
is.infinite(x)

# What do you get from following code

x[is.infinite(x)]

#__________ 
!is.infinite(x)

# What do you get from following code

x[!is.infinite(x)]

#____________________
v

is.infinite(v)

# What do you get from following code

x[is.infinite(x)]

#__________ 
!is.infinite(x)

# What do you get from following code

x[!is.infinite(x)]


# ______________mean(x)________
# This function return the mean of object x. But 
# The vlaue of x must be numeric. If there is a logical 
# Value is there it will turn to integer
# TRUE becomes 1
# FALSE becomes 0
x <- c(1, 0, NA, Inf)
(y <- is.na(x)) # returns y equal to (F, F, T, F)
 mean(y)  # will find mean of (0, 0, 1, 0) which is 1/4=0.25

 # ______________sum(x)________
 # calculate the sum of all elements in x. 
# If there is NA it will return NA unless we remove it.
 x
 sum(x)
 sum(x, na.rm = T)
# we also need to remove Inf, but there is no finite argument
# So If there is Inf in data we should remove it because 
 # we know the result of adding with Inf it is always Inf.
 
 # ______________sd(x)________
# Will find the standard deviation
 sd(x, na.rm = T)
  # Inf it is not a finite number and it should be remove
 
 
 
 
 
 
# What would you call it? 
 # It rescale vector x to vector whose values are between 
 #  0 and 1________ x/sum______ rescale
 rescale <- function(v, na.rm = FALSE)
 {
   v <- v[!is.infinite(v)] # Remove infinite values
   
   u <- v/sum(v, na.rm = na.rm) 
 }
 x <- c(1, 0, NA, Inf, -Inf)   
 (rescale(x, TRUE)) 
 
 # It also
 #  divides____________ sd/mean. 
 # So it is "The Coefficient of Variation". 
 # We don't know nothing about the data x
 # CV will give us the relative magnitude of SD.____cov
 
 # cov is short for Coefficient of Variation
 
 cov <- function(v)
 {
   
   v <- v[!is.na(v)  & !is.infinite(v)] # This code remove
                                        #NA and Inf
   
   u <- sd(v)/mean(v)
          
 }
 
 x <- c(1, 0, NA, Inf, -Inf)   
(cov(x))


# How many arguments does it need? 
 # We need two arguments. We must remove infinite from
 # our vectors
 
# Can you rewrite it to be more expressive or 
# less duplicative?

 cov.rescale <- function(v, na.rm = FALSE)
 {
   # w <- v[!is.na(v)  & !is.infinite(v)] # This code remove
   #NA and Inf
   
   v <- v[!is.infinite(v)] # Remove infinite values
   
   u <- c(sd(v, na.rm = na.rm)/mean(v, na.rm = na.rm),
          v/sum(v, na.rm = na.rm)) # First element is cov and
   # The rest is vector V rescales 0 to 1
   # Note: cov is short for Coefficient of Variation
 }
 x <- c(1, 0, NA, Inf, -Inf)   
 cov.rescale(x, T) -> u
 u[1]
 
 
 sd(c(1,0,NA), na.rm = T)/mean(c(1,0,NA), na.rm = T)
 
 sd(c(1,0))/mean(c(1,0))
 
 
 y <- c(10, 5, 1, NA, Inf, -Inf) 
 
 (cov.rescale(y, T))
 
 # Without the function
 sd(y, na.rm = T)/mean(y, na.rm = T)
 y1 <- y[!is.infinite(y)]
 sd(y1, na.rm = T)/mean(y1, na.rm = T)  # Coefficient of Variation
 y1/sum(y1, na.rm = T) # rescales 0 to 1
#_____________________________________________________

# write your own functions to compute the variance and 
# skewness of a numeric vector.
 
 variance.0 <- function(v, na.rm = FALSE)
 {
  
   n <- length(v)
   m <- mean(v, na.rm = na.rm)
   d.sq <- (v - m)^2
   ( sum(d.sq, na.rm = na.rm)/(n-1))
   
 }
 x <-  c(1:10)
 variance.0(x)
 var(x)
 
 x <-  c(NA, 1:10)
 var(x)
 variance.0(x, T)
 var(x, na.rm = T)
 

 #### My Function failed. Why?
 
 #### Need to be  fixed
 
 variance.1 <- function(v, na.rm = FALSE) 
   {
   w <- v[!is.na(v)] # We do not want to count NA
   n <- length(w)
   m <- mean(v, na.rm = na.rm)
   d.sq <- (v - m)^2
   ( sum(d.sq, na.rm = na.rm)/(n-1))
 
 }
 x <-  c(1:10)
variance.1(x)
var(x)

x <-  c(NA, 1:10)
var(x)
variance.1(x, T)
var(x, na.rm = T)

#
variance.1_1 <- function(v, na.rm = FALSE) 
{
  w <- v[!is.na(v)] # We do not want to count NA
  n <- length(w)
  m <- mean(w, na.rm = na.rm)
  d.sq <- (w - m)^2
  ( sum(d.sq, na.rm = na.rm)/(n-1))
  
}
x <-  c(NA, 1:10)
var(x)
variance.1_1(x, T)
var(x, na.rm = T)





#

variance.2 <- function(v)
{
  v <- v[!is.na(v)] # We need to remove NA. We do not want 
                    # it in our calculation
  n <- length(v)
  m <- mean(v)
  d.sq <- (v - m)^2
  ( sum(d.sq)/(n-1))
  
}
x <-  c(1:10)
variance.2(x)
var(x)

x <-  c(NA, 1:10)
variance.2(x)
var(x, na.rm = T)

#_____________SKEW______________

# sum((x-mu)^3)/(n-2) / var(x)^(3/2)



skew <- function(v, na.rm = FALSE)
{
  w <- v[!is.na(v)] # We do not want to count NA
  
  n <- length(w)
  m <- mean(v, na.rm = na.rm)
  d.cub <- (v - m)^3
  num <- sum(d.cub, na.rm = na.rm)/(n-2)
  #dnum <- variance.1(v, na.rm = na.rm)
  dnum <- var(v, na.rm = na.rm)
  
    (num / dnum^(3/2))
  
}
x <-  c(1, 2, 5, 100, NA)
skew(x, na.rm = T)

x <-  c(NA, 1:10)
skew(x, na.rm = T)

# Skewnss refers to DISTORTION that deviates from normal dist.
# If the skewness betwen -0.5 and 0.5 data is approximately symmetric.
# less than -1 or greater than 1 highly skewed
# Of course there are more to this.

#_______________________________________________________

# Write both_na( ), a function that takes two vectors
# of the same length and returns the number of positions
# that have an NA in both vectors.

  # Note the sum function Coerced variables from 
  # logic to integer T = 1 and F = 0
  #
  # we know T & T = T 
  # Otherwise the # operation for to logic is F

    
# Consider v <- c(NA, 12, NA, 0) and W <- c(1, 12, NA, 0)
# They have NA in third element so the function should
 # Return 1

both_na <- function(v, w) {
  sum(is.na(v) & is.na(w))
}

x <- c(NA, 12, NA, 0) 
y <- c(5, 12, NA, 0)
both_na(x, y)

# What does (is.na(x) & is.na(y)) do?
 (is.na(x) & is.na(y))
# What does sum(is.na(x) & is.na(y)) do? and why?



x1 <- c(NA, 12, NA, 0) 
y1 <- c(NA, 12, NA, 0)
both_na(x1, y1)

# What does (is.na(x1) & is.na(y1)) do?

x <- c(NA, 12, NA, 0) 
y <- c(1, 12, 0, NA)
both_na(x, y)


x <- c(NA, 12, NA, 0) 
y <- c(1, 12, 0, NA, 1)
both_na(x, y)   # The error says not of the same length


x <- c(NA, 12, NA, 0, 1, NA, 7) 
y <- c(NA, 12, NA, NA, 10, NA, 5) 
both_na(x, y)


#______________________________________________________________

## What do the following functions do? Why are they useful 
## even though they are so short?

# is_directory <- function(x) file.info(x)$isdir

#    # The function is_directory() checks whether the path in
# # x is a directory.

#is_readable <- function(x) file.access(x, 4) == 0

#   # The function is_readable() checks whether the path in 
# # x is readable, meaning that the file exists and the user
# # has permission to open it.

# # These functions are useful even though they are short
# # because their names make it much clearer what the code is doing.


#___________________________________________________________

## Functions are for humans and computers #############

# Use a name for your function that is short,
# but clearly evoke what the function does.


### Create Headers --------------------------------------



# To create Headres use the shortcut  ctrl+shift+R
# Use the shortcut and and a box will open 
# Write anything you Like inside it and then click okay

 ----------------------------------------
  

### EXERCISES -------------------------------------------
# Read the source code for each of the following three 
# functions, puzzle out what they do, and 
# then brainstorm better names.

f0 <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
}

# We get an error for f0, but there is no error for f1. Why?

f1 <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
}

# Is there any differences between these two functions?
# the first function called f0, if you change it to f1
# still not working. What is the problem?
# There is no issue with with f0.In deed f0 and f1 are 
# exactly the same. So, you need search for error
# before we define f0, Did you find it?
# We have a line with dashes
# The line has no hashtage, R consider that line as 
# a command, remove the lne of put hashtag before the dashes.

f1("Hamid Semiyari", "Hamid S")

# Let's see how it works

substr("Hamid Semiyari", start = 1, stop = 7)

substr("Hamid Semiyari", start = 1, stop = 7)=="Hamid S"

# ___ QUESTION ___
# _____________How code is working ____________

f1 <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix
  # nchar(prefix) tells howmany charcters is in prefix
            #  Let us assume k characters is in prefix
  # substr(string, 1, nchar(prefix)) Tells to print
            # charcters 1 to k from string
  # substr(string, 1, nchar(prefix)) == prefix
            # if the characters 1 to k of string is same as 
            # prefix the code return TRUE. Otherwise FALSE
  
}

# Let us breake it down
#___________________ ONE
test_f1_1 <- function(string, prefix) {
nchar(prefix)
  # nchar(prefix) tells howmany charcters is in prefix
  #  Let us assume k characters is in prefix
 }

x <- ("BEST CLASS EVER. Is IT TRUE?")
test_f1_1(x, "BEST")


test_f1_1(x, "VER")


#___________

nchar("VER")
#___________________ TWO

test_f1_2 <- function(string, prefix) {
  substr(string, 1, nchar(prefix))
 
  # substr(string, 1, nchar(prefix)) Tells to print
  # charcters 1 to k from string
 }

x <- ("BEST CLASS EVER. Is IT TRUE?")
test_f1_2(x, "BEST")


test_f1_2(x, "VER") # What would you think it will return?

#___________________ THREE

test_f1_3 <- function(string, prefix) {
  substr(string, 1, nchar(prefix)) == prefix

  # substr(string, 1, nchar(prefix)) == prefix
  # if the characters 1 to k of string is same as 
  # prefix the code return TRUE. Otherwise FALSE
  
}

x <- ("BEST CLASS EVER. Is IT TRUE?")
test_f1_3(x, "BEST")


test_f1_3(x, "VER") # What would you think it will return?

# ____ QUESTION______
####_________________________What does substr() do?

?substr

#__ substr(x, start=i, stop=k)_____
# The string from ith letter to kth letter extracted

substr("This is a Test", 4, 12)

substr("This is a Test", 12, 4)

substr("This is a Test", 4, 1)

substr("This is a Test", 12, 14)

substr("This is a Test", 12, 17)

#______________END of Question__________________



c1 <- c("abc")
c2 <- c("as", "df", "gh", "ag")

f1(c1, "a")
f1(c1, "b")
f1(c1, "bc")
f1(c2, "a")
f1(c2, "ab")
f1(c2, "asb")
f1(c2, "df")

# The function look at the string. If the string starts with 
#the given prefix, then it returns TRUE other wise FALSE.

# A better namecode be
# ____ prefix____or ____ has_prefix_____ or ...
#_____________________________________________________
x<- c(1, 200, 3.4, 11, 2.4, 56, 0.98, 100, 200, pi)
y<- c(10, 20, 30, 40, 50, 60, 1, 2, 3, 4)
f2 <- function(x) {
  if (length(x) <= 1) return(NULL)
  x[-length(x)]
}

f2(x)
f2(c(10,20, 30, 40, 50, 60))
f2(y)
f2(c2)
f2(c1)



# What will return   x[-5]


# This function take a vector and drop the last entries

# _____ drop_last _____ remove_last ____ ...



# What does x[-length(x)] do?
-length(x)
x[-length(x)]

#_____________________________________________________
f3 <- function(x, y) {
  rep(y, length.out = length(x))
}


# ____ QUESTION______
####_________________________What does rep() do?

x

x1 <- rep(x)
x1

x2 <- rep.int(x, times = 2)
x2


x3 <- rep_len(x, length.out = 6)
x3

#______________END of Question__________________

x
y
f3(x,y)
f3(y, x)


c1
c2
f3(c1, c2)
f3(c2, c1)

# It look at the first vector and then recreate the
# second vector as the same length of the first one.
# If the first vector is larger then it drops the extra 
# entries,
# if it is shorter, then it repeats its entries in oredr

# A better name could be
#__________ recycle ___ repeat ___ same_length____SameLength

#__________________________________________________________

# Compare and contrast rnorm() and MASS::mvrnorm(). 
# How could you make them more consistent?

?rnorm
?mvrnorm  # it says no documentaion
?MASS::mvrnorm

# If you look at the R documentation for these two, you'll see
# rnorm is uni-variate normal distribution
# mvnorm provide sample from multivaiate normal distribution


# The main argumentin 
#____________________ rnorm
#____ n (sample size), mean, sd (standard deviation)

#____________________ mvrnorm
#____ n (sample size), mu(mean), sigma (standard deviation
 # Their arguments should have the same name.
# Standard deviation does not make sense in multivariate
# because we use matrix Sigma (Capital case)
# But as you may remember, we call (population)
# standard deviation in univariate normal distribution also
# sigma (lower case)


#__________________________________________________________

# Make a case for why norm_r(), norm_d() etc would be
# better than rnorm(), dnorm(). Make a case for the opposite.

?rnorm
?dnorm

?rbinom
?dbinom

?runif
?dunif

help(rexp)
?dexp

# If named norm_r() and norm_d(), the naming convention 
# groups functions by their distribution.

rnorm(5)
dnorm(0)
pnorm(1)
qnorm(0.84)

# If named rnorm(), and dnorm(), the naming convention 
# groups functions by the action they perform.

# r* functions always sample from distributions: 
# for example, rnorm(), rbinom(), runif(), and rexp().

# d* functions calculate the probability density or 
# mass of a distribution: 
# For example, dnorm(), dbinom(), dunif(), and dexp().

# R distributions use this latter naming convention.

#__________________________________________________________


## Conditional Execution --------------------------------

?if  # Not working. Use __BACKTICCKS
?`if`

has_name <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    rep(FALSE, length(x))
  } else {
    !is.na(nms) & nms != ""
  }
}



?names  # gets or set the name of an object.
#______________________
v <- c(1:5)
v
names(v) <- letters[1:5]     # Assign names to vector
v


is.null(v)  # False
!is.na(v) & v != ""


has_name(v)

#______________________
x1<- c(1:8)

N <- names(x1)
N

is.null(N)  # True
rep(FALSE, length(x1))




has_name(x1)


#_________________________


x2 <- c(10)
N <- names(x2)
N

is.null(N)  # True
rep(FALSE, length(x2))




has_name(x2)

#_____________________

x3 <- c("")
length(x3)
N <- names(x3)
N

is.null(N)  # True
rep(FALSE, length(x3))

has_name(x3)





x3 != ""
!is.na(x3)

x4 <- c(NA)
x4
has_name(x4)
x4 != ""
!is.na(x4)


# Let us creat a vector and assigning name to its elements
class <- c("412", "612")
names(class) <- c("underdraduate", "Graduate")

names(class)

print(class)

has_name(class)

### Conditions ==========================================

#______ NOTE
# Difference between = and ==
test <- 5*2 # test = 5*2, set x to be equal to 5*10

test == 10  # Asking, is the x =10?

# Which one to use & or &&?
# Use &&. why?
   # If the first statement is FALSE, then regardless 
# of the second statement the result is FALSE.
# So, we really do not need to evaluate second statement.
# && do not evaluate the second statement if the first is F
# Thus, it make computation a little faster

F & T
F && T


c(F, T, T, F) & c(T, T, F, T)
c(F, T, T, F) && c(T, T, F, T)

c(T, T, T, T, T, T) & c(T, T, F, T, T, T)
c(T, T, T, T, T, T) && c(T, T, F, T, T, T) # First is TRUE

c(F, T, T, T, T) & c(T, T, T, T, T)
c(F, T, T, T, T) && c(T, T, T, T, T) # The first is FALSE

# How about | and ||?
F | T
F || T


c(F, T, T, F) | c(T, T, F, F)
c(F, T, T, F) || c(T, T, F, F)

c(F, T, T, T, T, T) | c(T, T, F, T, T, T)
c(F, T, T, T, T, T) || c(T, T, F, T, T, T) # First is TRUE

c(F, T, T, T, T) & c(F, T, T, T, T)
c(F, T, T, F, T) && c(F, T, F, T, T) # The first is FALSE

#____ When to use any() and all()?
#  check for NA values, or to examine a vector of 
# logical values

?all
?any

set.seed(2)
x <- rnorm(7)
x
x1 <- round(x, 2)
x2 <- sort(x1)
x2
y <- range(x2)
y

#Or we could say
set.seed(2)
y <- range(x <- sort(round(stats::rnorm(7), 2)) )
y

any(y >2)
any(y==2)
any(y<=2)

any(is.na(y))

all(y > 0.5)
all(y<1.59)
all(y<=1.59)
#________
?sample 
z <- sample(c(TRUE, FALSE), size = 3, replace = TRUE)
      # makes a sample of size n=3, of the x=(TRUE, FALSE)
      # With replacement
z
any(z)  # any value TRUE?
all(z)  # all values TRUE?

z1 <- c(T,T,T)
z1

# z1 could be created by rep()
z1_alt <- rep(TRUE, 3)
z1_alt

any(z1_alt)
all(z1_alt)


z2 <- rep(F, 3)
z2
any(z2)
all(z2)

z3 <- c(rep(T, 2), F)
z3
any(z3)
all(z3)


z4 <- c(0, 1, 2)
z4
any(z4)  
all(z4)


z5 <- c("")
z5
any(z5)
all(z5)

z6 <- c(NA)
z6
any(z6)
all(z6)

# This one might be confusing!!! But see the explanation
z7 <- c()   # A vector of zero element
z7
    # ALL of ZERO members/values could be TRUE
all(z7)
    # But there are NOT ANY TRUE values/members     
any(z7)

#____________ near()

x <- sqrt(2)
x
x^2 == 2

near(x^2, 2)   # Not working. Why? We did not load 
               # any packages. we need dplyr package

dplyr::near(x^2, 2)

#______ identical()

x <- 0   # Double precision floating point number/numeric
x

y <- 0L  # integer (or single precision. But R  base does not 
             # single precision type) 
y

identical(x, y)  # 
             # Why decimal called double precision? because
             # it has two parts. Whole and decimal parts
#_______________________________________________________

    ### Multiple Conditions ============================
TempC <- function(temp){

if(temp <= 0) {
  print("Freezing")
} else if (temp <= 10) {
  "Cold"
} else if (temp <= 20) {
  "Cool"
} else if (temp <= 30) {
  "Warm"
} else{
  "Hot"
}
}

TempC(-5)
#_____ Sometimes is easier to use switch()
# instead of multiple conditions
?switch

operation <- function(x, y, op){
  switch(op,
         "plus" = x + y,
         "minus" = x - y,
         "times" = x * y,
         "divide" = x / y,
         stop("unkown op!")
         )
}

operation(1, 2, "plus")
operation(5, 2, "divide")
operation(1, 4, "exp")
operation(1, 4, "times")



### Code Style ==========================================




### EXERCISES ==========================================

#What is the difference between if and ifelse()?
#Carefully read the help and construct three examples that illustrate the key differences.

?`if`
# If statement is tells R to decide whether a statement or
# block of statements will be used or not.

x<- 2

if ( x == 0 ){
  "Undefined"
} else {
  1/x
}
  

#____________________
?ifelse
# Test each element

x <- c(-1, -0.5, 0, 0.5, 1 )

ifelse(x !=0, 1/x, NaN)


# or ab you may write as follow
1/(ifelse(x!=0, x, NaN))

#__________________________________________________________

# Write a greeting function that says is a good morning,
# good afternoon, or good evening, depending 
# on the time of day. (Hint: use a time argument that 
# defaults to lubridate::now(). That will make it easier
# to test your function.)

?lubridate

greeting <- function(t) {
  g <- lubridate::hour(t)
  if (g < 12) {
    print("good morning")
  } else if (g  < 16) {
    print("good afternoon")
  } else if (g < 19) {
    print("good evening")
  } else {
    print("good night")
  }
}

t <- lubridate::now()
t
greeting(t)

greeting("2022-01-19 10:25:10")

# If you messed up with the denting, you may want to
# ctrl+shift + A


# You would like to know, how many days or weeks you are old


your_age <- function(age, dw){
  
if ( dw == "weeks"){
  
  print("You have selected weekly")
  difftime(Sys.time(), age, units = "weeks")
  
} else {
  
  print("You have selected daily")
  difftime(Sys.time(), age, units = "days")
}
}

# The above code is not dented well. Highlight it and then
# ctrl+shift + A

age <- as.Date("1966-08-23")
your_age(age, "weeks")


s <- strptime("August 23 1966", "%B %d %Y")
s

your_age(s, "day")








#__________________________________________________________

#Implement a fizzbuzz function. It takes a single number
# as input. If the number is divisible by three, 
# it returns “fizz”. If it's divisible by five 
# it returns “buzz”. If it's divisible by three and 
# five, it returns fizzbuzz. Otherwise, 
# it returns the number. Make sure you first write 
# working code before you create the function.

fizz <- function(x){
  if (x %% 3 == 0 && x %% 5 == 0 ) {
    print("fizzbuzz")
  } else if (x %% 5 == 0 ) {
    print("buzz")
  } else if (x %% 3 == 0 ) {
    print("fizz")
  } else {
    x
  }
}

fizz(30)
fizz(25)
fizz(27)
fizz(13)
fizz(0)


#______OR

fizzbuzz <- function(x) {
  # these two lines check that x is a valid input
  stopifnot(length(x) == 1)          # x is a single 
  stopifnot(is.numeric(x))           # and x is a number
  if (!(x %% 3) && !(x %% 5)) {
    "fizzbuzz"
  } else if (!(x %% 3)) {
    "fizz"
  } else if (!(x %% 5)) {
    "buzz"
  } else {
    # ensure that the function returns a character vector
    as.character(x)
  }
}

fizzbuzz(15)
fizzbuzz(35)
fizzbuzz(33)
fizzbuzz(17)
fizzbuzz(0)

#______OR
# This function can be slightly improved by combining 
# the first two lines conditions so we only check 
# whether x is divisible by 3 once.

fizzbuzz2 <- function(x) {
  # these two lines check that x is a valid input
  stopifnot(length(x) == 1)
  stopifnot(is.numeric(x))
  if (!(x %% 3)) {
    if (!(x %% 5)) {
      "fizzbuzz"
    } else {
      "fizz"
    }
  } else if (!(x %% 5)) {
    "buzz"
  } else {
    # ensure that the function returns a character vector
    as.character(x)
  }
}
fizzbuzz2(9) 
fizzbuzz2(19) 
fizzbuzz2(45) 
fizzbuzz2(0)

#______OR
# Instead of only accepting one number as an input, 
# we could a FizzBuzz function that works on a vector. 
# The case_when() function vectorizes multiple if-else 
# conditions, so is perfect for this task. 
# In fact, fizz-buzz is used in the examples in the 
# documentation of case_when().

fizzbuzz_vec <- function(x) {
  dplyr::case_when(!(x %% 3) & !(x %% 5) ~ "fizzbuzz",
            !(x %% 3) ~ "fizz",
            !(x %% 5) ~ "buzz",
            TRUE ~ as.character(x)
  )
}
fizzbuzz_vec(c(0, 1, 2, 3, 5, 9, 10, 12, 15))

#______OR
#The following function is an example of a vectorized
# FizzBuzz function that only uses bracket assignment.

fizzbuzz_vec2 <- function(x) {
  y <- as.character(x)
  # put the individual cases first - any elements divisible by both 3 and 5
  # will be overwritten with fizzbuzz later
  y[!(x %% 3)] <- "fizz"
  y[!(x %% 3)] <- "buzz"
  y[!(x %% 3) & !(x %% 5)] <- "fizzbuzz"
  y
}

fizzbuzz_vec2(c(0, 1, 2, 3, 5, 9, 10, 12, 15))

# This question, called the “Fizz Buzz” question,
# is a common programming interview question used for
# screening out programmers who can’t program.
#__________________________________________________________

#How could you use cut() to simplify this set of nested
# if-else statements

# temp <- 35

if (temp <= 0) {
  "freezing"
} else if (temp <= 10) {
  "cold"
} else if (temp <= 20) {
  "cool"
} else if (temp <= 30) {
  "warm"
} else {
  "hot"
}    


?cut


#  How would you change the call to cut() 
# What is the other chief 
# advantage of cut() for this problem? 
# (Hint: what happens if you have many values in temp?)

#

temp <- seq(-10, 50, by = 5)

cut(temp, c(-Inf, 0, 10, 20, 30, Inf),
    right = TRUE,
    labels = c("freezing", "cold", "cool", "warm", "hot")
)

#__OR

 temp <- c(23, 0, 15, 45, -15)

 cut(temp, breaks = c(-Inf, 0, 10, 20, 30, Inf),
     labels = c("freezing", "cold", "cool", "warm", "hot"))
                      # By default interval is closed 
                      # If x = 0, then it is in Freezing
 
 cut(temp, breaks = c(-Inf, 0, 10, 20, 30, Inf),
     labels = c("freezing", "cold", "cool", "warm", "hot"),
     right = FALSE)   # right = FALSE means interval is not 
                      # Open. If x =0, then it is in cold
 
 
 


#__________________________________________________________

# What happens if you use switch() with numeric values?
switch(x, 
       a = ,
       a = "",
       b = "ab",
       c = "Hi",
       d = "cd"
)   
  


switch(4, 
       a = "Me",
       b = "!",
       c = "Hi",
       d = "It's"
) 
switch(1, 
       a = "Me",
       b = "!",
       c = "Hi",
       d = "It's"
) 
switch(2, 
       a = "Me",
       b = "!",
       c = "Hi",
       d = "It's"
) 
 
switch(3, 
       a = "Me",
       b = "!",
       c = "Hi",
       d = "It's"
)  

#_OR 
switch(4.9, 
       a = "Me",
       b = "!",
       c = "Hi",
       d = "It's"
) 
switch(1.2, 
       a = "Me",
       b = "!",
       c = "Hi",
       d = "It's"
) 
switch(2.075, 
       a = "Me",
       b = "!",
       c = "Hi",
       d = "It's"
) 

switch(3.99, 
       a = "Me",
       b = "!",
       c = "Hi",
       d = "It's"
)   


# Note that switch() truncates the numeric value, 
# it does not round to the nearest integer. 
# While it is possible to use non-integer numbers 
# with switch(), you should avoid it
#__________________________________________________________


## Function Arguments ----------------------------------
?log

?mean

# What does trim do? mean( x, trim = 0.1)
# It tells R to remove the data entries  that is on lower 
# 0.1/2 = 0.05 = 5% and above 5%. Why for purpose of outlier
# Why? we know outlier adersly affect mean!

x <- c(0:10, 50)
x
mean(x)  # the result is 8.75

# We are finding the percentiles for x
quantile(x, probs = seq(.05, .95, by = .05
))

# It shows 0 is lower 5% and 50 is above 5%. se we are 
# removing these two
# thus newx is (1:10)

newx <- c(1:10)
mean(newx)      # The result is 5.5

# Now let us check

xm <- mean(x)   # mean without trim- Result 8.75


xmt <- mean(x, trim = 0.10)  # mean with trim- Result 5.5


#___________
# The argument is confidence and its default value is 95%. 
# Why 95%? becaus ethis is the most common value for
# the confidence interval

# Compute confidence interval around mean using normal approximation
mean_ci <- function(x, conf = 0.95) {
  se <- sd(x) / sqrt(length(x))
  alpha <- 1 - conf
  mean(x) + se * qnorm(c(alpha / 2, 1 - alpha / 2))
}


x <- runif(100)  # uniform distribution
mean_ci(x)

# What does qnorm( ) do?
qnorm(0.05/2)  # zvalue when area to the left is alpha/2
qnorm(1-0.05/2) #zvalue when area to the left is 1-alpha/2

### Choosing Names =====================================

# See Functions.pdf


### Checking Values ====================================

# To find the weight of average bot vector data and weight
# must be of the same size. However, if they are not 
# the same size, R does not retuen an error. Why? 
# because of vector recycling rules

wt_mean <- function(x, w) {
  sum(x * w) / sum(w)
}

x <- c(100, 90, 95)
w <- c(30, 50, 20)
wt_mean(x, w)

________

wt_mean(1:6, 1:3)  # x = (1,2,3,4,5,6) and w =(1,2,3)
                # No error, but it should return an error
________

x1 <- c(1:6)
w1 <- c(1:3)
wt_mean(x1, w1)  # No error, but it should return an error

________

x2 <- c(100, 90)
w2 <- c(30, 50, 20)

wt_mean(x2, w2)  # this return the error


#_____   USE stop()

wt_mean_s <- function(x, w) {
  if (length(x) != length(w)) {
    stop("`x` and `w` must be the same length", call. = FALSE)
  }
  sum(w * x) / sum(w)
}

wt_mean_s(x1, w1)   # Returns an error

#_______   USE stopifnot()-- what should be true rather 
# than checking for what might be wrong

wt_mean_si <- function(x, w, na.rm = FALSE) {
  stopifnot(is.logical(na.rm), length(na.rm) == 1)
  stopifnot(length(x) == length(w))
  
  if (na.rm) {
    miss <- is.na(x) | is.na(w)
    x <- x[!miss]
    w <- w[!miss]
  }
  sum(w * x) / sum(w)
}

  # length(na.rm) ==1, checks if na.rm is avector of size 1. 

wt_mean_si(1:6, 1:3, na.rm = TRUE)

wt_mean_si(1:6, 6:1, na.rm =T)# There is no NA to be removed 

wt_mean_si(1:6, 6:1, na.rm =FALSE)

wt_mean_si(1:6, 6:1, na.rm = "foo")





### Dot-Dot-Dot ========================================
# Many functions in R take an arbitrary number of inputs:
sum(1:10)
sum(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
stringr::str_c("a", "b", "c", "d")

# They rely on a special argument...
# This special argument captures any number of arguments
# that aren’t otherwise matched.

#This is a useful catch-all if your function primarily 
# wraps another function. For example,

commas <- function(...) stringr::str_c(..., collapse = ", ")

commas(letters[1:10])
commas(LETTERS[1:10])

#____________
letters[1:10]
LETTERS[1:10]
#____________

rule <- function(..., pad = "-") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  cat(title, " ", stringr::str_dup(pad, width), "\n", sep = "")
}


rule("Important output")

# The technique has some issue, any misspelled arguments will
# not raise an error.

x <- c(1, 2)
sum(x, na.mr = TRUE)  # The sum should be 3

sum(x, na.rm = TRUE)

### Lazy Evaluations ===================================
# By default, R function arguments are lazy — they’re only 
# evaluated if they’re actually used:

f <- function(x) {
  10
}

f(stop("This is an error!")) # This will return 10

# If you want to ensure that an argument is evaluated 
# you can use force():

f <- function(x) {
  force(x)
  10
}

f(stop("This is an error!"))

# This is important when creating closures with lapply()
# or a loop:

add <- function(x) {
  function(y) x + y
}

adders <- lapply(1:10, add)
adders[[1]](10)

adders[[10]](10)

# NOTE____about ___ alpply()__ 
#___it is operation on list object and we have not
#_______talked about list yet

# x is lazily evaluated the first time that you call one of
# the adder functions. At this point, the loop is
# complete and the final value of x is 10. 
# Therefore all of the adder functions will add 10 on 
# to their input, probably not what you wanted!
# Manually forcing evaluation fixes the problem:

add <- function(x) {
  force(x)
  function(y) x + y
}

adders2 <- lapply(1:10, add)
adders2[[1]](10)

adders2[[10]](10)

### EXERCISES ==========================================

# What does  commas(letters, collapse = "-") do? Why?

commas(letters, collapse = "-")

# It gives an error. This is because when the argument
# collapse is given to commas(), it is passed to str_c()
# as part of ...

#__________________________________________________________


# It’d be nice if you could supply multiple characters to 
# the pad argument, e.g  rule("Title", pad = "-+"). 
# Why doesn’t this currently work? How could you fix it?

# We had the function in this file
rule("Important output")

# You can currently supply multiple characters to the 
# pad argument, but the output will not be the desired 
# width. The rule() function duplicates pad a number of 
# times equal to the desired width minus the length of 
# the title and five extra characters. 
# This implicitly assumes that pad is only one character.
# If pad were two character, the output will be almost 
# twice as long.

rule("Valuable output", pad = "-+")

# One way to handle this is to use str_trunc() to 
# truncate the string, and str_length() to calculate 
# the number of characters in the pad argument.

library(magrittr)
library(stringr)

rule <- function(..., pad = "-") {
  title <- paste0(...)
  width <- getOption("width") - nchar(title) - 5
  padding <- str_dup(
    pad,
    ceiling(width / str_length(title))
  )%>%
    str_trunc(width)
  cat(title, " ", padding, "\n", sep = "")
}


rule("Important output")

rule("Valuable output", pad = "-+")

rule("Vital output", pad = "-+-")

#__________________________________________________________

# What does the trim argument to mean() do? 
# When might you use it?


# The trim arguments trims a fraction of observations from
# each end of the vector (meaning the range) before 
# calculating the mean. This is useful for calculating a 
# measure of central tendency that is robust to outliers.


#__________________________________________________________

# The default value for the method argument to cor() is
# c("pearson", "kendall", "spearman"). 
# What does that mean? What value is used by default?

c("pearson", "kendall", "spearman")

# It means that the method argument can take one of 
# those three values. The first value, "pearson", 
# is used by default.

#__________________________________________________________

## Return Values ---------------------------------------

# The value returned by the function is usually the last
# statement it evaluates, but you can choose to return 
# early by using return().


### Explicit Return Statements =========================

complicated_function <- function(x, y, z) {
  if (length(x) == 0 || length(y) == 0) {
    return(0)
  }
  
  # Complicated code here
}

# If statement with one complex block and one simple block

f <- function() {
  if (x) {
    # Do 
    # something
    # that
    # takes
    # many
    # lines
    # to
    # express
  } else {
    # return something short
  }
}


#  if the first block is very long, by the time you get to 
# the else, you’ve forgotten the condition. 
# One way to rewrite it is to use an early return for 
# the simple case:

f <- function() {
  if (!x) {
    return(something_short)
  }
  
  # Do 
  # something
  # that
  # takes
  # many
  # lines
  # to
  # express
}





### Writing Pipeable Functions =========================

# There are two basic types of pipeable functions: 
# transformations and side-effects.

#### transformation ----------------------------------

# an object is passed to the function’s first argument and
# a modified object is returned




#### side-effects ----------------------------------

# the passed object is not transformed. Instead, the 
# function performs an action on the object, like drawing 
# a plot or saving a file. Side-effects functions should
# “invisibly” return the first argument, so that while 
# they’re not printed they can still be used in a pipeline.


show_missings <- function(df) {
  n <- sum(is.na(df))
  cat("Missing values: ", n, "\n", sep = "")
  
  invisible(df)
}

# invisible() means that the input df doesn’t get printed out

show_missings(mtcars)


# But it’s still there, it’s just not printed by default:


x <- show_missings(mtcars) 

class(x)

dim(x)


# And we can still use it in a pipe:

mtcars %>% 
  show_missings() %>% 
  mutate(mpg = ifelse(mpg < 20, NA, mpg)) %>% 
  show_missings() 


# _________________________________________________________


## Environment ---------------------------------------

#  The environment of a function controls how R finds the
# value associated with a name.
# For example, take this function:

f <- function(x) {
  x + y
} 

# In many programming languages, this would be an error,
# because y is not defined inside the function. 
# In R, this is valid code because R uses rules called
# lexical scoping

# Since y is not defined inside the function, R will look in 
# the environment where the function was defined:

y <- 100
f(10)


y <- 1000
f(10)

# you should avoid creating functions like this deliberately


# The advantage of this behaviour is that from a language
# standpoint it allows R to be very consistent. 
# Every name is looked up using the same set of rules.
# For f() that includes the behaviour of two things that
# you might not expect: { and +. This allows you to do
# devious things like:


`+` <- function(x, y) {
  if (runif(1) < 0.1) {
    sum(x, y)
  } else {
    sum(x, y) * 1.1
  }
}
table(replicate(1000, 1 + 2))
rm(`+`)








#________________________________________________

# CLEAN UP #############################################


# We can do clean up with some other packages at the end

## Clear Environment ##################################

rm(list = ls())

# It takes whatever is in the environment and clean it up



## Clear contributed packages ##########################

p_unload(all) # Remove all add-ons



# This command comes from pacman and unload all of the 
# contributed packages



## Clear plots #########################################

dev.off()  # only if there is a plot


## Clear Console #######################################

cat("\014") # shortcut --->   ctrl+L







