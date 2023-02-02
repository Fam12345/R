# Implement a fizzbuzz() function which takes a single number as input
# x: is a single number as input
# If the number is divisible by three and five, return "fizzbuzz"
# If the number is divisible by three, return "fizz"
# If the number is divisible by five, return "buzz"


fizzbuzz_s <- function(x) {
  stopifnot(length(x) == 1) # checking if x is single input, if not error
  stopifnot(is.numeric(x)) # Checking if x is numeric, if not error  
  if(x%%5==0 & x%%3==0)
    return("fizzbuzz")
  else if(x%%5==0)
    return("buzz")
  else if (x%%3==0)
    return("fizz")
  else
    return(x)
}



