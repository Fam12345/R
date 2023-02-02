print('hello world')
3%%2 # reminder or module
14%%3
15%%2
help('%%') # to get help make sure it is in quotations 
3:1
a = 10 # does it work?
a =3 
b=5
sum(a, b)
a<-b<-c<-25
a<-20
a+b

v <- c(1,3,5,17) #  c combine or concatenate or collect 
# creating squencesv
seq(10,0, by= -2)
10:0
0:10

# Getting help

?seq() 
help('seq()')

seq(0,1, length.out = 11)

(w <- c(1,-3,5,-9)) # rap the data if you want to print at the same time

print(v+w)
exp(1)
# In programming language log means natural log.
# In programming 0.25 * 10^-8 = 0.25E-8 
log10(100)

##Logicals 

10 == 2*5
TRUE & TRUE
T & F
F&F
F&T

F|F
# R is case sensitive. 

T&F|T
 # only compare the first elements of the vectors 

v=c(F,T,F,T)
l=c(T,F,T,T)
v||l

#####################Comparing 
x<-c(1:4)
w <- c(1,2,4,3)
v<- c(2,3)
y<-x

x==w
w+v

(x<3)&&(y>3)
(x<3)||(y>3)

#####

x<- 1:5
x[c(T,F,T,T,F)]

x[x<3] ### Creating objects is better for programming: 
#it gives power to play with the objects.

##########Functions###

log(x=4, base = 2)

log(4,2)
log(2,4) 
tidyverse_update()
library(tidyverse)
mpg
ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))
# This is second session: Sepetmper 12, 2022
#Pipes and Fuctions: Pipes are most useful for rewriting a fairly short linear sequence of operations
library(magrittr): # %>%, %T>%, %$%
  rnorm(100) %>%
  matrix(ncol = 2) %T>%
  plot() %>%
  str()  

rescale01 <- function(v) {
  v1 <- v - min(v, na.rm = TRUE)
  d <-  max(v,na.rm = TRUE)-min(v, na.rm = TRUE)
  u<- v1/d
}

l<- c(1,3,5)
print(rescale01(l))

# inf/inf= It has no answer and is undefined since infinity is not a number, 
# but just a concept of something very big. You need actual numbers, or
# something that resolves to a real number, like a limiting series, to give an answer.

rescale01 <- function(x) {
  rng <- range(x, na.rm = TRUE)
  (x - rng[1]) / (rng[2] - rng[1])
}

l<- c(1,3,5,-Inf, Inf)
print(rescale01(l))

#Range function in CS it retain the max and min.
# range(l) gives (min,max)

df <- tibble::tibble(
a= rnorm(10),
b =rnorm(10),
c =rnorm(10),
d = rnorm(10)

)

View(df)

x <- c(1, 2,3 ,4, 5, Inf, -Inf)
mean(is.na(x))
# ! NEGATION 
sum(x, na.rm = NA)

x/sum(x, na.rm = TRUE)
sum(x)

u= c(1,2,3,4)
u[-1:0]
# variance= sd/mean^2
# Measure of Skweness 
#The formula given in most textbooks is Skew = 3 * (Mean – Median) / Standard Deviation.
#  


#For assignment magrittr provides the %<>% operator which allows you to replace code like
# Writing a function has three big advantages over using copy-and-paste:
#   
# 1. You can give a function an evocative name that makes your code easier to understand.
# 
# 2. As requirements change, you only need to update code in one place, instead of many.
# 
# 3. You eliminate the chance of making incidental mistakes when you copy and paste (i.e. updating a variable name in one place, but not in another).


#function has:
# name 
# argmunets 
# body

#------------------------------
# **** Careful of package applications 

atan(cos(sin(sqrt(4*pi))))
#piping 
library(magrittr)
(sin(sqrt(4*pi)))%>%
  cos() %>%
  atan()
# Order of operation matter in pipping 

4%>%
cos() %>%
atan()

m1= rnorm(10) 
m2 = matrix(m1, ncol = 2)
str(m2)
m3 =plot(m2) 
str(m3)

# rnorm gives you the same thing if you make seed set.

#Lists

my_list = list(letters[1:3], 555, c(1,2,3))
my_list

head(mtcars)

library(dplyr)  
glimpse(mtcars)

#P_unload and rm

### Functions 

library(lattice)
library(tidyverse)
head(mpg)
glimpse(mpg)
mpg$drv
unique(mpg)
typeof(mpg$cyl)
ggplot(data = mpg) + 
  geom_point(mapping = aes(displ, y = hwy, size = class))
mpg%>%
  ggplot(aes(hwy, displ)) + geom_point() + geom_smooth(se=F) # CONFIDENCE INTERVAL

mpg%>%
  ggplot(aes(displ, hwy)) + geom_point() + geom_smooth()
# Data must be data frame or tibble()
?geom_smooth


?ggplot()


ggplot(mpg, mapping=aes(displ, hwy), color=drv) + geom_point() 

?mpg

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")

install.packages("palmerpenguins")
library(palmerpenguins)
data("penguins")        
view(penguins)   
tibble(penguins)
?penguins

pen_mass_depth <- ggplot(data = penguins,
                         mapping = aes(x  = body_mass_g, 
                                       y = bill_depth_mm, 
                                       color = species)) +
  geom_point()

pen_mass_depth

?mtcars
typeof(mtcars$hp)
fg