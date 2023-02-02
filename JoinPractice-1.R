


library(tidyverse)
library(nycflights13)

data("airlines")
data("airports")
data("flights")
data("planes")
data("weather")


head(flights)
head(airlines)
head(planes)





dx <- tribble(~kf, ~val_x, ~val_X, 
             1,    "x1",    "X1",
             2,    "x2",    "X2",
             3,    "x3",    "X3",
             1,    "x3",    "X3",)


dy <- tribble(~kp, ~val_y,
             1,    "y1",
             2,    "y2",
             4,    "y3")
# kp is primary key in dy
dy %>%
  group_by(kp) %>%
  count() %>%
  filter(n > 1)

# kf is foreign key in dx
dx %>%
  group_by(kf) %>%
  count() %>%
  filter(n > 1)



# kp and val_x are primary key in dy
dx %>%
  group_by(kf, val_x) %>%
  count() %>%
  filter(n > 1)


# Inner Join ------------------------------------------------------

x <- tribble(~key, ~val_x, ~val_X, 
             1,    "x1",    "z1",
             2,    "x2",    "z2",
             3,    "x3",    "z3")


y <- tribble(~key, ~val_y,
             1,    "y1",
             2,    "y2",
             4,    "y3")



inner_join(x, y, by = "key")
inner_join(y,x, by = "key")


inner_join(flights, planes, by = "tailnum") -> fp_inner

fp_inner

glimpse(fp_inner)

names(flights)
?flights  # year, month, day Date of departure.

names(planes)
?planes   # year manufactured.

names(fp_inner)

# Outer Join ------------------------------------------------------



# Example: Add the full airline names to the `flights` data frame.

head(airlines$carrier) # carrier and the name are variables in airlines.
head(airlines$name)

# Carrier is a primary key for airlines
airlines %>%
  group_by(carrier) %>%
  count() %>%
  filter(n > 1)

# Carrier is a foreign key for flights
flights %>%
  group_by(carrier) %>%
  count() %>%
  filter(n > 1)




fa_left <- left_join(flights, airlines, by = "carrier")
fa_left
# flights has 19 variables and it 
# joined by carrier and get the column name added to outcome 
# When mutating the new column moves to the right most place. Thus, name is the 
# last column

names(flights)
names(airlines)
names(fa_left)


left_join(flights, airlines, by = "carrier") %>%
  select(name, everything())   # It brings name to be first 




x <- tribble(~key, ~val_x, ~val_z, 
             1,    "x1",    "z1",
             2,    "x2",    "z2",
             3,    "x3",    "z3")

y <- tribble(~key, ~val_y,
             1,    "y1",
             2,    "y2",
             4,    "y3")

left_join(x, y, by="key")




left_join(y, x, by="key")




right_join(x, y, by="key")




full_join(x, y, by="key")

# Duplicate keys--------------------------------------------------------

y_m <- tribble(~key, ~val_y,
               1,    "y1",
               2,    "y2")

x_mult <- tribble(~key, ~val_x,
                  1,    "x1",
                  2,    "x2",
                  2,    "x3",
                  1,    "x4")
# key is a foreign key for x_mult
x_mult %>%
  group_by(key) %>%
  count() %>%
  filter(n > 1)

# key is a Primary key for y_m
y_m %>%
  group_by(key) %>%
  count() %>%
  filter(n > 1)


left_join(x_mult, y_m, by = "key") # Duplicated keys in one dataframe

left_join(y_m, x_mult, by = "key")


# Duplicate in both
y_m1 <- tribble(~key, ~val_y,
                1,    "y1",
                2,    "y2",
                2,    "y3",
                3,    "y4")

x_mult1 <- tribble(~key, ~val_x,
                   1,    "x1",
                   2,    "x2",
                   2,    "x3",
                   3,    "x4")
left_join(x_mult1, y_m1, by = "key") # Duplicated key in both dataframes. 
# If you have duplicates in both (usually a mistake), then you get every 
# possible combination of the values in x and y at the key values where there
# are duplicated.

# is there any otherway to produce this?

right_join(x_mult1, y_m1,  by = "key")

# Or

right_join(y_m1, x_mult1,  by = "key")

right_join(y_m1, x_mult1,  by = "key") %>%
  select(key, val_x, val_y)

# or

full_join(x_mult1, y_m1, by = "key")

# Exercise

# R code to create

tribble(~key, ~val_x, ~val_y, 
        1,    "x1",    "y1",
        1,    "x1",    "y4",
        2,    "x2",    "y2",
        2,    "x2",    "y3",
        2,    "x3",    "y2",
        2,    "x3",    "y3",
        1,    "x4",    "y1",
        1,    "x4",    "y4")


# First We need the data frames


x_mult <- tribble(~key, ~val_x,
                  1,    "x1",
                  2,    "x2",
                  2,    "x3",
                  1,    "x4")

y_mult <- tribble(~key, ~val_y,
                  1,    "y1",
                  2,    "y2",
                  2,    "y3",
                  1,    "y4")
left_join(x_mult, y_mult, by="key")

# Other implementations--------------------------------------------
# Note base::merge can perform all four types of mutating join:

u <- tribble(~key, ~val_u,
             1,    "u1",
             2,    "u2",
             3,    "u3")

v <- tribble(~key, ~val_v,
             1,    "v1",
             2,    "v2",
             4,    "v4")



inner_join(u,v, by = "key")

base::merge(u, v)

semi_join(u, v, by = "key")

semi_join(v, u, by = "key")
#____________________
left_join(u,v, by = "key")

base::merge(x=u, y=v, by = "key", all.x= TRUE)
#____________________

right_join(u,v, by = "key")

base::merge(x=u, y=v, by = "key", all.y= TRUE)
#____________________

full_join(u,v, by = "key")

base::merge(x=u, y=v, by = "key", all= TRUE)
#____________________

##Example:-------------------
#Find the 10 days of the year that have the highest median 
#departure delay, then select all flights from those 10 days.

flights %>%
  group_by(year, month, day) %>%
  summarize(med_dep = median(dep_delay, na.rm = TRUE)) %>%
  arrange(desc(med_dep)) %>%
  ungroup() %>%
  slice(1:10) ->
  ten_worst


semi_join(flights, ten_worst)


# Filtering Joins ----------------------------------
# Semi-joins are useful for matching filtered summary tables back to 
# the original rows.
u
v
semi_join(u, v, by = "key")

semi_join(v, u, by = "key")

x_mult
y_mult

s<- semi_join(x_mult, y_mult, by = "key")
s

# The inverse of a semi-join is an anti-join. An anti-join keeps the rows that
# don't have a match.


u
v

anti_join(u, v, by = "key")

anti_join( v, u, by = "key")


anti_join(s, y_mult, by = "key")

anti_join(s, x_mult, by = "key")

anti_join(x_mult, y_mult, by = "key")




# Defining The Key Columns-----------------------------------------

#If the primary and foreign keys do not match, you need to specify 
# that using a named vector as
#       left_join(x, y, by = c("a" = "b")), 
# where a is the key in x and b is the key in y.

# The primary and foreign keys do not match for 
#         flights and airports.

# Why we use "origin" = "faa"?


# How many observation and variables in flights and airports?
head(airports)
unique(airports)
names(airports)  # Does it have origin?
unique(airports$faa)
names(flights)  # Does it have faa?
unique(flights$origin)




# What would be the variables of the following?
new <- left_join(flights, airports, by = c("origin" = "faa"))

names(new)


# which dataframe has origin? and faa is replaced with origin!
names(airports)
unique(airports$faa)


# If you have multiple variables acting as the key, you need the by 
# argument to be a vector.

names(flights)
names(weather)


left_join(flights, weather, by = c("origin", "year", "month", "day",
                                   "hour"))

