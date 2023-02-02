library(tidyverse)
#Do cars with big engines use more fuel than cars with small engines?
ggplot2::mpg[displ,hwy]
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
 
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))
