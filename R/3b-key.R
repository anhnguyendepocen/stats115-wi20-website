library(bayesrules)
library(fivethirtyeight)
library(tidyverse)

plot_beta(14,1)
plot_beta(1,1)
plot_beta(5,11)

set.seed(84735)

bechdel %>% 
  sample_n(20) %>% 
  group_by(binary) %>% 
  summarize(n()) 

plot_beta_binomial(alpha = 1, beta = 1, x = 9, n = 20)

summarize_beta_binomial(alpha = 1, beta = 1, 
                        x = 9, n = 20)



# Step 1 for Paola

bechdel %>% 
  filter(year == 1971) %>% 
  group_by(binary) %>% 
  summarize(n())

summarize_beta_binomial(alpha = 14, beta = 1, 
                        x = 0, n = 5)

# Step 2 for Paola 

## New prior alpha = 14, beta = 6

bechdel %>% 
  filter(year == 1972) %>% 
  group_by(binary) %>% 
  summarize(n())

summarize_beta_binomial(alpha = 14, 
                        beta = 6, 
                        x = 1, n =3)

# Step 3 for Paola

## New prior alpha = 15, beta = 8

bechdel %>% 
  filter(year == 1973) %>% 
  group_by(binary) %>% 
  summarize(n())

summarize_beta_binomial(alpha = 15, 
                        beta = 8, 
                        x = 1, n =5)

# Paola ends with a posterior of 16, 12

## Mark

bechdel %>% 
  filter(year == 1971 | 
           year == 1972 | 
           year == 1973) %>% 
  group_by(binary) %>% 
  summarize(n())

summarize_beta_binomial(alpha = 14, 
                        beta = 1, 
                        x = 2, n =13)
