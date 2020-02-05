library(tidyverse)

## Start with prior Beta(2,2)
## 9 successes in 10 trials 
## We know we will have a poster Beta (11,3)
## We will try grid approximation with 6 (and then 101) different pi values  

# Step 1: Define a grid of 6 pi values
pi_grid <- seq(from = 0, to = 1, length = 6)
grid_data <- data.frame(pi_grid)


# Step 2: Evaluate the prior & likelihood at each pi
grid_data <- grid_data %>%
  mutate(prior = dbeta(pi_grid, 2, 2)) %>%
  mutate(likelihood = dbinom(9, 10, pi_grid))

grid_data <- grid_data %>%
  mutate(unnormalized = likelihood * prior) %>%
  mutate(posterior = unnormalized / sum(unnormalized))

# Confirm that the posterior approximation sums to 1
grid_data %>%
  summarize(sum(posterior), sum(unnormalized))

# Examine the grid approximated posterior
round(grid_data, 2)


# Plot the grid approximated posterior
ggplot(grid_data, aes(x = pi_grid, y = posterior)) +
  geom_point() +
  geom_segment(aes(x = pi_grid, xend = pi_grid,
                   y = 0, yend = posterior))


# Set the seed
set.seed(84735)

# Step 4: sample from the discretized posterior
post_sample <- sample_n(grid_data, size = 10000,
                        weight = posterior, replace = TRUE)

post_sample %>% 
  group_by(pi_grid) %>% 
  summarize(n = n()) %>% 
  mutate(perc = n / sum(n))

# Histogram of the grid simulation with posterior pdf
ggplot(post_sample, aes(x = pi_grid)) +
  geom_histogram(aes(y = ..density..), color = "white") +
  stat_function(fun = dbeta, args = list(11, 3)) +
  lims(x = c(0, 1))


### A Poisson-Gamma Example
## Prior for lambda is Gamma(3,1)
## x = 5, n = 1
## We know the posterior will be Gamma(8,2) 
## but let's show it with grid approximation
## lambda goes from 0 to infinity but for the purposes 
## of this simulation we will bound it 0 to 15.

# Step 1: Define a grid of 501 lambda values
lambda_grid <- seq(from = ___, to = ___, length = ___)
grid_data <- data.frame(lambda_grid)
# Step 2: Evaluate the prior & likelihood at each lambda
grid_data <- grid_data %>%
  ___(prior = dgamma(___, ___, ___)) %>%
  ___(likelihood = dpois(___, ___))
# Step 3: Approximate the posterior
grid_data <- grid_data %>%
  ___(unnormalized = ___) %>%
  ___(posterior = ___)
# Set the seed
set.seed(84735)
# Step 4: sample from the discretized posterior
post_sample <- sample_n(___, size = ___,
                           weight = ___, replace = ___)


  