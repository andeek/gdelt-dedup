# libraries
library(tidyverse)

# results
# 500 observations, 1000 iterations, 100 simulated datasets
load("notes/gibbs_output.Rdata")

m <- 1000
lambda <- 50

distortions <- data.frame(zero = rep(NA, m),
                    ones = rep(NA, m),
                    twos = rep(NA, m))
for(i in seq_len(m)) {
  tab <- table(num.distortions.sims[, i, lambda])
  distortions[i, "zero"] <- tab[1] # no distortion
  distortions[i, "ones"] <- tab[2] # single distortion
  distortions[i, "twos"] <- tab[3] # two distortions
}

distortions %>%
  mutate(iter = 1:m) %>%
  gather(dist, val, -iter) %>%
  ggplot() +
  geom_line(aes(iter, val)) +
  facet_wrap(.~dist, scales = "free_y")


