# libraries ----
library(ggplot2)
library(dplyr)
library(tidyr)

# load data ----
load("results/posterior_comparison_3_records.Rdata")
head(err.df)

# make some initial plots ----
err.df %>%
  gather(method, post_prob_truth, starts_with("Posterior")) %>%
  separate(method, c("junk", "junk2", "method")) %>%
  ggplot() +
  geom_boxplot(aes(as.character(Number_of_Errors), post_prob_truth, fill = method)) +
  facet_wrap(.~Lambda)

# things look vary similar depending on the number of elements of the partition
# lets check this is true
err.df %>%
  group_by(Lambda, Number_of_Errors) %>%
  mutate(xyz_iter = 1:n()) %>%
  ungroup() %>%
  mutate(Lambda = as.character(Lambda)) %>%
  mutate(num_in_partition = unlist(lapply(strsplit(Lambda, ","), function(x) length(unique(x))))) %>% 
  gather(method, post_prob_truth, starts_with("Posterior")) %>%
  separate(method, c("junk", "junk2", "method")) -> unique_err.df 

# these are NOT the same within the num_in_partition
unique_err.df %>% filter(xyz_iter == 2 & method == "Sadinle" & Number_of_Errors == 0 & num_in_partition == 1) 
unique_err.df %>% filter(xyz_iter == 2 & method == "Sadinle" & Number_of_Errors == 0 & num_in_partition == 2) 

# but it looks like the distributions are?
unique_err.df %>%
  filter(num_in_partition == 2) %>%
  group_by(Lambda, method, Number_of_Errors) %>%
  summarise(q_.1 = quantile(post_prob_truth, .1),
            q_.25 = quantile(post_prob_truth, .25),
            q_.5 = quantile(post_prob_truth, .5),
            q_.75 = quantile(post_prob_truth, .75),
            q_..9 = quantile(post_prob_truth, .9),
            mean = mean(post_prob_truth),
            sd = sd(post_prob_truth)) %>%
  arrange(method, Number_of_Errors, Lambda) %>%
  data.frame() %>%
  head(20)

# look at the dsns
unique_err.df %>% 
  filter(Lambda == "2,2,1") %>% # only need to look at one distribution
  ggplot() +
  geom_density(aes(post_prob_truth, colour = method, fill = method), alpha = .5) +
  facet_wrap(.~Number_of_Errors, scales = "free")


unique_err.df %>% 
  filter(Lambda == "2,2,1") %>% # only need to look at one distribution
  ggplot() +
  geom_boxplot(aes(as.character(Number_of_Errors), post_prob_truth, colour = method, fill = method), alpha = .5)


