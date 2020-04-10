library(tidyverse)
library(ggthemes)
library(ggpubr)

t1 <- read.csv("Data/one_seventeen_preds.csv")
t2 <- read.csv("Data/eighteen_fortyfour_preds.csv")
t3 <- read.csv("Data/fortyfive_sixtyfour_preds.csv")
t4 <- read.csv("Data/sixtyfive_eightyfive_preds.csv")

individual_test_preds <- rbind(t1, t2, t3, t4)
preds <- as.data.frame(individual_test_preds)[,2:6]

preds %>% 
  mutate(error = prediction - RIDAGEYR) %>% 
  ggplot(mapping = aes(x = prediction, y = RIDAGEYR)) + 
  geom_jitter(color = "dodgerblue", alpha = 0.5, size = 1.5) +
  geom_abline(slope = 1, lwd = 1, color = "darkblue", alpha = 0.7) +
  labs(x = "Predicted Age", y = "Age") +
  theme_tufte()+
  theme(axis.text.x = element_text(color="black", 
                                   size=14),
        axis.text.y = element_text(color="black", 
                                   size=14),
        axis.title = element_text(face = "bold", size = 18))


pop_comp <- function(pred) {
  out_top_1 <- as.data.frame(pred) %>% 
    mutate(error = prediction - RIDAGEYR) %>% 
    top_frac(n = 0.05, wt = error) %>% 
    select(RIDAGEYR, RIDRETH1, RIAGENDR, INDFMPIR, error) %>% 
    mutate(Population = "OUTLIERS")
  
  out_bottom_1 <- as.data.frame(pred) %>% 
    mutate(error = prediction - RIDAGEYR) %>% 
    top_frac(n = -0.05, wt = error) %>% 
    select(RIDAGEYR, RIDRETH1, RIAGENDR, INDFMPIR, error) %>% 
    mutate(Population = "OUTLIERS")
  
  norms1 <- as.data.frame(pred) %>% 
    mutate(error = prediction - RIDAGEYR) %>% 
    filter(error > max(out_bottom_1$error) & error < min(out_top_1$error)) %>% 
    select(RIDAGEYR, RIDRETH1, RIAGENDR, INDFMPIR, error) %>% 
    mutate(Population = "NHANES")
  
  out_1 <- rbind(out_top_1, out_bottom_1)
  
  comp <- rbind(norms1, out_1)
  ks <- ks.test(out_1$INDFMPIR, norms1$INDFMPIR)
  
  racechi <- with(comp, table(Population, RIDRETH1))
  x2race <- chisq.test(racechi)
  
  genderchi <- with(comp, table(Population, RIAGENDR))
  x2gender <- chisq.test(genderchi)
  
  list(c(max(out_bottom_1$error),min(out_top_1$error)), ks, x2race, x2gender)
}
pop_comp(pred = t2)



out_top_1 <- as.data.frame(t1) %>% 
  mutate(error = prediction - RIDAGEYR) %>% 
  top_frac(n = 0.05, wt = error) %>% 
  select(RIDAGEYR, RIDRETH1, RIAGENDR, INDFMPIR, error) %>% 
  mutate(Population = "OUTLIERS")

out_bottom_1 <- as.data.frame(t1) %>% 
  mutate(error = prediction - RIDAGEYR) %>% 
  top_frac(n = -0.05, wt = error) %>% 
  select(RIDAGEYR, RIDRETH1, RIAGENDR, INDFMPIR, error) %>% 
  mutate(Population = "OUTLIERS")

norms1 <- as.data.frame(t1) %>% 
  mutate(error = prediction - RIDAGEYR) %>% 
  filter(error > max(out_bottom_1$error) & error < min(out_top_1$error)) %>% 
  select(RIDAGEYR, RIDRETH1, RIAGENDR, INDFMPIR, error) %>% 
  mutate(Population = "NHANES")

out_1 <- rbind(out_top_1, out_bottom_1)

comp <- rbind(norms1, out_1)
ks <- ks.test(out_1$INDFMPIR, norms1$INDFMPIR)

racechi <- with(comp, table(Population, RIDRETH1))
x2race <- chisq.test(racechi)

genderchi <- with(comp, table(Population, RIAGENDR))
x2gender <- chisq.test(genderchi)

list(ks, x2race, x2gender)


