library(tidyverse)
library(ggthemes)
library(ggpubr)
library(segmented)

nhanes <- read_csv("Data/NHANES_Final_Dataset.csv",
                   guess_max = 77000)

nhanes1 <- nhanes %>% 
  select(-c(LBXFB,
            LBXP1,
            LBXP2,
            LBXHE2,
            LBXSY1,
            LBXS06MK,
            LBXS11MK,
            LBXS16MK,
            LBXS18MK,
            LBXH2RL,
            LBX06,
            LBX11,
            LBX16,
            LBX18))

breakpoints <- function(analyte) {
  segment1 <- nhanes1 %>% 
    select(RIDAGEYR, analyte) %>% 
    filter(RIDAGEYR >= 11 & RIDAGEYR <= 30)
  
  model <- lm(as.formula((paste(analyte , paste('~RIDAGEYR')))), data = segment1)
  
  x <- segmented(model)
  y <- davies.test(model, values = x$psi[2])
  tryCatch({data.frame(x$psi[2], y$p.value)},  
           error = function(cond) {print(data.frame(0,1))},
           warning = function(cond) {return(NULL)})
}

breakpoint_data <- sapply(colnames(nhanes1[2:343]), breakpoints)
t(breakpoint_data)

bp <- t(breakpoint_data)


y <- as.data.frame(bp) %>% 
  mutate(significant = (y.p.value < (0.05/342))) %>% 
  filter(significant == TRUE) %>% 
  select(x.psi.2.)

median(as.numeric(as.matrix(y)))
