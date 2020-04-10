library(tidyverse)
library(heatmaply)

breakdown <- read_csv("Data/Group_Corr_Data.csv")

bd_data <- breakdown %>% 
  mutate(cluster = paste(RIDRETH1, Gender)) %>% 
  select(Factor, cluster, `Relative Importance`) 

bd_matrix <- bd_data %>%
  spread(key = cluster, value = `Relative Importance`) %>% 
  as.data.frame()

rownames(bd_matrix) <- bd_matrix$Factor
bd_matrix

bd_data_frame <- bd_matrix[, 2:11]
bd_data_frame[is.na(bd_data_frame)] <- 0

bd_data_frame

df1 <- as.data.frame(cor(bd_data_frame, use = "complete.obs",method = "pearson"))
df1

# write_csv(df1, "correlation_matrix_all_groups_all_ages.csv")
