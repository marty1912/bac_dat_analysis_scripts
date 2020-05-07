library(dplyr)

d <- read.csv("all_data.csv")

d <- d %>% group_by(prob_code,mode) %>% mutate(trial_n = row_number())

print(d)



write.csv(d,file="data_all_n_row.csv",row.names=FALSE,na="")
