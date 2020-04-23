library(dplyr)

dat <- read.csv("all_data.csv")

unique_prob_codes <- unique(dat$prob_code)

dat <- dat %>% filter(practice == FALSE)  %>% filter(correct_ord == TRUE)%>% filter(is.na(rt_ord)== FALSE) %>% 
    group_by(prob_code,mode,ordered,distance)   %>% summarise(mean_rt= mean(rt_ord,rm.na=TRUE))

print(dat)

write.csv(dat,file="data_sum_by_part_mode.csv",row.names=FALSE,na="")


dat <- data.frame(dat)

dat <- reshape(dat, idvar=c("prob_code","mode","ordered"), timevar = c("distance"), direction="wide")

dat$rev_dist_effect <- dat$mean_rt.2 - dat$mean_rt.1
dat$rev_dist_effect_normed <- (dat$mean_rt.2 - dat$mean_rt.1)/((dat$mean_rt.2 + dat$mean_rt.1)/2)

dat$dist_effect <- dat$mean_rt.1 - dat$mean_rt.2
dat$dist_effect_normed <- (dat$mean_rt.1 - dat$mean_rt.2)/((dat$mean_rt.2 + dat$mean_rt.1)/2)

write.csv(dat,file="data_with_dist_effect.csv",row.names=FALSE,na="")
