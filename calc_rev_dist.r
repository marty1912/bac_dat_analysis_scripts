library(dplyr)
library(tidyr)

#dat <- read.csv("all_data.csv")
dat <- read.csv("data_cleaned.csv")

unique_prob_codes <- unique(dat$prob_code)

dat <- dat %>% filter(practice == FALSE)  %>% filter(correct_num == TRUE)%>% filter(is.na(rt_num)== FALSE) %>% 
    group_by(prob_code,mode,ordered,distance)   %>% summarise(mean_rt= mean(rt_num,rm.na=TRUE))

print(dat)

write.csv(dat,file="data_sum_by_part_mode.csv",row.names=FALSE,na="")

dat_for_anova <- data.frame(dat)

dat_for_anova <- reshape(dat_for_anova,idvar=c("prob_code","distance","mode"), timevar = c("ordered"),direction="wide")
dat_for_anova <- reshape(dat_for_anova,idvar=c("prob_code","distance"), timevar = c("mode"),direction="wide")
dat_for_anova <- reshape(dat_for_anova,idvar=c("prob_code"), timevar = c("distance"),direction="wide")

write.csv(dat_for_anova,file="data_for_anova.csv",row.names=FALSE,na="")

dat <- data.frame(dat)

dat <- reshape(dat, idvar=c("prob_code","mode","ordered"), timevar = c("distance"), direction="wide")

dat$rev_dist_effect <- dat$mean_rt.2 - dat$mean_rt.1
dat$rev_dist_effect_normed <- (dat$mean_rt.2 - dat$mean_rt.1)/((dat$mean_rt.2 + dat$mean_rt.1)/2)

dat$dist_effect <- dat$mean_rt.1 - dat$mean_rt.2
dat$dist_effect_normed <- (dat$mean_rt.1 - dat$mean_rt.2)/((dat$mean_rt.2 + dat$mean_rt.1)/2)

write.csv(dat,file="data_with_dist_effect.csv",row.names=FALSE,na="")


dat <- reshape(dat, idvar=c("prob_code","mode"), timevar = c("ordered"), direction="wide")
dat <- reshape(dat, idvar=c("prob_code"), timevar = c("mode"), direction="wide")

write.csv(dat,file="data_with_dist_effect_1rpP.csv",row.names=FALSE,na="")


