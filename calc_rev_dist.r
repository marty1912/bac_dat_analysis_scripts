library(dplyr)
library(tidyr)

#dat <- read.csv("all_data.csv")
dat <- read.csv("data_cleaned.csv")

unique_prob_codes <- unique(dat$prob_code)

dat <- dat %>% filter(practice == FALSE)  %>% filter(correct_num == TRUE)%>% filter(is.na(rt_num)== FALSE) %>% 
    group_by(prob_code,mode,ordered,distance)   %>% summarise(mean_rt= mean(rt_num,rm.na=TRUE))

print(dat)

write.csv(dat,file="data_sum_by_part_mode.csv",row.names=FALSE,na="")

## here we can calculate the IES
dat_acc <- read.csv(file="data_accuracy.csv")
dat_ies <- merge(data.frame(dat_acc),data.frame(dat),by=c("prob_code","ordered","distance","mode"))
write.csv(dat_ies,file="data_merged_ies.csv",row.names=FALSE,na="")
dat_ies <- dat_ies %>% mutate(ies = mean_rt/accuracy)

write.csv(dat_ies,file="data_ies.csv",row.names=FALSE,na="")

#######################################################################################

# TODO: REMOVE!!
#dat_ies$mean_rt <- dat_ies$ies
#dat <- dat_ies %>% select(prob_code,ordered,distance,mode,mean_rt)
dat <- dat_ies
####################################################################################


dat_for_anova <- data.frame(dat)

dat_for_anova <- reshape(dat_for_anova,idvar=c("prob_code","distance","mode"), timevar = c("ordered"),direction="wide")
dat_for_anova <- reshape(dat_for_anova,idvar=c("prob_code","distance"), timevar = c("mode"),direction="wide")
dat_for_anova <- reshape(dat_for_anova,idvar=c("prob_code"), timevar = c("distance"),direction="wide")

write.csv(dat_for_anova,file="data_for_anova.csv",row.names=FALSE,na="")

dat <- data.frame(dat)

dat <- reshape(dat, idvar=c("prob_code","mode","ordered"), timevar = c("distance"), direction="wide")

dat$rev_dist_effect_ies <- dat$ies.2 - dat$ies.1
dat$rev_dist_effect_ies_normed <- (dat$ies.2 - dat$ies.1)/((dat$ies.2 + dat$ies.1)/2)

dat$dist_effect_ies <- dat$ies.1 - dat$ies.2
dat$dist_effect_ies_normed <- (dat$ies.1 - dat$ies.2)/((dat$ies.2 + dat$ies.1)/2)


dat$rev_dist_effect <- dat$mean_rt.2 - dat$mean_rt.1
dat$rev_dist_effect_normed <- (dat$mean_rt.2 - dat$mean_rt.1)/((dat$mean_rt.2 + dat$mean_rt.1)/2)

dat$dist_effect <- dat$mean_rt.1 - dat$mean_rt.2
dat$dist_effect_normed <- (dat$mean_rt.1 - dat$mean_rt.2)/((dat$mean_rt.2 + dat$mean_rt.1)/2)

write.csv(dat,file="data_with_dist_effect.csv",row.names=FALSE,na="")


dat <- reshape(dat, idvar=c("prob_code","mode"), timevar = c("ordered"), direction="wide")
dat <- reshape(dat, idvar=c("prob_code"), timevar = c("mode"), direction="wide")

dat <- dat[complete.cases(dat),]

dat_all <- read.csv("data_cleaned.csv")
dat_all <- dat_all %>% filter(prob_code %in% dat$prob_code)

dat_rig <- dat_all %>% select(prob_code,rig_randomness_p)  %>% distinct(prob_code,.keep_all=TRUE)

dat <- merge(dat_rig,dat,by='prob_code')
dat <- dat %>% mutate(rig_random_sig = ifelse(rig_randomness_p < 0.05,1,0))


write.csv(dat,file="data_with_dist_effect_1rpP.csv",row.names=FALSE,na="")
write.csv(dat_all,file="data_cleaned_complete.csv",row.names=FALSE,na="")


