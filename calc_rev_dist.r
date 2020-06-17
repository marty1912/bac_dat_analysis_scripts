library(dplyr)
library(tidyr)

#######################################################################################
# step 1: get the data.
####################################################################################
dat <- read.csv("data_cleaned.csv")

unique_prob_codes <- unique(dat$prob_code)

dat <- dat %>% filter(practice == FALSE)  %>% filter(correct_num == TRUE)%>% filter(is.na(rt_num)== FALSE) %>% 
    group_by(prob_code,mode,ordered,distance)   %>% summarise(mean_rt= mean(rt_num,rm.na=TRUE))


#######################################################################################
# calc ies
####################################################################################
dat_acc <- read.csv(file="data_accuracy.csv")
dat_ies <- merge(data.frame(dat_acc),data.frame(dat),by=c("prob_code","ordered","distance","mode"))
dat_ies <- dat_ies %>% mutate(ies = mean_rt/accuracy)


dat <- dat_ies



dat_for_anova <- data.frame(dat)

dat_for_anova <- reshape(dat_for_anova,idvar=c("prob_code","distance","mode"), timevar = c("ordered"),direction="wide")
dat_for_anova <- reshape(dat_for_anova,idvar=c("prob_code","distance"), timevar = c("mode"),direction="wide")
dat_for_anova <- reshape(dat_for_anova,idvar=c("prob_code"), timevar = c("distance"),direction="wide")

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


dat <- reshape(dat, idvar=c("prob_code","mode"), timevar = c("ordered"), direction="wide")
dat <- reshape(dat, idvar=c("prob_code"), timevar = c("mode"), direction="wide")

dat <- dat[complete.cases(dat),]

dat_all <- read.csv("data_cleaned.csv")
dat_all <- dat_all %>% filter(prob_code %in% dat$prob_code)

dat_rig <- dat_all %>% select(prob_code,rig_randomness_p)  %>% distinct(prob_code,.keep_all=TRUE)

dat <- merge(dat_rig,dat,by='prob_code')
dat <- dat %>% mutate(rig_random_sig = ifelse(rig_randomness_p < 0.05,1,0))


########################################################################################################### 
# write data: 
# if you are using jamovi the file data_with_dist_effect_1rpP.csv is for you  (to do anova etc.)
# else use the data_cleaned_complete file 
########################################################################################################### 
write.csv(dat,file="data_with_dist_effect_1rpP.csv",row.names=FALSE,na="")
write.csv(dat_all,file="data_cleaned_complete.csv",row.names=FALSE,na="")


