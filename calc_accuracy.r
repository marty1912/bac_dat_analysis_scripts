library(dplyr)
library(tidyr)

d <- read.csv("all_data.csv")

d$correct_num[is.na(d$correct_num)] <- FALSE
d$correct_dual[is.na(d$correct_dual)] <- FALSE

dat <- d
unique_prob_codes <- unique(dat$prob_code)

dat <- dat %>% filter(practice == FALSE)  %>% 
    group_by(prob_code,mode,ordered,distance,correct_num)   %>% summarise( total_observations=n() ) 


dat$total_observations[is.na(dat$total_observations)] <- 0

dat <- data.frame(dat)
dat <- reshape(dat, idvar=c("prob_code","mode","ordered","distance"), timevar = c("correct_num"), direction="wide")

# replace na values with 0
dat$total_observations.FALSE[is.na(dat$total_observations.FALSE)] <- 0
dat$total_observations.TRUE[is.na(dat$total_observations.TRUE)] <- 0
dat$accuracy <- dat$total_observations.TRUE / (dat$total_observations.TRUE+dat$total_observations.FALSE)

print(dat)
write.csv(dat,file="data_accuracy.csv",row.names=FALSE,na="")
d_for_anova <- dat


############################################################################################################
dat <- d
unique_prob_codes <- unique(dat$prob_code)

dat <- dat %>% filter(practice == FALSE)  %>% 
    group_by(prob_code,correct_num)   %>% summarise( total_observations=n() ) 
#    group_by(prob_code,mode,ordered,distance,correct_num)   %>% summarise( total_observations=n() ) 


dat$total_observations[is.na(dat$total_observations)] <- 0

dat <- data.frame(dat)

#dat <- reshape(dat, idvar=c("prob_code","mode","ordered","distance"), timevar = c("correct_num"), direction="wide")
dat <- reshape(dat, idvar=c("prob_code"), timevar = c("correct_num"), direction="wide")


# replace na values with 0
dat$total_observations.FALSE[is.na(dat$total_observations.FALSE)] <- 0
dat$total_observations.TRUE[is.na(dat$total_observations.TRUE)] <- 0
dat$accuracy <- dat$total_observations.TRUE / (dat$total_observations.TRUE+dat$total_observations.FALSE)

print(dat)
write.csv(dat,file="data_accuracy_p_participant.csv",row.names=FALSE,na="")

############################################################################################################
dat <- d

dat <- dat %>% filter(practice == FALSE) %>% filter(mode == "dual_vis" | mode == "dual_phon")  %>%
    group_by(prob_code,mode,correct_dual)   %>% summarise( total_observations=n() ) 
#    group_by(prob_code,mode,dualered,distance,correct_dual)   %>% summarise( total_observations=n() ) 


dat$total_observations[is.na(dat$total_observations)] <- 0

dat <- data.frame(dat)


#dat <- reshape(dat, idvar=c("prob_code","mode","dualered","distance"), timevar = c("correct_dual"), direction="wide")
dat <- reshape(dat, idvar=c("prob_code","mode"), timevar = c("correct_dual"), direction="wide")



# replace na values with 0
dat$total_observations.FALSE[is.na(dat$total_observations.FALSE)] <- 0
dat$total_observations.TRUE[is.na(dat$total_observations.TRUE)] <- 0
dat$accuracy <- dat$total_observations.TRUE / (dat$total_observations.TRUE+dat$total_observations.FALSE)


print(dat)
write.csv(dat,file="data_accuracy_p_participant_dual.csv",row.names=FALSE,na="")


dat_accuracy <- read.csv("data_accuracy.csv")
dat_accuracy <- dat_accuracy %>% group_by(prob_code,mode) %>% mutate(accuracy = mean(accuracy))
write.csv(dat_accuracy,file="data_accuracy_part_mode.csv",row.names=FALSE,na="")

dat_accuracy <- data.frame(d_for_anova)
dat_accuracy <- dat_accuracy %>% select(prob_code,accuracy,mode,distance,ordered)
dat_accuracy <- data.frame(dat_accuracy)
dat_accuracy <- reshape(dat_accuracy, idvar=c("prob_code","ordered","distance"), timevar = c("mode"), direction="wide")
dat_accuracy <- reshape(dat_accuracy, idvar=c("prob_code","distance"), timevar = c("ordered"), direction="wide")
dat_accuracy <- reshape(dat_accuracy, idvar=c("prob_code"), timevar = c("distance"), direction="wide")

write.csv(dat_accuracy,file="data_accuracy_anova.csv",row.names=FALSE,na="")


