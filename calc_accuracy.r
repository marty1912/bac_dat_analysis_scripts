library(dplyr)
library(tidyr)

########################################################################################################### 
# in this script we calculate the accuracy values for each participant
########################################################################################################### 
d <- read.csv("all_data.csv")


########################################################################################################### 
# we first get the total observations per participant and group 
# mode ordered distance and if it was correct or not
########################################################################################################### 
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

write.csv(dat,file="data_accuracy.csv",row.names=FALSE,na="")

############################################################################################################
# get dual task accuracy.
############################################################################################################
dat <- d

dat <- dat %>% filter(practice == FALSE) %>% filter(mode == "dual_vis" | mode == "dual_phon")  %>%
    group_by(prob_code,mode,correct_dual)   %>% summarise( total_observations=n() ) 


dat$total_observations[is.na(dat$total_observations)] <- 0

dat <- data.frame(dat)

dat <- reshape(dat, idvar=c("prob_code","mode"), timevar = c("correct_dual"), direction="wide")



# replace na values with 0
dat$total_observations.FALSE[is.na(dat$total_observations.FALSE)] <- 0
dat$total_observations.TRUE[is.na(dat$total_observations.TRUE)] <- 0
dat$accuracy <- dat$total_observations.TRUE / (dat$total_observations.TRUE+dat$total_observations.FALSE)


write.csv(dat,file="data_accuracy_p_participant_dual.csv",row.names=FALSE,na="")

