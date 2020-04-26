library(dplyr)

dat <- read.csv("all_data.csv")

unique_prob_codes <- unique(dat$prob_code)

dat <- dat %>% filter(practice == FALSE)  %>% 
    group_by(prob_code,mode,ordered,distance,correct_ord)   %>% summarise( total_observations=n() ) 



dat <- data.frame(dat)

dat <- reshape(dat, idvar=c("prob_code","mode","ordered","distance"), timevar = c("correct_ord"), direction="wide")


# replace na values with 0
dat$total_observations.FALSE[is.na(dat$total_observations.FALSE)] <- 0
dat$total_observations.TRUE[is.na(dat$total_observations.TRUE)] <- 0
dat$accuracy <- dat$total_observations.TRUE / (dat$total_observations.TRUE+dat$total_observations.FALSE)

print(dat)
write.csv(dat,file="data_accuracy.csv",row.names=FALSE,na="")

dat <- read.csv("all_data.csv")

unique_prob_codes <- unique(dat$prob_code)

dat <- dat %>% filter(practice == FALSE)  %>% 
    group_by(prob_code,correct_ord)   %>% summarise( total_observations=n() ) 
#    group_by(prob_code,mode,ordered,distance,correct_ord)   %>% summarise( total_observations=n() ) 



dat <- data.frame(dat)

#dat <- reshape(dat, idvar=c("prob_code","mode","ordered","distance"), timevar = c("correct_ord"), direction="wide")
dat <- reshape(dat, idvar=c("prob_code"), timevar = c("correct_ord"), direction="wide")


# replace na values with 0
dat$total_observations.FALSE[is.na(dat$total_observations.FALSE)] <- 0
dat$total_observations.TRUE[is.na(dat$total_observations.TRUE)] <- 0
dat$accuracy <- dat$total_observations.TRUE / (dat$total_observations.TRUE+dat$total_observations.FALSE)

print(dat)
write.csv(dat,file="data_accuracy_p_participant.csv",row.names=FALSE,na="")

dat <- read.csv("all_data.csv")

unique_prob_codes <- unique(dat$prob_code)

dat <- dat %>% filter(practice == FALSE)  %>% 
    group_by(prob_code,correct_dual)   %>% summarise( total_observations=n() ) 
#    group_by(prob_code,mode,dualered,distance,correct_dual)   %>% summarise( total_observations=n() ) 



dat <- data.frame(dat)

#dat <- reshape(dat, idvar=c("prob_code","mode","dualered","distance"), timevar = c("correct_dual"), direction="wide")
dat <- reshape(dat, idvar=c("prob_code"), timevar = c("correct_dual"), direction="wide")


# replace na values with 0
dat$total_observations.FALSE[is.na(dat$total_observations.FALSE)] <- 0
dat$total_observations.TRUE[is.na(dat$total_observations.TRUE)] <- 0
dat$accuracy <- dat$total_observations.TRUE / (dat$total_observations.TRUE+dat$total_observations.FALSE)

print(dat)
write.csv(dat,file="data_accuracy_p_participant_dual.csv",row.names=FALSE,na="")
