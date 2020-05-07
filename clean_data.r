library(dplyr)
library(dplyr)

dat <- read.csv("all_data.csv")
dat <- dat %>% filter(practice==FALSE)


# 4 conditions with:
# 4 practice per trial = 16
# 40 in the normal tasks = 160
# total 176
dat_n_trials <- dat %>% group_by(prob_code) %>% summarise(total_trials=n()) %>% filter(total_trials > 176)
write.csv(dat_n_trials,file="part_with_too_many_trials.csv",row.names=FALSE,na="")

dat_n_trials <- dat %>% group_by(prob_code) %>% summarise(total_trials=n()) %>% filter(total_trials < 176)
write.csv(dat_n_trials,file="part_canceled.csv",row.names=FALSE,na="")



# add a few variables we need to exclude people maybe
dat <- dat %>% group_by(prob_code) %>% mutate(total_trials=n()) %>% ungroup()
dat <- dat %>% group_by(prob_code,mode) %>% mutate(trials_p_mode=n()) %>% ungroup()
dat <- dat %>% mutate(screen_ratio=screen_width/screen_height) 

# exclusion because numeric task
dat_accuracy <- read.csv("data_accuracy.csv")
dat_accuracy <- dat_accuracy %>% group_by(prob_code,mode) %>% mutate(accuracy = mean(accuracy))
prob_modes_to_exclude_num <- dat_accuracy %>% filter(accuracy < 0.65) %>% select(prob_code,mode)
prob_modes_to_exclude_num <- unique(prob_modes_to_exclude_num)
print("excluded because numeric accuracy too low:")
print(prob_modes_to_exclude_num)
print(nrow(prob_modes_to_exclude_num))

# exclusion because dual task
dat_accuracy <- read.csv("data_accuracy_p_participant_dual.csv")
prob_modes_to_exclude_dual <- dat_accuracy %>% filter(accuracy < 0.65) %>% select(prob_code,mode)
prob_modes_to_exclude_dual <- unique(prob_modes_to_exclude_dual)
print("excluded because dual accuracy too low:")
print(prob_modes_to_exclude_dual)
print(nrow(prob_modes_to_exclude_dual))

dat_exclude <- rbind(as.data.frame(prob_modes_to_exclude_num),as.data.frame(prob_modes_to_exclude_dual)) %>% select(prob_code,mode)
dat_exclude <- unique(dat_exclude)

print("excluded total:")
print(dat_exclude)
print(nrow(dat_exclude))

filter_out_prob_codes_per_mode <- function(data ,exclude_prob_code_mode){
    for (m in unique(exclude_prob_code_mode$mode)){
        # for every mode we do the same thing:
        exclude_part <- exclude_prob_code_mode %>% filter(mode == m)
        for (p in unique(exclude_part$prob_code)){
            # exclude the rows where prob_code is our prob code and mode is the mode where he/she wasnt good
            data <- data %>% filter(!(prob_code == p & mode == m))
        }
    }

    return(data)
}

# filter based on unrealistic reaction times
dat <- filter_out_prob_codes_per_mode(dat,dat_exclude)
dat <- dat %>% filter(is.na(rt_num) |  rt_num > 0.150)  %>% filter(is.na(rt_dual) | rt_dual > 0.150)

mean_rt_num = mean(dat$rt_num,na.rm=TRUE)
mean_rt_dual = mean(dat$rt_dual,na.rm=TRUE)

sd_rt_num = sd(dat$rt_num,na.rm=TRUE)
sd_rt_dual = sd(dat$rt_dual,na.rm=TRUE)


dat <- dat %>% filter(is.na(rt_num) |  ((rt_num > mean_rt_num -3*sd_rt_num) & (rt_num < mean_rt_num +3*sd_rt_num)))  #%>% 
#    filter(is.na(rt_dual) |  ((rt_dual > mean_rt_dual -3*sd_rt_dual) & (rt_dual < mean_rt_dual +3*sd_rt_dual))) 

# TODO: dual für phon vis getrennt ausschließen!!
# TODO: get list of excluded participants.

write.csv(dat,file="data_cleaned.csv",row.names=FALSE,na="")
