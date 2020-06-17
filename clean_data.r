library(dplyr)
library(rstatix)

# filter_out_prob_codes_per_mode
# @param data - the dataframe where we want to exlcude the participant per mode
# @param exclude_prob_code_mode - dataframe with prob_code and mode as cols (the ones you want to exclude)
#
# @ret - the dataframe without the participants (in modes) that are in exclude_prob_code_mode
#

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


# we write our output to the file exclude part.txt log file. so we can check stuff easily later.
sink("exclude_part.txt")
dat <- read.csv("all_data.csv")


########################################################################################################### 
# get some info about our data here..
########################################################################################################### 

print("demo data")
dat_part_info <- dat %>% select(prob_code,demo_age,demo_sex)
# get one row per participant.
dat_part_info <- dat_part_info %>% distinct(prob_code,.keep_all=TRUE)
print(dat_part_info)

print("mean age:")
mean(dat_part_info$demo_age,na.rm=TRUE) %>% print()
print("sd age:")
sd(dat_part_info$demo_age,na.rm=TRUE) %>% print()

print("sex:")
dat_part_info %>% count(demo_sex)



########################################################################################################### 
# demographic data
########################################################################################################### 

print("demo age na ")
dat %>% filter(is.na(demo_age)) %>% distinct(prob_code) %>% print() 
dat %>% filter(is.na(demo_age)) %>% distinct(prob_code) %>% nrow() %>% print() 
dat <- dat %>% filter(!is.na(demo_age))

print("(demo_age <= 30)")
dat %>% filter(!(demo_age <= 30)) %>% distinct(prob_code) %>% print() 
dat %>% filter(!(demo_age <= 30)) %>% distinct(prob_code) %>% nrow() %>% print() 
dat <- dat %>% filter(demo_age <= 30)

print("(demo_age >= 18)")
dat %>% filter(!(demo_age >= 18)) %>% distinct(prob_code) %>% print() 
dat %>% filter(!(demo_age >= 18)) %>% distinct(prob_code) %>% nrow() %>% print() 
dat <- dat %>% filter(demo_age >= 18)

print("(demo_rl == Rechtshänder)")
dat %>% filter(!(demo_rl == "Rechtshänder")) %>% distinct(prob_code) %>% print() 
dat %>% filter(!(demo_rl == "Rechtshänder")) %>% distinct(prob_code) %>% nrow() %>% print() 
dat <- dat %>% filter(demo_rl == "Rechtshänder")

print("(demo_drogen == Nein)")
dat %>% filter(!(demo_drogen == "Nein")) %>% distinct(prob_code) %>% print() 
dat %>% filter(!(demo_drogen == "Nein")) %>% distinct(prob_code) %>% nrow() %>% print() 
dat <- dat %>% filter(demo_drogen == "Nein")

print("(demo_alkohol == Nein)")
dat %>% filter(!(demo_alkohol == "Nein")) %>% distinct(prob_code) %>% print() 
dat %>% filter(!(demo_alkohol == "Nein")) %>% distinct(prob_code) %>% nrow() %>% print() 
dat <- dat %>% filter(demo_alkohol == "Nein")

print("(demo_adhs == Nein)")
dat %>% filter(!(demo_adhs == "Nein")) %>% distinct(prob_code) %>% print() 
dat %>% filter(!(demo_adhs == "Nein")) %>% distinct(prob_code) %>% nrow() %>% print() 
dat <- dat %>% filter(demo_adhs == "Nein")

print("(demo_dyslex== Nein)")
dat %>% filter(!(demo_dyslex== "Nein")) %>% distinct(prob_code) %>% print() 
dat %>% filter(!(demo_dyslex== "Nein")) %>% distinct(prob_code) %>% nrow() %>% print() 
dat <- dat %>% filter(demo_dyslex== "Nein")


print("excluded demo total:")
read.csv("all_data.csv") %>% distinct(prob_code) %>% filter(!(prob_code %in% dat$prob_code)) %>% nrow() %>% print()


########################################################################################################### 
# participants who canceled:                             
########################################################################################################### 

# 4 conditions with:
# 4 practice per trial = 16
# 40 in the normal tasks = 160
# total 176
dat_n_trials <- dat %>% group_by(prob_code) %>% summarise(total_trials=n()) %>% filter(total_trials > 176)
print("too many trials:")
print(dat_n_trials)
dat <- dat %>% filter(!(prob_code %in% dat_n_trials$prob_code))

dat_n_trials <- dat %>% group_by(prob_code) %>% summarise(total_trials=n()) %>% filter(total_trials < 176)
print("canceled:")
print(dat_n_trials)

########################################################################################################### 
# add helper vars
########################################################################################################### 

# add a few variables we need to exclude people maybe
dat <- dat %>% group_by(prob_code) %>% mutate(total_trials=n()) %>% ungroup()
dat <- dat %>% group_by(prob_code,mode) %>% mutate(trials_p_mode=n()) %>% ungroup()
dat <- dat %>% mutate(screen_ratio=screen_width/screen_height) 


########################################################################################################### 
# only correct trials and no practice trials.                             
########################################################################################################### 

dat <- dat %>% filter(correct_num == TRUE)
dat <- dat %>% filter(practice == FALSE)

########################################################################################################### 
# exclusion because low accuracy in numeric task
########################################################################################################### 

dat_accuracy <- read.csv("data_accuracy_part_mode.csv")

prob_modes_to_exclude_num <- dat_accuracy %>% filter(accuracy < 0.60) %>% select(prob_code,mode)
prob_modes_to_exclude_num <- unique(prob_modes_to_exclude_num)
print("excluded because numeric accuracy too low:")
#print(prob_modes_to_exclude_num)
print(nrow(prob_modes_to_exclude_num))

########################################################################################################### 
# exclusion because low accuracy in dual task
########################################################################################################### 
dat_accuracy <- read.csv("data_accuracy_p_participant_dual.csv")
prob_modes_to_exclude_dual <- dat_accuracy %>% filter(accuracy < 0.60) %>% select(prob_code,mode)
prob_modes_to_exclude_dual <- unique(prob_modes_to_exclude_dual)
print("excluded because dual accuracy too low:")
#print(prob_modes_to_exclude_dual)
print(nrow(prob_modes_to_exclude_dual))

########################################################################################################### 
# filter partincipants per mode
########################################################################################################### 

dat_exclude <- rbind(as.data.frame(prob_modes_to_exclude_num),as.data.frame(prob_modes_to_exclude_dual)) %>% select(prob_code,mode)
dat_exclude <- unique(dat_exclude)

print("excluded blocks (one mode) total ")
#print(dat_exclude)
print(nrow(dat_exclude))
write.csv(dat_exclude,file="excluded_participants_p_mode.csv",row.names=FALSE,na="")


# filter based on unrealistic reaction times
dat <- filter_out_prob_codes_per_mode(dat,dat_exclude)



########################################################################################################### 
# filter outliers in rt data. for dual task and numeric
########################################################################################################### 

# exclude rts which are more than 3 sds from the mean

mean_rt_num = mean(dat$rt_num,na.rm=TRUE)
mean_rt_dual_phon = mean(dat$rt_dual[(dat$mode == "dual_phon")] ,na.rm=TRUE)
mean_rt_dual_vis = mean(dat$rt_dual[(dat$mode == "dual_vis")],na.rm=TRUE)

sd_rt_num = sd(dat$rt_num,na.rm=TRUE)
sd_rt_dual_phon = sd(dat$rt_dual[(dat$mode == "dual_phon") ],na.rm=TRUE)
sd_rt_dual_vis = sd(dat$rt_dual[(dat$mode == "dual_vis") ],na.rm=TRUE)


# exclude trials with rt more than 3 sds from the mean. 
dat <- dat %>% filter(!is.na(rt_num) &  ((rt_num > mean_rt_num -3*sd_rt_num) & (rt_num < mean_rt_num +3*sd_rt_num)))  %>% 
    filter((mode != "dual_phon")|(!is.na(rt_dual) 
                                  &  ((rt_dual> mean_rt_dual_phon -3*sd_rt_dual_phon) 
                                      & (rt_dual< mean_rt_dual_phon +3*sd_rt_dual_phon)))) %>%
filter((mode != "dual_vis")|(!is.na(rt_dual) 
                             &  ((rt_dual> mean_rt_dual_vis -3*sd_rt_dual_vis) 
                                 & (rt_dual< mean_rt_dual_vis +3*sd_rt_dual_vis))))


########################################################################################################### 
# exclude trials with rig randomness (sig) or NA 
# we leave the significant ones in because it does not matter in the rts.
########################################################################################################### 
dat <- dat %>% filter(( mode != "dual_rig") | (!is.na(rig_randomness_p) ))# &  (rig_randomness_p >= 0.05 )

########################################################################################################### 
# write data
########################################################################################################### 

write.csv(dat,file="data_cleaned.csv",row.names=FALSE,na="")

########################################################################################################### 
# print some more stats about the cleaning process
########################################################################################################### 

print("excluded participants total:")
read.csv("all_data.csv") %>% distinct(prob_code) %>% filter(!(prob_code %in% dat$prob_code)) %>% nrow() %>% print()


print("n participants before clean:")
read.csv("all_data.csv") %>% distinct(prob_code)  %>% nrow() %>% print()

print("n participants after clean:")
dat %>% distinct(prob_code)  %>% nrow() %>% print()


dat_single <- dat %>% filter(mode == "single" ) %>% distinct(prob_code)  
dat_dual_phon <- dat %>% filter(mode == "dual_phon" ) %>% distinct(prob_code)  
dat_dual_vis <- dat %>% filter(mode == "dual_vis" ) %>% distinct(prob_code)  
dat_dual_rig <- dat %>% filter(mode == "dual_rig" ) %>% distinct(prob_code)  

print("n participants after clean (single and dual phon):") 
dat %>% distinct(prob_code) %>% filter((prob_code %in% dat_single$prob_code) & (prob_code %in% dat_dual_phon$prob_code)) %>% nrow() %>% print()


print("n participants after clean (single and dual vis):") 
dat %>%  distinct(prob_code) %>% filter((prob_code %in% dat_single$prob_code) & (prob_code %in% dat_dual_vis$prob_code)) %>% nrow() %>% print()

print("n participants after clean (single and dual rig):") 
dat %>%  distinct(prob_code) %>% filter((prob_code %in% dat_single$prob_code) & (prob_code %in% dat_dual_rig$prob_code)) %>% nrow() %>% print()

sink()

