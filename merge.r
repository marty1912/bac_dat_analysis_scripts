library(dplyr)
library(snpar)

get_dual_diff <- function(vec){

    # calculate the difficulty of a given dual stim. because i forgot to add it in the experiment
    char_vec <- as.character(vec)
    difficulty <- c()

    if (length(char_vec)== 0){
        return(difficulty)
    }
    for (i in 1:length(char_vec)) {
        #char_vec[i] <- char_vec[i]+i
        if(is.na(char_vec[i])){
            difficulty <- append(difficulty,c(NA))
        }
        else{
            str_as_char_vec <- strsplit(char_vec[i],"")[[1]]
            if("{" %in% str_as_char_vec){
                diff <- floor(length(which(str_as_char_vec == "{"))/2)
                difficulty <- append(difficulty,c(diff))
            }
            else{
                diff <- floor(length(str_as_char_vec)/2)
                difficulty <- append(difficulty,c(diff))
            }
        }
    }

    return(difficulty)
}

get_data <- function(prob_code,get_dual_diff){

    filenames <- list.files("data/",pattern="*.csv$",full.names=TRUE)

    # get a vec for every col
    rt_num <- c()
    correct_num <- c()

    ordered <- c()
    ascending <- c()
    descending <- c()
    distance <- c()
    datetime <- c()
    rt_dual <- c()
    correct_dual <- c()
    dual_stim <- c()
    mode <- c()
    framerate <- c()
    practice <- c()
    numbers <- c()
    rig_randomness_p <- NA
    rig_randomness_runs <- NA
    rig_randomness_1s <- NA
    rig_randomness_0s <- NA

    client_info <- c()

    for (filename in filenames){


        if(startsWith(basename(filename),prob_code))
        {


            message(paste("filename: ",filename))

            file <- read.csv(filename,sep=",",as.is=TRUE)
            fields <- names(file)
            if (grepl("single",basename(filename),fixed=TRUE))
            {

                if ( "rt" %in% names(file)){
                    rt_num <- append(rt_num,file$rt)
                    correct_num <- append(correct_num,file$correct)
                }
                else{
                    # if the vector does not exist add it.
                    rt_num <- append(rt_num,rep(NA,nrow(file)))
                    correct_num <- append(correct_num,rep(NA,nrow(file)))
                }

                rt_dual <- append(rt_dual,rep(NA,nrow(file)))
                correct_dual <- append(correct_dual,rep(NA,nrow(file)))
                ordered <- append(ordered ,file$ordered)
                numbers<- append(numbers,file$numbers)
                ascending <- append(ascending ,file$ascending)
                descending <- append(descending ,file$descending)
                distance <- append(distance ,file$distance)
                datetime <- append(datetime ,file$datetime)
                framerate <- append(framerate ,file$framerate)
                dual_stim <- append(dual_stim,rep(NA,nrow(file)))
                mode <- append(mode,rep("single",nrow(file)))
                if (nrow(file) == 4) { practice <- append(practice,rep(TRUE,nrow(file))) } else{ practice <- append(practice,rep(FALSE,nrow(file))) }

            }
            else if (grepl("dual_phon",basename(filename),fixed=TRUE))
            {

                if ( "rt_ord" %in% names(file)){
                    rt_num <- append(rt_num,file$rt_ord)
                    correct_num <- append(correct_num,file$correct_ord)
                }
                else{
                    # if the vector does not exist add it.
                    rt_num <- append(rt_num,rep(NA,nrow(file)))
                    correct_num <- append(correct_num,rep(NA,nrow(file)))
                }
                if ( "rt_dual" %in% names(file)){
                    rt_dual <- append(rt_dual,file$rt_dual)
                    correct_dual <- append(correct_dual,file$correct_dual)
                }
                else{
                    # if the vector does not exist add it.
                    rt_dual <- append(rt_dual,rep(NA,nrow(file)))
                    correct_dual <- append(correct_dual,rep(NA,nrow(file)))
                }

                ordered <- append(ordered ,file$ordered)
                numbers<- append(numbers,file$numbers)
                ascending <- append(ascending ,file$ascending)
                descending <- append(descending ,file$descending)
                distance <- append(distance ,file$distance)
                datetime <- append(datetime ,file$datetime)
                framerate <- append(framerate ,file$framerate)
                dual_stim <- append(dual_stim,file$dual_stim)
                mode <- append(mode,rep("dual_phon",nrow(file)))
                if (nrow(file) == 4) { practice <- append(practice,rep(TRUE,nrow(file))) } else{ practice <- append(practice,rep(FALSE,nrow(file))) }

            }
            else if (grepl("dual_vis",basename(filename),fixed=TRUE))
            {
                if ( "rt_ord" %in% names(file)){
                    rt_num <- append(rt_num,file$rt_ord)
                    correct_num <- append(correct_num,file$correct_ord)
                }
                else{
                    # if the vector does not exist add it.
                    rt_num <- append(rt_num,rep(NA,nrow(file)))
                    correct_num <- append(correct_num,rep(NA,nrow(file)))
                }

                if ( "rt_dual" %in% names(file)){
                    rt_dual <- append(rt_dual,file$rt_dual)
                    correct_dual <- append(correct_dual,file$correct_dual)
                }
                else{
                    # if the vector does not exist add it.
                    rt_dual <- append(rt_dual,rep(NA,nrow(file)))
                    correct_dual <- append(correct_dual,rep(NA,nrow(file)))
                }

                ordered <- append(ordered ,file$ordered)
                numbers<- append(numbers,file$numbers)
                ascending <- append(ascending ,file$ascending)
                descending <- append(descending ,file$descending)
                distance <- append(distance ,file$distance)
                datetime <- append(datetime ,file$datetime)
                framerate <- append(framerate ,file$framerate)
                dual_stim <- append(dual_stim,file$dual_stim)
                mode <- append(mode,rep("dual_vis",nrow(file)))
                if (nrow(file) == 4) { practice <- append(practice,rep(TRUE,nrow(file))) } else{ practice <- append(practice,rep(FALSE,nrow(file))) }
            }
            else if (grepl("dual_rig",basename(filename),fixed=TRUE))
            {
                if ( "rt" %in% names(file)){
                    rt_num <- append(rt_num,file$rt)
                    correct_num <- append(correct_num,file$correct)
                }
                else{
                    # if the vector does not exist add it.
                    rt_num <- append(rt_num,rep(NA,nrow(file)))
                    correct_num <- append(correct_num,rep(NA,nrow(file)))
                }

                rt_dual <- append(rt_dual,rep(NA,nrow(file)))
                correct_dual <- append(correct_dual,rep(NA,nrow(file)))
                ordered <- append(ordered ,file$ordered)
                ascending <- append(ascending ,file$ascending)
                numbers<- append(numbers,file$numbers)
                descending <- append(descending ,file$descending)
                distance <- append(distance ,file$distance)
                datetime <- append(datetime ,file$datetime)
                framerate <- append(framerate ,file$framerate)
                dual_stim <- append(dual_stim,rep(NA,nrow(file)))
                mode <- append(mode,rep("dual_rig",nrow(file)))
                if (nrow(file) == 4) { practice <- append(practice,rep(TRUE,nrow(file))) } else{ practice <- append(practice,rep(FALSE,nrow(file))) }

            }
            else if (grepl("client_info",basename(filename),fixed=TRUE))
            {
                client_info <- file
            }
            else if (grepl("stair_phon",basename(filename),fixed=TRUE))
            {
            }
            else if (grepl("stair_vis",basename(filename),fixed=TRUE))
            {
            }
            else if (grepl("rig_practice",basename(filename),fixed=TRUE))
            {
            }
            else if (grepl("rig",basename(filename),fixed=TRUE))
            {

                message(paste("filename: in rig ",filename))
                # TODO: get median distance. and use as by
                distances <- c()
                for (i in 1:(length(file$rt)-1)){
                    distances <- append(distances,(file$rt[i+1] - file$rt[i]))
                }


                is_in_interval_vec <- c()

                # TODO: check how many distances are above or below the median.
                print("rig test ")
                print("median")
                print(median(distances))
                # TODO: find the duration of the task
                # TODO: use max time

                # selector for one method or the other.
                if (FALSE){
                interval_vec <- seq(from = 0, to = max(file$rt), by = 1)#median(distances))
                for (i in 1:(length(interval_vec)-1)){

                    between <- file  %>% filter(rt < interval_vec[i+1]) %>% filter(rt > interval_vec[i])

                    if(nrow(between) != 0){

                    is_in_interval_vec <- append(is_in_interval_vec,1)
                    }
                    else{
                    is_in_interval_vec <- append(is_in_interval_vec,0)
                    }

                }
                }else{
                for( i in distances){
                    if(i < median(distances)){
                        is_in_interval_vec <- append(is_in_interval_vec,1)
                    }else{
                        is_in_interval_vec <- append(is_in_interval_vec,0)
                    }

                }
                }

                is_in_interval_vec <- unlist(is_in_interval_vec)
                print(is_in_interval_vec,use.names = FALSE)

                runs_test <- runs.test(is_in_interval_vec, exact = FALSE, alternative = c("two.sided"))
                print(runs_test)

                rig_randomness_p <- runs_test$p.value

                rig_randomness_runs <- runs_test$statistic
                rig_randomness_1s <- length(is_in_interval_vec[is_in_interval_vec == 1])
                rig_randomness_0s <- length(is_in_interval_vec[is_in_interval_vec == 0])
                print(rig_randomness_0s)
                print(rig_randomness_1s)
                print(rig_randomness_runs)
                print(names(runs_test))

            }


        }


    }

    # TODO: get other cols
    dat_demo <- read.csv("data/demo_data.csv")
    dat_demo <-  dat_demo %>% filter(probanden_code == prob_code)
    print(head(dat_demo))
    if(nrow(dat_demo) == 0){
        demo_sex <- NA
        demo_age <- NA
        demo_rl <- NA
        demo_drogen <- NA
        demo_alkohol <- NA
        demo_dyslex <- NA
        demo_adhs <- NA
    }
    else{
        demo_sex <- dat_demo$sex
        demo_age <- dat_demo$age
        demo_rl <- dat_demo$rechts_links
        demo_drogen <- dat_demo$drogen
        demo_alkohol <- dat_demo$alkohol
        demo_dyslex <- dat_demo$Dyslexie
        demo_adhs <- dat_demo$ADHS
    }

    demo_sex <- rep(demo_sex,length(rt_num))
    demo_age <- rep(demo_age,length(rt_num))
    demo_rl <- rep(demo_rl,length(rt_num))
    demo_drogen <- rep(demo_drogen,length(rt_num))
    demo_alkohol <- rep(demo_alkohol,length(rt_num))
    demo_dyslex <- rep(demo_dyslex,length(rt_num))
    demo_adhs <- rep(demo_adhs,length(rt_num))

    prob_code <- rep(prob_code,length(rt_num))
    rig_randomness_p <- rep(rig_randomness_p,length(rt_num))
    rig_randomness_runs <- rep(rig_randomness_runs,length(rt_num))
    rig_randomness_1s <- rep(rig_randomness_1s,length(rt_num))
    rig_randomness_0s <- rep(rig_randomness_0s,length(rt_num))




    data <- data.frame(prob_code, rt_num, correct_num,numbers, ordered, ascending, descending, distance, datetime, rt_dual, correct_dual, mode,dual_stim,practice,rig_randomness_p,rig_randomness_runs,rig_randomness_1s,rig_randomness_0s,demo_sex,demo_age,demo_rl,demo_drogen,demo_alkohol,demo_dyslex,demo_adhs)
    data["dual_diff"] <- get_dual_diff(data$dual_stim)

    if (length(client_info) != 0){
        data["os"] <- rep(client_info$os[1],nrow(data))
        data["screen_height"] <-rep( client_info$screen_height_t_ratio[1],nrow(data))
        data["screen_width"] <-rep( client_info$screen_width_t_ratio[1],nrow(data))
    }

    return(data)
}


get_unique_prob_codes <- function(){
    filenames <- list.files("data/",pattern="*.csv$",full.names=TRUE)
    all_prob_codes <- c()
    for (filename in filenames){

        #print(filename)
        file <- read.csv(filename,stringsAsFactors=FALSE,colClasses=c('character'))
        # we dont have a prob code for client info but it does not really matter because its in the filename
        if ("prob_code" %in% colnames(file)){
            prob_code <- toString(file$prob_code[1])
            if (prob_code != ""){
                all_prob_codes <- append(all_prob_codes,prob_code)
            }
        }

    }
    return(unique(all_prob_codes))
}


prob_codes <- get_unique_prob_codes()
all_data_frames <- list()
for (i in seq_along(prob_codes)){
    data <- get_data(prob_codes[i],get_dual_diff)
    #if(nrow(data) != 0){
    all_data_frames[[i]] <- data
    # }

}
first_df <- all_data_frames[[1]]

if (length(all_data_frames) > 1){
for (i in 2:length(all_data_frames)){
    first_df <- rbind(first_df,all_data_frames[[i]])
}
}

print(names(first_df))

write.csv(first_df,file="all_data.csv",row.names=FALSE,na="")
