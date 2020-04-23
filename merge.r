library(dplyr)

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
    rt_ord <- c()
    correct_ord <- c()

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

    client_info <- c()

    for (filename in filenames){

        if(grepl(prob_code,basename(filename),fixed=TRUE))
        {
            file <- read.csv(filename,sep=",",as.is=TRUE)
            fields <- names(file)
            if (grepl("single",basename(filename),fixed=TRUE))
            {
                if ( "rt" %in% names("file")){
                    rt_ord <- append(file$rt)
                    correct_ord <- append(file$correct)
                }
                else{
                    # if the vector does not exist add it.
                    rt_ord <- append(rt_ord,rep(NA,nrow(file)))
                    correct_ord <- append(correct_ord,rep(NA,nrow(file)))
                }

                rt_dual <- append(rt_dual,rep(NA,nrow(file)))
                correct_dual <- append(correct_dual,rep(NA,nrow(file)))
                ordered <- append(ordered ,file$ordered)
                ascending <- append(ascending ,file$ascending)
                descending <- append(descending ,file$descending)
                distance <- append(distance ,file$distance)
                datetime <- append(datetime ,file$datetime)
                framerate <- append(framerate ,file$framerate)
                mode <- append(mode,rep("single",nrow(file)))

            }
            else if (grepl("dual_phon",basename(filename),fixed=TRUE))
            {
                if ( "rt_ord" %in% names("file")){
                    rt_ord <- append(file$rt_ord)
                    correct_ord <- append(file$correct_ord)
                }
                else{
                    # if the vector does not exist add it.
                    rt_ord <- append(rt_ord,rep(NA,nrow(file)))
                    correct_ord <- append(correct_ord,rep(NA,nrow(file)))
                }
                if ( "rt_dual" %in% names("file")){
                    rt_dual <- append(file$rt_dual)
                    correct_dual <- append(file$correct_dual)
                }
                else{
                    # if the vector does not exist add it.
                    rt_dual <- append(rt_dual,rep(NA,nrow(file)))
                    correct_dual <- append(correct_dual,rep(NA,nrow(file)))
                }

                ordered <- append(ordered ,file$ordered)
                ascending <- append(ascending ,file$ascending)
                descending <- append(descending ,file$descending)
                distance <- append(distance ,file$distance)
                datetime <- append(datetime ,file$datetime)
                framerate <- append(framerate ,file$framerate)
                dual_stim <- append(dual_stim,file$dual_stim)
                mode <- append(mode,rep("dual_phon",nrow(file)))

            }
            else if (grepl("dual_vis",basename(filename),fixed=TRUE))
            {
                if ( "rt_ord" %in% names("file")){
                    rt_ord <- append(file$rt_ord)
                    correct_ord <- append(file$correct_ord)
                }
                else{
                    # if the vector does not exist add it.
                    rt_ord <- append(rt_ord,rep(NA,nrow(file)))
                    correct_ord <- append(correct_ord,rep(NA,nrow(file)))
                }
                if ( "rt_dual" %in% names("file")){
                    rt_dual <- append(file$rt_dual)
                    correct_dual <- append(file$correct_dual)
                }
                else{
                    # if the vector does not exist add it.
                    rt_dual <- append(rt_dual,rep(NA,nrow(file)))
                    correct_dual <- append(correct_dual,rep(NA,nrow(file)))
                }

                ordered <- append(ordered ,file$ordered)
                ascending <- append(ascending ,file$ascending)
                descending <- append(descending ,file$descending)
                distance <- append(distance ,file$distance)
                datetime <- append(datetime ,file$datetime)
                framerate <- append(framerate ,file$framerate)
                dual_stim <- append(dual_stim,file$dual_stim)
                mode <- append(mode,rep("dual_vis",nrow(file)))
            }
            else if (grepl("dual_rig",basename(filename),fixed=TRUE))
            {
                if ( "rt" %in% names("file")){
                    rt_ord <- append(file$rt)
                    correct_ord <- append(file$correct)
                }
                else{
                    # if the vector does not exist add it.
                    rt_ord <- append(rt_ord,rep(NA,nrow(file)))
                    correct_ord <- append(correct_ord,rep(NA,nrow(file)))
                }

                rt_dual <- append(rt_dual,rep(NA,nrow(file)))
                correct_dual <- append(correct_dual,rep(NA,nrow(file)))
                ordered <- append(ordered ,file$ordered)
                ascending <- append(ascending ,file$ascending)
                descending <- append(descending ,file$descending)
                distance <- append(distance ,file$distance)
                datetime <- append(datetime ,file$datetime)
                framerate <- append(framerate ,file$framerate)
                dual_stim <- append(dual_stim,rep(NA,nrow(file)))
                mode <- append(mode,rep("dual_rig",nrow(file)))

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

        }


    }
    data <- data.frame( rt_ord, correct_ord, ordered, ascending, descending, distance, datetime, rt_dual, correct_dual, mode,dual_stim)
    prob_codes <- rep(prob_code,nrow(data))
    data["prob_code"] <- prob_codes
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
        file <- read.csv(filename)
        # we dont have a prob code for client info but it does not really matter because its in the filename
        if ("prob_code" %in% colnames(file)){
            prob_code <- toString(file$prob_code[1])
            all_prob_codes <- append(all_prob_codes,prob_code)
        }

    }
    return(unique(all_prob_codes))
}


prob_codes <- get_unique_prob_codes()
all_data_frames <- list()
for (i in seq_along(prob_codes)){
    data <- get_data(prob_codes[i],get_dual_diff)
    if(nrow(data) != 0){
        all_data_frames[[i]] <- data
    }
    
}
first_df <- all_data_frames[[1]]

for (i in 2:length(all_data_frames)){
    first_df <- rbind(first_df,all_data_frames[[i]])
}

print(names(first_df))

write.csv(first_df,file="all_data.csv",row.names=FALSE,na="")
