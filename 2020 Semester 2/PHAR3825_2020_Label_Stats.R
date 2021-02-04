compute_StudentScores <- function(df){
  
  LabelNames <- colnames(df)[grepl('2.Labels\\.\\.\\.', colnames(df))] # Get the results of the Labels Only
  LabelNames <- LabelNames[!grepl('Comments', (LabelNames))] # Not the comments
  LabelNames <- LabelNames[!grepl('TOTAL', (LabelNames))] # Not the comments
  
  matrix(LabelNames)
  
  df_out<-df[LabelNames]
  
  SP<-apply(df_out,1,function(x) sum(x=="SP", na.rm=TRUE)) 
  SUP<-apply(df_out,1,function(x) sum(x=="SUP", na.rm=TRUE))
  U<-apply(df_out,1,function(x) sum(x=="U", na.rm=TRUE))
  
  df_StudentScores <- cbind(df[,c("Preferred.name","Surname", "CANVAS_SECTION_MEMBERSHIPS.47C94A6F")], SP)
  df_StudentScores <- cbind(df_StudentScores, SUP)
  df_StudentScores <- cbind(df_StudentScores, U)
  df_StudentScores <- df_StudentScores[-nrow(df_StudentScores),] # Remove the dummy entry pharmacy student
  
  # Needs the lookup table already imported.
  # 2020 Version
    
  df_StudentScores$CANVAS_SECTION_MEMBERSHIPS.47C94A6F <- gsub("3825","",df_StudentScores$CANVAS_SECTION_MEMBERSHIPS.47C94A6F)
  df_StudentScores$CANVAS_SECTION_MEMBERSHIPS.47C94A6F <- str_extract(df_StudentScores$CANVAS_SECTION_MEMBERSHIPS.47C94A6F, "[0-9]{1,2}")
  df_StudentScores$CANVAS_SECTION_MEMBERSHIPS.47C94A6F <- as.factor(as.numeric(df_StudentScores$CANVAS_SECTION_MEMBERSHIPS.47C94A6F))
  # levels(df_StudentScores$CANVAS_SECTION_MEMBERSHIPS.47C94A6F) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "Groupx")
  df_StudentScores <- merge(df_StudentScores,lookup[,c('O','D')], by.x=c("CANVAS_SECTION_MEMBERSHIPS.47C94A6F"), by.y=c("O"))
  
  df_StudentScores$top_scoring <- df_StudentScores$SP+df_StudentScores$SUP

  return(df_StudentScores)
}

Label_Stats <- function(df){
  
  df_StudentScores <- compute_StudentScores(df)
  
  top_students <- df_StudentScores[df_StudentScores$top_scoring==max(df_StudentScores$top_scoring, na.rm=TRUE),'Surname']
  top_score <- max(df_StudentScores$top_scoring, na.rm=TRUE)
  lowest_score <- max(df_StudentScores$U, na.rm=TRUE)
  n_students_With_low_score <- sum(df_StudentScores$U==max(df_StudentScores$U, na.rm=TRUE), na.rm=TRUE)


 lowest_group <- df_StudentScores %>% 
    group_by(D) %>% 
    summarise(count=mean(U), na.rm=TRUE) 
  
  highest_group <- df_StudentScores %>% 
    group_by(D) %>% 
    summarise(count=mean(SP, na.rm=TRUE)) 
  
  return(list(top_students = top_students,
              top_score = top_score, 
              lowest_Score = lowest_score, 
              n_students_lowscore = n_students_With_low_score,
              worst_group = lowest_group[order(lowest_group$count, decreasing = TRUE)[1],],
              best_group = highest_group[order(highest_group$count, decreasing = TRUE)[1],],
              mean_group_SP = mean(highest_group[2][[1]], na.rm=TRUE),
              mean_group_U = mean(lowest_group[2][[1]], na.rm=TRUE)
              ))
}

CompletionRates <- function(df){
  
  LabelNames <- colnames(df)[grepl('2.Labels\\.\\.', colnames(df))] # Get the results of the Labels Only
  LabelNames <- LabelNames[!grepl('Comments', (LabelNames))] # Not the comments
  LabelNames <- LabelNames[!grepl('TOTAL', (LabelNames))] # Not the comments
  
  matrix(LabelNames)
  
  df_out<-df[LabelNames]
  
  rate_for_schedule <- matrix(c('Session\\.1', 'Session\\.2', 'Session\\.3', 'Session\\.4', 'Session\\.5'))
  
  for (schedule_name in rate_for_schedule){
    schedule_matrix <- (df[colnames(df_out)[grepl(schedule_name, colnames(df_out))] ])
    completed_products <- sum(!(df[colnames(df_out)[grepl(schedule_name, colnames(df_out))] ])=="", na.rm=TRUE)
    rate_for_schedule[rate_for_schedule==schedule_name] <- round(completed_products/(ncol(schedule_matrix)*nrow(schedule_matrix))*100,2)
  }
  
  rate_for_schedule <- as.data.frame(rate_for_schedule)
  rownames(rate_for_schedule)[] <- c('Session 1', 'Session 2', 'Session 3', 'Session 4', 'Session 5')
  colnames(rate_for_schedule)[] <- "Percent"
  
  return(list(schedule_completion_rate = rate_for_schedule))
}
