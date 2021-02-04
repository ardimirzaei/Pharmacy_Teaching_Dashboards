students_passing_counselling <- function(df){
	dat_file <- df 
	student_counselling <- as.data.frame(dat_file[,grepl('Cou.+OVERALL',colnames(dat_file))])
	student_counselling <- student_counselling[,4:ncol(student_counselling)]

	for(x in 1:ncol(student_counselling)){
		student_counselling[,x] <- ifelse(grepl("P",student_counselling[,x]),1,0)
	}

	return (list(
		rate = round(sum(apply(student_counselling,1,function(x) sum(x, na.rm=TRUE))>1, na.rm=TRUE)/nrow(student_counselling)*100,2),
		count = sum(apply(student_counselling,1,function(x) sum(x, na.rm=TRUE))>1, na.rm=TRUE)
		)
	)

}



student_reflection <- function(df,reflec_week){
	reflection_columns <- as.matrix(df[,grepl('Rate',colnames(df))])

	for (x in 1:5) {
		reflection_columns[,x] <- as.numeric(gsub("[^0-9]","",reflection_columns[,x]))	
	}

	reflection_columns <- as.data.frame(reflection_columns)
	colnames(reflection_columns) <- c("Reflection1","Reflection2","Reflection3","Reflection4", "Reflection5")
	levels(reflection_columns[,1]) = c("Poor", "Need Improvement","Did OK","Fantastic") 
	levels(reflection_columns[,2]) = c("Poor", "Need Improvement","Did OK","Fantastic") 
	levels(reflection_columns[,3]) = c("Poor", "Need Improvement","Did OK","Fantastic") 
	levels(reflection_columns[,4]) = c("Poor", "Need Improvement","Did OK","Fantastic")
	levels(reflection_columns[,5]) = c("Poor", "Need Improvement","Did OK","Fantastic")
	
	reflection_summary <- reflection_columns %>% 
		gather() %>% 
		group_by(key) %>% 
		table() %>% 
		as.data.frame() 

	colnames(reflection_summary) <- c("Reflection","Score","Frequency")
	levels(reflection_summary[,2]) = c("Poor", "Need Improvement","Did OK","Fantastic")
	reflection_summary[,2] <- factor(reflection_summary[,2],levels(reflection_summary[,2])[c(4,3,2,1)])

	if (reflec_week==1){ reflection_summaryWeek <- reflection_summary[reflection_summary$Reflection=='Reflection1',]} 
	if (reflec_week==2){ reflection_summaryWeek <- reflection_summary[reflection_summary$Reflection=='Reflection2',]} 
	if (reflec_week==3){ reflection_summaryWeek <- reflection_summary[reflection_summary$Reflection=='Reflection3',]} 
	if (reflec_week==4){ reflection_summaryWeek <- reflection_summary[reflection_summary$Reflection=='Reflection4',]} 
	if (reflec_week==5){ reflection_summaryWeek <- reflection_summary[reflection_summary$Reflection=='Reflection5',]} 


	# if (reflec_week==1){ reflection_summary <- reflection_summary[reflection_summary$Reflection=='Reflection1',]} 
	# if (reflec_week==2){ reflection_summary <- reflection_summary[reflection_summary$Reflection=='Reflection2',]} 
	# if (reflec_week==3){ reflection_summary <- reflection_summary[reflection_summary$Reflection=='Reflection3',]} 
	# if (reflec_week==4){ reflection_summary <- reflection_summary[reflection_summary$Reflection=='Reflection4',]} 
	# if (reflec_week==5){ reflection_summary <- reflection_summary[reflection_summary$Reflection=='Reflection5',]} 

					# Gren		Blue		Purple	Red
	myColors <- c("#00BA38", "#619CFF", "#ea61ff", "#F8766D")

	b <- reflection_summary %>% 
		ggplot(aes(x="Reflection",y=Frequency, fill=Score)) + 
		geom_col(width = 0.5) +
		scale_fill_manual(values=myColors) +
		coord_flip() + 
		theme_classic() + 
		theme(axis.title.y = element_blank(), 
			axis.title.x = element_blank(),
			legend.title = element_blank(),
			legend.position = "top") +
		# guides(fill = guide_legend(reverse = TRUE)) +
		facet_grid(cols=vars(Reflection)) +
		labs(title="Self reflection scoring by students")


	return(b)
}




