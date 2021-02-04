plot_progress_2020 <- function(df){


	df$CANVAS_SECTION_MEMBERSHIPS.47C94A6F <- gsub("3825","",df$CANVAS_SECTION_MEMBERSHIPS.47C94A6F)
	df$CANVAS_SECTION_MEMBERSHIPS.47C94A6F <- str_extract(df$CANVAS_SECTION_MEMBERSHIPS.47C94A6F, "[0-9]{1,2}")
	df$CANVAS_SECTION_MEMBERSHIPS.47C94A6F <- as.factor(as.numeric(df$CANVAS_SECTION_MEMBERSHIPS.47C94A6F))

	df$CANVAS_SECTION_MEMBERSHIPS.47C94A6F <- factor(df$CANVAS_SECTION_MEMBERSHIPS.47C94A6F,levels(df$CANVAS_SECTION_MEMBERSHIPS.47C94A6F)[c(5, 6, 1, 2, 7, 8, 9, 10, 3, 4,11)])

	# df <- df[!df$CANVAS_SECTION_MEMBERSHIPS.47C94A6F=="Groupx",]

	####
	dat_file <- df 
	student_counselling <- as.data.frame(dat_file[,grepl('OVERALL',colnames(dat_file))])
	student_counselling <- as.data.frame(student_counselling[,grepl('Counselling',colnames(student_counselling))])
	student_counselling <- student_counselling[,1:ncol(student_counselling)]

	for(x in 1:ncol(student_counselling)){
		student_counselling[,x] <- ifelse(grepl("P",student_counselling[,x]),1,0)
	}

	counsel_full <- student_counselling[,1]
	for (i in 2:ncol(student_counselling)){
		store <- matrix(apply(student_counselling[,1:i], 1, sum))
		counsel_full <- cbind(counsel_full, store)
		passing_counsel <- matrix(apply(counsel_full, 2, function(x) sum(x>2)))
	}

	counsel_passing_rate <- cumsum(matrix(as.matrix(student_counselling)))
	counsel_passing_rate <-  data.frame(x = seq(1,length(counsel_passing_rate)), y = counsel_passing_rate)




	#####
	LabelNames <- colnames(df)[grepl('2.Labels\\.\\.\\.', colnames(df))] # Get the results of the Labels Only
	LabelNames <- LabelNames[!grepl('Comments', (LabelNames))] # Not the comments
	LabelNames <- LabelNames[!grepl('TOTAL', (LabelNames))] # Not the comments

	matrix(LabelNames)

	df_out<-df[LabelNames]

	for (i in 1:ncol(df_out)){
		df_out[,i] <- gsub("SP|SUP",1,df_out[,i])
		df_out[,i] <- gsub("U",0,df_out[,i])
		df_out[,i] <- as.numeric(df_out[,i])
		df_out[is.na(df_out[,i]),i] <- 0    
	}
	df_out <- as.data.frame(df_out)

	df_StudentScores <- cbind(df[,c("Preferred.name","Surname", "CANVAS_SECTION_MEMBERSHIPS.47C94A6F")], df_out)
	df_StudentScores <- df_StudentScores[order(df_StudentScores$CANVAS_SECTION_MEMBERSHIPS.47C94A6F),]

	for (i in 5:ncol(df_StudentScores)){
		# print(mean(as.numeric(as.matrix(df_StudentScores[,4:i])), na.rm=TRUE))
	}

	storefull <- df_StudentScores[,4]
	for (i in 5:ncol(df_StudentScores)){
		store <- matrix(apply(df_StudentScores[,4:i], 1, sum))
		storefull <- cbind(storefull, store)
		passing_students <- matrix(apply(storefull, 2, function(x) sum(x>9)))
	}

	passing_students_rate <-  data.frame(x = seq(1,length(passing_students)), y = passing_students)

	#########################


	gc_2020 <- counsel_passing_rate %>%
		ggplot(aes(x =x, y= y)) +
			geom_line() +
			geom_line(aes(y=x)) + 
			geom_smooth(method="loess") +  
			geom_vline(xintercept = 1032) + 
			theme_bw() +
			labs(title = "2020 Cumulative sum of passed counselling ") + 
			ylab("Total Passed Counselling Sessions") + 
			xlab("Number of Counselling Sessions Conducted")


	gp_2020 <- passing_students_rate %>%
		ggplot(aes(x =x, y= y)) +
			geom_line() +
			# geom_smooth(method="lm") +  
			theme_bw() +
			labs(title = "2020 Cumulative sum of passed students ") + 
			ylab("Total Students Passed") + 
			xlab("Number of Products Made") + 
			scale_x_continuous(breaks= seq(	1, 	nrow(passing_students_rate), 1))


	return(list(
		graph_2020_products = gp_2020,
		graph_2020_counselling = gc_2020
	))
}

