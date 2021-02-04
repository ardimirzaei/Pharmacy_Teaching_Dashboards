plot_progress_2019 <- function(df){


	# levels(df$CANVAS_SECTION_MEMBERSHIPS.AC53D7C5) <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "Groupx")

	df$CANVAS_SECTION_MEMBERSHIPS.AC53D7C5 <- gsub("3825","",df$CANVAS_SECTION_MEMBERSHIPS.AC53D7C5)
	df$CANVAS_SECTION_MEMBERSHIPS.AC53D7C5 <- str_extract(df$CANVAS_SECTION_MEMBERSHIPS.AC53D7C5, "[0-9]{1,2}")
	df$CANVAS_SECTION_MEMBERSHIPS.AC53D7C5 <- as.factor(as.numeric(df$CANVAS_SECTION_MEMBERSHIPS.AC53D7C5))

	df$CANVAS_SECTION_MEMBERSHIPS.AC53D7C5 <- factor(df$CANVAS_SECTION_MEMBERSHIPS.AC53D7C5,levels(df$CANVAS_SECTION_MEMBERSHIPS.AC53D7C5)[c(5, 6, 1, 2, 7, 8, 9, 10, 3, 4,11)])



	# levels(df$CANVAS_SECTION_MEMBERSHIPS.AC53D7C5)[c(5, 6, 1, 2, 7, 8, 9, 10, 3, 4,11)]

	# df$CANVAS_SECTION_MEMBERSHIPS.AC53D7C5 <- factor(df$CANVAS_SECTION_MEMBERSHIPS.AC53D7C5,levels(df$CANVAS_SECTION_MEMBERSHIPS.AC53D7C5)[c(5, 6, 1, 2, 7, 8, 9, 10, 3, 4,11)])

	df <- df[!df$CANVAS_SECTION_MEMBERSHIPS.AC53D7C5=="Groupx",]

	####
	dat_file <- df 
	student_counselling <- as.data.frame(dat_file[,grepl('OVERALL',colnames(dat_file))])
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

	df_StudentScores <- cbind(df[,c("Preferred.name","Surname", "CANVAS_SECTION_MEMBERSHIPS.AC53D7C5")], df_out)
	df_StudentScores <- df_StudentScores[order(df_StudentScores$CANVAS_SECTION_MEMBERSHIPS.AC53D7C5),]

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


	gc_2019 <- counsel_passing_rate %>%
		ggplot(aes(x =x, y= y)) +
			geom_line() +
			geom_line(aes(y=x)) + 
			geom_smooth(method="loess") +  
			geom_vline(xintercept = 528) + 
			theme_bw() +
			labs(title = "2019 Cumulative sum of passed counselling ") + 
			ylab("Total Passed Counselling Sessions") + 
			xlab("Number of Counselling Sessions Conducted")

	gp_2019 <- passing_students_rate %>%
		ggplot(aes(x =x, y= y)) +
			geom_line() +
			# geom_smooth(method="lm") +  
			theme_bw() +
			labs(title = "2019 Cumulative sum of passed students ") + 
			ylab("Total Students Passed") + 
			xlab("Number of Products Made") + 
			scale_x_continuous(breaks= seq(	1, 	nrow(passing_students_rate), 1))




	return(list(
		graph_2019_products = gp_2019,
		graph_2019_counselling = gc_2019
	))
}



