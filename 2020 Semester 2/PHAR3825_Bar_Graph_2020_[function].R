ardiPHAR3825_2020_Bar_Graph<-function (x){
	df=x
	#INIT

	#df <- read.csv(file.choose())
	df <- df[!df[,1]=="Pharmacy",]

	require(plotly)
	require(dplyr)


	ColDrugNameis<-c(
	"Perindopril",
	"Metformin",
	"Symbicort",
	"Ventolin",
	"Warfarin",
	"Esomeprazole",
	"Atorvastatin",
	"Paracetamol.and.codeine",
	"Cefalexin",
	"Levothyroxine",
	"Celecoxib",
	"Oxycodone.and.naloxone",
	"Levonorgestrol.ethinylestradiol",
	"Amlodipine",
	"Gliclazide",
	"Clopidogrel",
	"Doxycycline",
	"Triotropium.bromide",
	"Irebsartan..Hyrochlorthiazide",
	"Amitryptyline",
	"Prednisolone"

	)

	LabelNames <- colnames(df)[grepl('2.Labels\\.\\.\\.', colnames(df))] # Get the results of the Labels Only
	LabelNames <- LabelNames[!grepl('Comments', (LabelNames))] # Not the comments
	LabelNames <- LabelNames[!grepl('TOTAL', (LabelNames))] # Not the comments

	matrix(LabelNames)

	df_out<-df[LabelNames]
	colnames(df_out)[]<-ColDrugNameis # Load from another place


	SP<-apply(df_out,2,function(x) sum(x=="SP"))
	SUP<-apply(df_out,2,function(x) sum(x=="SUP"))
	U<-apply(df_out,2,function(x) sum(x=="U"))
	df_bind<-rbind(rbind(SP,SUP),U)
	df_bind<-as.data.frame(t(df_bind))
	#Make sure to fix facotr details. 
	p <- plot_ly(df_bind, x = factor(rownames(df_bind), levels=rownames(df_bind)), y = ~SP, type = 'bar', name = 'Satisfactory ',marker = list(color = c("green")),text = SP, textposition = 'auto') %>%
		add_trace(y = ~SUP, name = 'S/Unprofessional',marker = list(color = c("orange")),text = SUP, textposition = 'auto') %>%
		add_trace(y = ~U, name = 'Unsatisfactory',marker = list(color = c("silver")),text = U, textposition = 'auto') %>%
		layout(	yaxis = list(title = 'Count'), barmode = 'stack',

				xaxis = list(title = 'Drug'))

return(p)
}