source("PHAR3825_2020_Naming.R")

# COUNSELLING PART
ardiPHAR3825_2020_Counsel_Table<-function (x){
	df=x
require(plotly)
require(dplyr)

if(sum(ls()=="ColNameIS")==0){
	source("PHAR3825_2020_Naming.R")
}

CounselNames<-colnames(df)[grepl('Counselling', colnames(df))]
CounselNames <- CounselNames[!grepl('COMMENTS', (CounselNames))] # Not the comments
CounselNames<-CounselNames[grepl('OVERALL|COMMUNICATION|CONTENT', (CounselNames))] 

df_out<-df[CounselNames]
colnames(df_out)[]<-ColCounsel[1:ncol(df_out)] #Loadfrom antoher place

CoStore<-paste0(rep('Co',1,6),seq(1,6,1)) # For Comms
CStore<-paste0(rep('C',1,13),seq(1,13,1)) # For Content

SP<-apply(df_out,2,function(x) sum(x=="SP", na.rm=TRUE))
SUP<-apply(df_out,2,function(x) sum(x=="SUP"))
U<-apply(df_out,2,function(x) sum(x=="U"))
df_bind<-rbind(rbind(SP,SUP),U)

for ( y in CStore){
	Dump<-(apply(df_out,2,function(x) sum(grepl(y,x))))
	df_bind<-rbind(df_bind,Dump)
}
rownames(df_bind)[(nrow(df_bind)-length(CStore)+1):nrow(df_bind)]<-CStore

for ( y in CoStore){
	Dump<-(apply(df_out,2,function(x) sum(grepl(y,x))))
	df_bind<-rbind(df_bind,Dump)
}

rownames(df_bind)[(nrow(df_bind)-length(CoStore)+1):nrow(df_bind)]<-CoStore

ActiveColumns<-!is.na((apply(df_bind,2,function(x) sum(x)))>0) #Find the active cells)

# df_bind<-as.data.frame(df_bind[,ActiveColumns])
return(df_bind)
return(CStore)
return(CoStore)
}

ardiPHAR3825_2020_Counsel_Graph<-function(x){
	#Assumens df_bind_Results
	df_results_plotly<-as.data.frame(t(x))

	p <- plot_ly(df_results_plotly, x = ~rownames(df_results_plotly), y = df_results_plotly$SP, type = 'bar', name = 'Satisfactory ',marker = list(color = c("green")),text = df_results_plotly$SP, textposition = 'auto') %>%
		add_trace(y = df_results_plotly$SUP, name = 'S/Unprofessional',marker = list(color = c("orange")),text = df_results_plotly$SUP, textposition = 'auto') %>%
		add_trace(y = df_results_plotly$U, name = 'Unsatisfactory',marker = list(color = c("silver")),text = df_results_plotly$U, textposition = 'auto') %>%
		layout(	yaxis = list(title = 'Count'), barmode = 'stack',
				xaxis = list(title = 'Session'))
	return(p)
}

# # Results
# df_bind_results<-df_bind[c('SP','SUP','U'),grepl('OVERALL',colnames(df_bind))]

# # Content
# df_bind_content<-df_bind[CStore,grepl('CONTENT',colnames(df_bind))]

# # Communication
# df_bind_comms<-df_bind[CoStore,grepl('COMMUNICATION',colnames(df_bind))]



# CoStore<-paste0(rep('Co',1,6),seq(1,6,1)) # For Comms
# CStore<-paste0(rep('C',1,13),seq(1,13,1)) # For Content

CoStore<-SRES_Components[(nrow(SRES_Components)-5):nrow(SRES_Components),2] #For Commss
CStore<-SRES_Components[1:nrow(head(SRES_Components,13)),2] # For Content