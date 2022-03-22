#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(DT)
library(shinydashboard)
library(plotly)

source("phar3825_functions.R")

#https://rstudio.github.io/shinydashboard/structure.html
#https://fontawesome.com/v4/icons/

if (file.exists("sresapi.key")){
    key_file <- readLines("sresapi.key", warn = FALSE)
    key_placeholder <- "Key FOUND on file!" 
    if (nchar(key_file)<32){ key_placeholder <- "Please Add Your SRES API Key" }
} else {
    key_placeholder <- "Please Add Your SRES API Key" 
}

header <- dashboardHeader(title = "PHAR3825 Dashboard",
                          dropdownMenuOutput("messageMenu")
                          )

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(textInput("sresapi", "SRES API", value = "", width = NULL, placeholder = key_placeholder)),
        menuItem(actionButton("submit_sres", "Refresh SRES Data", icon = icon("refresh"), style='padding:4px; font-size:90%; margin-left: -3px; margin-right: -3px; margin-top: -3px; margin-bottom: -10px', align = "left", width = '100%')),
        # menuItem(textOutput('msg_outputs')),
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard") ),
        menuItem("Data Explorer", icon = icon("th"), tabName = "data_explorer",
                 menuSubItem("Raw", tabName = "raw",icon = icon("angle-double-right")),
                 # badgeLabel = "new", badgeColor = "green",
                 menuSubItem("Marks", tabName = "pass_rate",icon = icon("angle-double-right")),
                 menuSubItem("Labels", tabName = "labels",icon = icon("angle-double-right")),
                 menuSubItem("Products", tabName = "products",icon = icon("angle-double-right")),
                 menuSubItem("Documentation", tabName = "documentation",icon = icon("angle-double-right"))
                 ),
        
        menuItem("Group Plots", tabName = "group_plot", icon = icon("chart-bar"))
    )
)

body <- dashboardBody(
    tags$head(tags$style(HTML(".small-box {height: 150px}"))),
    
    includeScript('www/messsage-handler.js'),
    tags$head(tags$script(src = "message-handler.js")),
    tabItems(
        tabItem(tabName = "dashboard",
                h2("Dashboard tab content"),
                textOutput('nstudents'),
                verbatimTextOutput("value"),
                fluidRow(
                    width = 12,
                    uiOutput('dashboard_boxes'),
                    # A static valueBox
                    # valueBox(10 * 2, "Total Students", icon = icon("users", lib = "font-awesome"))
                )
        ),
        
        tabItem(tabName = "raw",
                h2("Class Data Explorer"),
                # box( title = "Class Data Details", status = "primary", solidHeader = T, 
                     # column(width = 12,
                            DT::dataTableOutput("raw"),style = "height:850px; overflow-y: scroll;overflow-x: scroll;" # can add at the begninning  = height:850px; 
                     # )
                # )
                
        ),
        tabItem(tabName = "pass_rate",
                h2("Class Data Explorer"),
                DT::dataTableOutput("pass"),style = "height:850px; overflow-y: scroll;overflow-x: scroll;" 
        ),
        tabItem(tabName = "labels",
                h2("Class Data Explorer"),
                DT::dataTableOutput("label"),style = "height:850px; overflow-y: scroll;overflow-x: scroll;" 
        ),
        tabItem(tabName = "documentation",
                h2("Class Data Explorer"),
                DT::dataTableOutput("doc"),style = "height:850px; overflow-y: scroll;overflow-x: scroll;" 
        ),
        tabItem(tabName = "products",
                h2("Class Data Explorer"),
                DT::dataTableOutput("prod"),style = "height:850px; overflow-y: scroll;overflow-x: scroll;" 
        ),
        
        
        tabItem(tabName = "group_plot",
                h2("Group tab content"),
                plotlyOutput('group_plots')
                

        )
    )
)

ui <- dashboardPage(header, sidebar, body, skin = "purple")

# 
# # Define UI for application that draws a histogram
# ui <- dashboardPage(
# 
#     # Application title
#     dashboardHeader(title = "PHAR3815 Dashboard"),
# 
#     includeScript('www/messsage-handler.js'),
#     tags$head(tags$script(src = "message-handler.js")),
#     
#     dashboardSidebar(),
#     # # input SRES API
#     # dashboardSidebar(
#     #     # sidebarPanel(
#     #         textInput("sresapi", "SRES API", value = "", width = NULL, placeholder = NULL),
#     #         actionButton("submit_sres", "Submit"),
#     #         p("Please check this link if you do not have an API key: https://bit.ly/sres-api1"),
#     #         #cars[-1, ]x
#     #         br()
#     #         
#     #     ),
# 
#     # Show a plot of the generated distribution
#     dashboardBody(
#         box(            textInput("sresapi", "SRES API", value = "", width = NULL, placeholder = NULL),
#                         actionButton("submit_sres", "Submit"),
#                         p("Please check this link if you do not have an API key: https://bit.ly/sres-api1"),
#                         #cars[-1, ]
#                         br()
#                         ),
#         box(textOutput("success")),
#         box(tableOutput("tbl"))
#     )
#     
# )

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    # if (file.exists("sresapi.key")){
    #     sres_api_token <- readLines("sresapi.key", warn = FALSE)
    #     if (nchar(sres_api_token)<32){ sres_api_token <- "Please Add Your SRES API Key" }
    # } else {
    #     sres_api_token <- "Please Add Your SRES API Key"
    # }

    vals <- reactiveValues()
    
    
    observeEvent(input$submit_sres, {
        withProgress(message = 'Refreshing Data', value = 0, {
            n = 10 # for progress bar
            url = sprintf("https://sres.sydney.edu.au/api/v1/tables/19CAD595_9B1E_4A5B_9B7D8DC24847DBDB/columns") #, ticker),
            
            if (file.exists("sresapi.key")){
                sres_api_token <- readLines("sresapi.key", warn = FALSE)
                # if (nchar(sres_api_token)<32){ sres_api_token <- "Please Add Your SRES API Key" }
            } else {
                sres_api_token <- input$sresapi    
            }
            
    
            vals$message <- "Downloading Table" ;incProgress(1/n, detail =vals$message)

            table_info <- download_content(sres_api_token, url)
            # print(table_info[c('name','uuid')])
            print("Table Info Loaded")
            vals$message <- "Table Download Complete"
            cols_to_use <- table_info[grep("[0-9]\\.[0-9].+",table_info$name), c('name','uuid')]
            
            vals$message <- "Downloading Column Information" ;incProgress(1/n, detail =vals$message)
            
            COLUMN <- paste0(cols_to_use$uuid,"&column_uuids=", collapse="")
            
            url <- sprintf("https://sres.sydney.edu.au/api/v1/tables/19CAD595_9B1E_4A5B_9B7D8DC24847DBDB/data?column_uuids=%s", COLUMN)
            
            col_data <- download_content(sres_api_token, url)
            vals$message <- "Column Download Complete" ;incProgress(1/n, detail =vals$message)
            
            vals$message <- "Cleaning Columns" ;incProgress(1/n, detail =vals$message)
            
            # rename the columns UUIDs
            for ( i in 1:length(cols_to_use$name)){
                colnames(col_data$data) <- gsub(cols_to_use$uuid[i], cols_to_use$name[i],colnames(col_data$data))
                # print(colnames(col_data$data))
            }
    
            col_data <- as.data.frame(as.matrix(col_data))
            colnames(col_data) <- gsub("data.","",colnames(col_data))
            # Sort the Columns
            col_data <- col_data[sort(colnames(col_data))]
            
            
            # session$sendCustomMessage(type = 'testmessage',
            #                           message = 'Data Loaded')
            print("Col Data Loaded")
            vals$col_data <- col_data
            vals$message <- "Clean Column Complete" ;incProgress(1/n, detail =vals$message)
            
            vals$marks <- clean_cell_and_mark(col_data)
            vals$marks <-as.data.frame(vals$marks)
            # apply(col_data, 2,function(x) gsub("\\[\"D[0-9]\"","________D________",x))
            vals$documentation <- apply(col_data, 2,function(x) as.numeric(str_count(x,"\"D[0-9]\""))) ; vals$documentation <- apply(vals$documentation, 2,function(x) ifelse(x==0, "",x)) ;
            vals$label <- apply(col_data, 2,function(x) as.numeric(str_count(x,"\"L[0-9]\""))) ; vals$label <- apply(vals$label, 2,function(x) ifelse(x==0, "",x)) ;
            vals$products <- apply(col_data, 2,function(x) as.numeric(str_count(x,"\"P[0-9]\""))) ; vals$products <- apply(vals$products, 2,function(x) ifelse(x==0, "",x)) ;

            # vals$documentation <- apply(col_data, 2,function(x) as.numeric(grepl("\\[\"D[0-9]\"",x))) ; vals$documentation <- apply(vals$documentation, 2,function(x) ifelse(x==0, "",x)) ;
            # vals$label <- apply(col_data, 2,function(x) as.numeric(grepl("\\[\"L[0-9]\"",x))) ; vals$label <- apply(vals$label, 2,function(x) ifelse(x==0, "",x)) ;
            # vals$products <- apply(col_data, 2,function(x) as.numeric(grepl("\\[\"P[0-9]\"",x))) ; vals$products <- apply(vals$products, 2,function(x) ifelse(x==0, "",x)) ;
            # 
            
            # Render dashboard Tab Boxes
            
            output$dashboard_boxes <- renderUI({
                db_colors <- c('red','orange','light-blue','blue','green')
                
                pass <- vals$marks
                pass <- apply(pass,2,function(x) round(sum(grepl('^S+',x)) / length(x) * 100,2))
                # pass <- pass[grepl("^[1-9]", colnames(pass))]
                pass <- pass[pass>0]
                lapply(1:length(pass), function(i) { 
                    valueBox(paste0(pass[i]," %"),
                             names(pass[i]),     #here display number1 one by one like name 
                             width = 4,
                             icon = icon("flask", lib = "font-awesome"),
                             color = db_colors[as.integer(pass[i]/20)]
                    )
                } )
            })
            
            # Group plots outputs 
            output$group_plots <- renderPlotly({
                up_marks <- vals$marks
                up_marks <- data.frame(up = apply(up_marks,2,function(x) round(sum(grepl('^U+',x)) / length(x) * 100,2)), products = names(up_marks))
                up_marks <- up_marks[up_marks$up > 0,]
                
                plt_colors <- as.numeric(str_extract(up_marks$products,"^[0-9]"))
                

                
                up_plot <- plot_ly(up_marks,x= ~products, y=~up, type='bar', 
                                   text=~up, texttemplate = '%{y:.2s}%', hovertemplate = paste('%{x}', '<br>UP: %{text:.2s}<br>'),
                                   marker = list(color = plt_colors,colorscale='Blues')
                                   ) %>%
                    layout(title = 'Percentage of Students Scoring Unsatisfactory',
                           xaxis = list(title = '',
                                        zeroline = TRUE#,
                                        # range = c(0, 250)
                                        ),
                           yaxis = list(title = 'Unsatisfactory Rate (%)'#,
                                        # range = c(0,1400)
                                        )
                           )
                
            })
            
            # Drop down menu
            output$messageMenu <- renderMenu({
                # # Code to generate each of the messageItems here, in a list. This assumes
                # # that messageData is a data frame with two columns, 'from' and 'message'.
                # msgs <- apply(messageData, 1, function(row) {
                #     messageItem(from = row[["from"]], message = row[["message"]])
                # })
                # 
                # # This is equivalent to calling:
                # #   dropdownMenu(type="messages", msgs[[1]], msgs[[2]], ...)
                
                missing_mark <- sum(vals$marks == "--Missing Mark--")
                dropdownMenu(type = "notifications",
                             notificationItem(
                                 text = paste0("There are ",missing_mark," students with a mark missing"),
                                 icon = icon("exclamation-triangle"),
                                 status = "warning"
                             ))
                
            })
            
            }) #End Progress 
        
    })
    output$success <-  renderText({ input$sresapi })
    output$msg_outputs <- renderText({ vals$message })
    output$nstudents <- renderText({
        if (!is.null(nrow(vals$col_data))){
            sprintf("There are %s students",nrow(vals$col_data))
        } else{
            sprintf("Please Load and Refresh your API key.\nPlease check this link if you do not have an API key: https://bit.ly/sres-api1")        
            }
    })
    output$raw <- renderDataTable({ 
        datatable(vals$col_data, options = list(paging = FALSE))
         })
    output$pass <- renderDataTable({ 
        datatable(vals$marks, options = list(paging = FALSE))
    })
    output$label <- renderDataTable({ 
        datatable(vals$label, options = list(paging = FALSE))
    })
    output$doc <- renderDataTable({ 
        datatable(vals$documentation, options = list(paging = FALSE))
    })
    output$prod <- renderDataTable({ 
        datatable(vals$products, options = list(paging = FALSE))
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
