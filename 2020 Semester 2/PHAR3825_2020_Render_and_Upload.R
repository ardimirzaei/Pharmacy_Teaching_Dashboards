
Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")

file_path <- ""
library(RCurl)
rmarkdown::render(paste0(file_path,"PHAR3825_2020_rmarkdown.Rmd"),
                  #rmarkdown::pdf_document(),
                  output_file =  paste("PHAR3825_2020", ".html", sep=''), 
                  output_dir = paste0(file_path)
)

# FTP UPLOAD