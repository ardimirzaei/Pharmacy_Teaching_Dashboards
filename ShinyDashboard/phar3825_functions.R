download_content <- function(sres_api_token, url){

  GET(
    url = url,
    content_type_json(),
    add_headers(`sres-api-token` = sprintf(" %s", sres_api_token))
  ) -> raw_result
  
  this_raw_content <- rawToChar(raw_result$content)  
  data = fromJSON(rawToChar(raw_result$content), simplifyDataFrame = TRUE)
  
  return(data)
  
}

clean_cell_and_mark <- function(df){

  df <- apply(df, 2,function(x) gsub("\"","",x))
  df <- apply(df, 2,function(x) gsub("\\[","",x))
  df <- apply(df, 2,function(x) gsub("^U.+","U",x))
  df <- apply(df, 2,function(x) gsub("^SUP.+","SUP",x))
  df <- apply(df, 2,function(x) gsub("^SP.+","S",x))
  df <- apply(df, 2,function(x) gsub("^,.+","--Missing Mark--",x))

  return(df)
}

