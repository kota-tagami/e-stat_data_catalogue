source("packages.R")

shiny_app_url <- ""
github_url <- "https://github.com/kota-tagami/e-stat_data_catalogue.git"
version <- "0.0.0.9000"

##---- estat-api ----
app_id <- "b6d04cab54dc4bf98cb56c86ffcc9c2229bfbe99"
endpoint <- "https://api.e-stat.go.jp/rest/"
ver <- "3.0"
app_type <- "/app/json/"

estat_api <- function(api_method, params = NULL){
  URL <- paste0(endpoint, ver, app_type, api_method)
  
  queries <- list(
    "appId" = app_id
  )
  if(!is.null(params)) {
    queries <- c(queries, params)
  }
  
  res <- httr::GET(url = URL, query = queries)
  content <- httr::content(res)
  
  content
}


##---- stat_list ----
stat_list <- 
  tribble(
    ~ stat_name, ~ stat_code, ~ url,
    "国勢調査", "00200521", "https://www.e-stat.go.jp/statistics/00200521",
    "労働力調査", "00200531", "https://www.e-stat.go.jp/statistics/00200531",
    "就業構造基本調査", "00200532", "https://www.e-stat.go.jp/statistics/00200532",
    "賃金構造基本統計調査", "00450091", "https://www.e-stat.go.jp/statistics/00450091"
  )
