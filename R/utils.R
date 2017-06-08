#' @importFrom purrr keep
#' @importFrom stringr str_to_upper
NULL

modify_url_helper <- function(type, tableID, lang = "en") {
  if(!all(type %in% c("subjects", "tables", "tableinfo", "data"))) {
    abort('type can only take the values: "subjects", "tables", "tableinfo" or "data"')
  }

  url <- "http://api.statbank.dk"

  if(type == "subjects") {
    modify_url(url, path = c("v1","subjects"))
  } else if(type == "tables") {
    modify_url(url, path = c("v1","tables"))
  } else if(type == "tableinfo") {
    modify_url(url, path = c("v1", "tableinfo", table_api(tableID)))
  } else {
    modify_url(url, path = c("v1", "data", table_api(tableID), "CSV"))
  }
}

lang_helper <- function(lang) {
  if(!lang %in% c("en", "da")) {
    abort('lang can only take the values: "en" or "da')
  }
  lang
}




