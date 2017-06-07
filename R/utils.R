#' @importFrom purrr keep

url_api <- function(type) {
  if(!all(type %in% c("subjects", "tables", "tableinfo", "data"))) {
    abort('type can only take the values: "subjects", "tables", "tableinfo" or "data"')
  }

  if(type == "subjects") {
    "http://api.statbank.dk/v1/subjects"
  } else if (type == "tables") {
    "http://api.statbank.dk/v1/tables"
  } else if (type == "tableinfo") {
    "http://api.statbank.dk/v1/tableinfo"
  } else {
    "http://api.statbank.dk/v1/data"
  }
}

lang_api <- function(lang) {
  if(!lang %in% c("en", "da")) {
    abort('lang can only take the values: "en" or "da')
  }
  lang
}

ignore_null <- function(x) {
  keep(x, function(x) !is_null(x))
}

variables_api <- function(tableID, lang = "en") {
  if(!all(lang %in% c("da", "en"))) {
    abort('lang can only take the values: "en" or "da')
  }
  url <- url_api("tableinfo")
  query <- list(id = tableID, format = "JSON", lang = lang)

  GET_res <- GET(url, query = query)
  fromJSON(content(GET_res, "text"))[["variables"]]
}
