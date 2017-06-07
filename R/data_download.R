#' @importFrom stringr str_c
#' @importFrom purrr flatten_chr
#' @importFrom readr read_csv2
#' @importFrom rlang is_character
#' @importFrom rlang set_names

#' @export
dst_download <- function(tableID, vars, lang = "en") {
  lang <- lang_api(lang)
  url <- modify_url_api("data", tableID = tableID)
  if(is_missing(vars)) {
    warning("No vars is specified.")
    query <-  list(lang = lang, Tid = "*")
  }

  vars <- vars_api(tableID, vars)
  vars <- set_names(rep("*", length(vars)), vars)

  if("Tid" %in% names(vars)) {
    query <- c(list(lang = lang), vars)
  } else {
    query <- c(list(lang = lang, Tid = "*"), vars)
  }


  if(status_code(GET(url, query = query)) == 400) {
    msg <- fromJSON(content(GET(url, query = query), "text"))[["message"]]
    abort(msg)
  }

  read_csv2(modify_url(url, query = query), na = c(".."))
}

#' @export
dst_variables <- function(tableID, lang = "en", columns = c("id", "text")) {
  lang <- lang_api(lang)
  url <- modify_url_api("tableinfo", tableID = tableID)
  tableID <- table_api(tableID)
  query <- list(format = "JSON", lang = lang)
  columns_values <- c("id", "text", "elimination", "time", "map", "values")

  if(!all(columns %in% columns_values)) {
    glue('columns can take the values: {columns_values}', sep = ", ")
  }

  GET_res <- GET(url, query = query)
  fromJSON(content(GET_res, "text"))[["variables"]][columns]
}


