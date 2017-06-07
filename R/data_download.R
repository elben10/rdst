#' @importFrom stringr str_c
#' @importFrom purrr flatten_chr

#' @export
dst_download <- function(tableID, vars = NULL, lang = "en") {
  if(is_missing(tableID)) abort("tableID must be provided. See dst_table()")
  if(!all(vars %in% variables_api(tableID, lang)) & !is_null(vars)) {
    abort(glue('vars can only take the following values: ',
               '{str_c(flatten_chr(dst_variables(tableID, lang, c("id"))), collapse = ", ")}. ',
               'see dst_variables() for description'))
  }
}

#' @export
dst_variables <- function(tableID, lang = "en", columns = c("id", "text")) {
  lang <- lang_api(lang)
  url <- url_api("tableinfo")
  query <- list(id = tableID, format = "JSON", lang = lang)
  columns_values <- c("id", "text", "elimination", "time", "map", "values")

  if(!all(columns %in% columns_values)) {
    glue('columns can take the values: {columns_values}', sep = ", ")
  }

  GET_res <- GET(url, query = query)
  fromJSON(content(GET_res, "text"))[["variables"]][columns]
}


