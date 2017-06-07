#' @importFrom glue glue
#' @importFrom rlang is_missing
#' @importFrom rlang is_null
NULL

#' retrieve tables from Statistics Denmark
#'
#' You can use `dst_tables()` to get a tibble, which contain information about the data sets
#' available at the Bureau.
#'
#' @param subjectsID Should the data sets be within a specific field. See dst_subject() for IDs.
#' @param lang the desired language. Can take the values "da" for danish and "en" english.
#' @param columns the returned columns. Can take the values "id", "text", "unit", "updated",
#' "firstPeriod", "latestPeriod", "active" or "variables"
#'
#' @return Tibble which contain information about all tables available at Danish Statistics. If
#' subject is provided only tables within the subject is returned. tibble will contain the columns
#' specified in the columns argument. The language is as default English otherwise "da" is
#' provided in the lang argument.
#' @export
#' @examples
#' dst_tables() # retrieve all tables
#' dst_tables(subjectsID = "02") # Population and elections data sets
dst_tables <- function(subjectsID = NULL, lang = "en", columns = c("id", "text")) {
  url <- modify_url_api("tables")
  lang <- lang_api(lang)
  query <- ignore_null(list(lang = lang, format = "JSON", subjects = subjectsID))

  columns_values <- c("id", "text", "unit", "updated", "firstPeriod", "latestPeriod",
                      "active", "variables")
  subjectsID_values <- c("01", "02", "03", "04", "05", "06", "07", "11", "13", "14", "16", "18")

  if(!all(columns %in% columns_values)) {
    abort(glue('Columns can only take the values: "id", "text", "unit", "updated", ',
               '"firstPeriod", "latestPeriod", "active" or "variables"'))
  }

  if(!all(subjectsID %in% subjectsID_values) & !is_null(subjectsID)) {
    abort(glue('subjects can only take the values: "01", "02", "03", "04", "05", "06", ',
          '"07", "11", "13", "14", "16" or "18". See dst_subject()'))
  }

  GET_res <- GET(url, query = query)
  as_tibble(fromJSON(content(GET_res, "text"))[columns])
}
