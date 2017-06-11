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
dst_tables <- function(subjectsID, lang = "en", columns = c("id", "text")) {
  url <- modify_url_helper("tables")
  lang <- lang_helper(lang)

  columns_values <- c("id", "text", "unit", "updated", "firstPeriod", "latestPeriod",
                      "active", "variables", "all")
  subjectsID_values <- c("01", "02", "03", "04", "05", "06", "07", "11", "13", "14", "16", "18")

  if(!all(columns %in% columns_values)) {
    abort(glue('Columns can only take the values: "id", "text", "unit", "updated", ',
               '"firstPeriod", "latestPeriod", "active" or "variables"'))
  }

  if(!is_missing(subjectsID)) {
    if(!all(subjectsID %in% subjectsID_values)) {
      abort(glue('subjects can only take the values: "01", "02", "03", "04", "05", "06", ',
                 '"07", "11", "13", "14", "16" or "18". See dst_subject()'))
    }
  }

  if(is_missing(subjectsID)) {
    query <- list(lang = lang, format = "JSON")
  } else {
    subjectsID <- str_c(subjectsID, collapse = ",")
    query <- list(lang = lang, format = "JSON", subjects = subjectsID)
  }

  GET_res <- GET(url, query = query)

  if(all(columns == "all")) {
    as_tibble(fromJSON(content(GET_res, "text")))
  } else {
    as_tibble(fromJSON(content(GET_res, "text"))[columns])
  }
}

table_helper <- function(tableID) {
  if(is_missing(tableID)) {
    abort("tableID must be provided. See dst_tables()")
  }
  if(!(str_to_upper(tableID) %in% flatten_chr(dst_tables(columns = "id")))) {
    abort("tableID is not correct. See dst_tables()")
  }

  tableID
}

#' @export
dst_tables_search <- function(subjectsID, includes, excludes, lang = "en", columns = c("id", "text")) {
  if(is_missing(includes) & is_missing(excludes)) {
   abort("includes and excludes is missing. Use dst_tables() instead.")
  } else if(is_missing(excludes)) {
    tables <- dst_tables(subjectsID = subjectsID, lang = lang, columns = columns)
    tables[str_detect(tables[["text"]], includes), ]
  } else if(is_missing(includes)) {
    tables <- dst_tables(subjectsID = subjectsID, lang = lang, columns = columns)
    tables[!str_detect(tables[["text"]], excludes), ]
  } else {
    tables <- dst_tables(subjectsID = subjectsID, lang = lang, columns = columns)
    tables[!str_detect(tables[["text"]], excludes) & str_detect(tables[["text"]], includes), ]
  }

}
