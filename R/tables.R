#' @importFrom glue glue
#' @importFrom rlang is_missing
#' @importFrom rlang is_null

#' @export
dst_tables <- function(subjects = NULL, lang = "en", columns = c("id", "text")) {
  url <- modify_url_api("tables")
  lang <- lang_api(lang)
  query <- ignore_null(list(lang = lang, format = "JSON", subjects = subjects))

  columns_values <- c("id", "text", "unit", "updated", "firstPeriod", "latestPeriod",
                      "active", "variables")
  subjects_values <- c("01", "02", "03", "04", "05", "06", "07", "11", "13", "14", "16", "18")

  if(!all(columns %in% columns_values)) {
    abort(glue('Columns can only take the values: "id", "text", "unit", "updated", ',
               '"firstPeriod", "latestPeriod", "active" or "variables"'))
  }

  if(!all(subjects %in% subjects_values) & !is_null(subjects)) {
    abort(glue('subjects can only take the values: "01", "02", "03", "04", "05", "06", ',
          '"07", "11", "13", "14", "16" or "18". See dst_subject()'))
  }

  GET_res <- GET(url, query = query)
  as_tibble(fromJSON(content(GET_res, "text"))[columns])
}
