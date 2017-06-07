#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom rlang abort
#' @importFrom tibble as_tibble
NULL

#' Retrieve subjects from Statistics Denmark
#'
#' You can use `dst_subject()` to get the subjects, which Statistics Denmark use to categorize
#' their data sets.
#'
#' @param lang description language - "da" for danish and "en" for english.
#' @param columns columns to return. Can take the values "id", "description", "active",
#' "hasSubject" or "subjects".
#'
#' @return Tibble containing the specified columns in the chosen language.
#' @export
#' @examples
#' View(dst_subjects())
dst_subjects <- function(lang = "en", columns = c("id", "description")) {
  url <- modify_url_api("subjects")
  lang <- lang_api(lang)
  query <- list(lang = lang, format = "JSON")

  columns_values <- c("id", "description", "active", "hasSubjects", "subjects")

  if(!all(columns %in% columns_values)) {
    abort('Columns can only take the values: "id", "description", "active", "hasSubject" or
          "subjects"')
  }
  GET_res <- GET(url, query = query)
  as_tibble(fromJSON(content(GET_res, "text"))[columns])
}
