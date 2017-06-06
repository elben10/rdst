#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom rlang abort
#' @importFrom tibble as_tibble

#' @export
dst_subjects <- function(lang = "en", columns = c("id", "description")) {
  url <- url_api("subjects")
  lang <- lang_api(lang)

  if(!all(columns %in% c("id", "description", "active", "hasSubjects", "subjects"))) {
    abort('Columns can only take the values: "id", "description", "active", "hasSubject" or
          "subjects"')
  }
  GET_res <- GET(url, query = list(lang = lang, format = "JSON"))
  as_tibble(fromJSON(content(GET_res, "text"))[columns])
}
