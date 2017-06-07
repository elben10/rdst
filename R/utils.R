#' @importFrom purrr keep
#' @importFrom stringr str_to_upper

modify_url_api <- function(type, tableID, lang = "en") {
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

lang_api <- function(lang) {
  if(!lang %in% c("en", "da")) {
    abort('lang can only take the values: "en" or "da')
  }
  lang
}

ignore_null <- function(x) {
  keep(x, function(x) !is_null(x))
}

table_api <- function(tableID) {
  if(is_missing(tableID)) {
    abort("tableID must be provided. See dst_tables()")
  }
  if(!(str_to_upper(tableID) %in% flatten_chr(dst_tables(columns = "id")))) {
    abort("tableID is not correct. See dst_tables()")
  }

  tableID
}

vars_api <- function(tableID, vars) {
  if(!is_missing(vars)) {
    if(!all(vars %in% flatten_chr(dst_variables(tableID, columns = "id")))) {
      abort(glue('vars can take the following values: ',
                 '{str_c(flatten_chr(dst_variables("BEV22", columns = "id")), collapse = ", ")}. ',
                 'The values must be provided as an charactervector. ',
                 'See dst_variables() for explanation of the vars'))
    } else {
      return(vars)
    }
  }
}
