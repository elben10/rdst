#' @importFrom purrr keep
#' @importFrom stringr str_to_upper

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

table_api <- function(tableID) {
  if(is_missing(tableID)) {
    abort("tableID must be provided. See dst_tables()")
  }
  if(!(str_to_upper(tableID) %in% flatten_chr(dst_tables(columns = "id")))) {
    abort("tableID is not correct. See dst_tables()")
  }

  tableID
}
