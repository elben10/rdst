url_api <- function(type) {
  arg_match(type, c("subjects", "tables", "tableinfo", "data"))

  if(type == "subject") {
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
  arg_match(lang, c("en", "da"))
  lang
}
