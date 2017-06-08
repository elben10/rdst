#' @importFrom purrr keep
#' @importFrom purrr map_dbl
#' @importFrom purrr map2_dbl
#' @importFrom stringr str_to_upper
#' @importFrom stringr str_detect
#' @importFrom stringr str_sub
#' @importFrom lubridate myd
#' @importFrom lubridate as_date
NULL

modify_url_helper <- function(type, tableID, lang = "en") {
  if(!all(type %in% c("subjects", "tables", "tableinfo", "data"))) {
    abort('type can only take the values: "subjects", "tables", "tableinfo" or "data"')
  }

  url <- "http://api.statbank.dk"

  if(type == "subjects") {
    modify_url(url, path = c("v1","subjects"))
  } else if(type == "tables") {
    modify_url(url, path = c("v1","tables"))
  } else if(type == "tableinfo") {
    modify_url(url, path = c("v1", "tableinfo", table_helper(tableID)))
  } else {
    modify_url(url, path = c("v1", "data", table_helper(tableID), "CSV"))
  }
}

lang_helper <- function(lang) {
  if(!lang %in% c("en", "da")) {
    abort('lang can only take the values: "en" or "da')
  }
  lang
}

date_helper <- function(tableID, lang = "en") {
  lang_helper(lang)
  table_helper(tableID)

  tables <- dst_tables(lang = lang, columns = c("id", "firstPeriod", "latestPeriod"))
  date <- tables[which(str_to_upper(tableID) == tables["id"]),c("firstPeriod", "latestPeriod")]
  date <- flatten_chr(date)

  date_type(date, lang)

}

is_quarter <- function(date_vector, lang) {
  return(all(str_detect(date_vector, "^[0-9][0-9][0-9][0-9](Q|K)[1-4]$")))
}

is_year <- function(date_vector) {
  all(str_detect(date_vector, "^[0-9][0-9][0-9][0-9]$"))
}

is_month <- function(date_vector) {
  all(str_detect(date_vector, "^[0-9][0-9][0-9][0-9]M[0-9][0-9]$"))
}

is_halfyear <- function(date_vector) {
    all(str_detect(date_vector, "^[0-9][0-9][0-9][0-9]H[1-2]"))
}

is_date <- function(date_vector) {
  all(str_detect(date_vector, "^[0-9][0-9][0-9][0-9]M[0-1][0-9]D[0-3][0-9]"))
}

make_myd <- function(m, y, d) {
  myd(str_c(m, y, d))
}

make_year_helper <- function(x) {
  make_myd("01", x, "01")
}


make_quarter_helper <- function(x) {
  year <- str_sub(x, 1, 4)
  quarter <- str_sub(x, 6, 6)

  if(quarter == "1") {
    month <- "01"
  } else if(quarter == "2") {
    month <- "04"
  } else if(quarter == "3") {
    month <- "07"
  } else {
    month <- "10"
  }

  make_myd(month, year, "01")
}

make_month_helper <- function(x) {
  year <- str_sub(x, 1, 4)
  month <- str_sub(x, 6, 7)

  make_myd(month, year, "01")
}

make_halfyear_helper <- function(x) {
  year <- str_sub(x, 1, 4)
  halfyear <- str_sub(x, 6, 6)

  if(halfyear == "1") {
    month <- "01"
  } else {
    month <- "07"
  }

  make_myd(month, year, "01")
}

make_date_helper <- function(x) {
  year <- str_sub(x, 1, 4)
  month <- str_sub(x, 6, 7)
  day <- str_sub(x, 8, 10)

  make_myd(month, year, day)
}

parse_date_helper <- function(x) {
  if(is_year(x)) {
    as_date(map_dbl(x, make_year_helper))
  } else if(is_quarter(x)) {
    as_date(map_dbl(x, make_quarter_helper))
  } else if(is_month(x)) {
    as_date(map_dbl(x, make_month_helper))
  } else if(is_date(x)) {
    as_date(map_dbl(x, make_date_helper))
  } else if(is_halfyear(x)) {
    as_date(map_dbl(x, make_halfyear_helper))
  } else {
    warning("Time has been returned as character, because it does not have a convertible format")
    x
  }
}
