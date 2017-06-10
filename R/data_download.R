#' @importFrom stringr str_c
#' @importFrom purrr flatten_chr
#' @importFrom readr read_csv2
#' @importFrom rlang is_character
#' @importFrom rlang set_names
#' @importFrom readr locale
NULL

#' download data sets from Statistics Denmark
#'
#' You can use this function to retrieve data sets from Statistics Denmark. It also let you choose
#' which variables to return.
#'
#' @param tableID the data set's ID. See dst_tables() to get the ID.
#' @param vars the variables to return. If not specified it will return time and value of the
#' what the data set measure.
#' @param lang used language in the data set. Can take the values "da" for danish and "en" for
#' english
#' @export
#' @examples
#' dst_download("folk1a")
#' dst_download("folk1a", c("ALDER", "CIVILSTAND"))
dst_download <- function(tableID, vars, lang = "en") {
  lang <- lang_helper(lang)
  url <- modify_url_helper("data", tableID = tableID)
  if(is_missing(vars)) {
    warning("No vars is specified. Only time will be included")
    query <-  list(lang = lang, Tid = "*")
  }
  vars <- vars_helper(tableID, vars)
  vars <- set_names(rep("*", length(vars)), vars)

  if("Tid" %in% names(vars)) {
    vars <- vars[!names(vars) %in% "Tid"]
    query <- c(list(lang = lang, Tid = "*"), vars)
  } else {
    query <- c(list(lang = lang, Tid = "*"), vars)
  }

  col_types <- str_c(rep("c", times = (length(vars)+1)), collapse = "")
  col_types <- str_c(col_types, "d")


  if(status_code(GET(url, query = query)) == 400) {
    msg <- fromJSON(content(GET(url, query = query), "text"))[["message"]]
    abort(msg)
  }

  res <- read_csv2(modify_url(url, query = query), na = c(".."), col_types = col_types,
            locale = locale(decimal_mark = ",", grouping_mark = "."))
  res["TID"] <- parse_date_helper(res[["TID"]])
  res
}

#' get data set variables
#'
#' You can use this function to get a tibble which has information about the available variables
#' in the respective data set as well as an description of those
#'
#' @param tableID the id of the desired table. See dst_tables() to get an overview of the table IDs.
#' @param lang the language used in the description of the variables. "en" for english "da" for
#' danish.
#' @param columns the columns returned. columns can take the following values: id, text,
#' elimination, time or map
#' @export
#' @examples
#' ## Not run:
#' dst_download("FOLK1A")
#' ## End(Not run)
#' dst_variables("FOLK1A", columns = c("id", "text", "time"))
dst_variables <- function(tableID, lang = "en", columns = c("id", "text")) {
  lang <- lang_helper(lang)
  url <- modify_url_helper("tableinfo", tableID = tableID)
  tableID <- table_helper(tableID)
  query <- list(format = "JSON", lang = lang)
  columns_values <- c("id", "text", "elimination", "time", "map", "values")

  if(!all(columns %in% columns_values)) {
    abort(glue('columns can take the values: {columns_values}', sep = ", "))
  }

  GET_res <- GET(url, query = query)
  fromJSON(content(GET_res, "text"))[["variables"]][columns]
}

vars_helper <- function(tableID, vars) {
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

