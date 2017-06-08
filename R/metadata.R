#' @importFrom purrr flatten
NULL

#' @export
dst_information <- function(tableID, lang = "en", columns = c("id", "description", "name", "mail")) {
  lang_helper(lang)
  url <- modify_url_helper("tableinfo", tableID = tableID)
  table_helper(tableID)
  query <- list(format = "JSON", lang = lang)

  columns_values <- c("id", "text", "description", "unit", "updated", "name", "phone",
                      "mail", "documentationID", "url", "footnote")

  if(!all(columns %in% columns_values)) {
    abort(glue('Variables can only take the values: {columns_values}',
               sep = ", "))
  }

  GET_res <- GET(url, query = query)
  gen_info <- c(extract_helper(GET_res, 1:5),
                flatten(extract_helper(GET_res, 6)),
                flatten(extract_helper(GET_res, 7)),
                extract_helper(GET_res, 8))

  names(gen_info)[9] <- "documentationID"
  print_information(gen_info[columns])
}

extract_helper <- function(x, indx) {
  fromJSON(content(x, "text"))[indx]
}

print_information <- function(x) {
  cat(str_c(names(x), flatten_chr(x), sep = ": "), sep = "\n")
}

#' @export
dst_variable_values <- function(tableID, lang = "en") {
  lang_helper(lang)
  url <- modify_url_helper("tableinfo", tableID = tableID)
  table_helper(tableID)
  query <- list(format = "JSON", lang = lang)
  GET_res <- GET(url, query = query)

  res_list <- flatten(extract_helper(GET_res)["variables"])
  names_list_uneven <- flatten_chr(res_list[c("id")])
  names_list_even <-  flatten_chr(res_list[c("text")])
  values_list <- flatten(res_list["values"])

  max_length <- max(map_dbl(values_list, nrow))

  res <- list()
  for(i in seq_along(values_list)) {
    res <- c(res, map(values_list[[i]], function(x) c(x, rep(NA, times = max_length - length(x)))))
  }
  even_indx <- 1:length(res)
  even_indx <- even_indx[even_numbers(1:length(res))]

  uneven_indx <- 1:length(res)
  uneven_indx <- uneven_indx[uneven_numbers(1:length(res))]

  names(res)[even_indx] <- names_list_even
  names(res)[uneven_indx] <- names_list_uneven
  as_tibble(res)


}

even_numbers <- function(x) x %% 2 == 0
uneven_numbers <- function(x) x %% 2 != 0

