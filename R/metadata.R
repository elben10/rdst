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
