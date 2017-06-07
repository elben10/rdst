#' @importFrom stringr str_c

#' @export
dst_download <- function(tableID, vars = NULL, lang = "en") {
  if(is_missing(tableID)) abort("tableID must be provided. See dst_table()")
  if(!all(vars %in% variables_api(tableID, lang)) & !is_null(vars)) {
    abort(glue('vars can only take the following values: ',
               '{str_c(variables_api(tableID), collapse = ", ")}',
               'see dst_variables() for description'))
  }
}



