#' Login to the drugshortagescanada.ca web API
#'
#' This function logs in to the DSC web API and stores the auth-token in the
#' 'dsc.authtoken' R environment variable so that it is accessible dsc_search()
#'
#' You should not need usually to call this function directly, as it is called
#' by dsc_search() if the auth-token has not be set.
#'
#' The username and password can be passed into the function, or can be set via
#' and environment variable. See the examples below.
#'
#' @param email Your email address used to log into the DSC
#' @param password Your password
#' @export
#'
#' @examples
#'   dsc_login('bill.gates@microsoft.com', 'passw0rd!')
#'
#'   # You can also set environment variables
#'   Sys.setenv('dsc.email' = 'bill.gates@microsoft.com')
#'   Sys.setenv('dsc.password' = 'passw0rd!')
#'
#'   dsc_login()
#'
#'   # These credentials can also be stored in the .Renviron file in the current
#'   # directory
dsc_login = function(email, password) {
  if (missing(email)) {
    email = Sys.getenv("dsc.email", unset = NA)
  }
  if (missing(password)) {
    password = Sys.getenv("dsc.password", unset = NA)
  }
  if (is.null(email) | is.null(password)) {
    stop("email/password must be specified")
  }
  
  r = httr::POST("https://www.drugshortagescanada.ca/api/v1/login", body = list(email = email, password = password))
  httr::stop_for_status(r, httr::content(r, as = "text"))
  authtoken = httr::headers(r)$`auth-token`
  Sys.setenv(dsc.authtoken = authtoken)
}


#' Return a single page of search results
#'
#' @param ... name = value pairs of query parameters.
#'
#' @return A JSON blog of search results. The 'data' field contains the matching records.
.dsc_search_single_page = function(...) {
  
  if (!"dsc.authtoken" %in% names(Sys.getenv())) {
    try(dsc_login())
  }
  
  authtoken = Sys.getenv("dsc.authtoken")
  if (authtoken == "") {
    stop("Environment variable dsc.authtoken not set. See dsc_login()")
  }
  
  r = httr::GET("https://www.drugshortagescanada.ca/api/v1/search", httr::add_headers(`auth-token` = authtoken), query = list(...))
  httr::stop_for_status(r, httr::content(r, as = "text"))
  return(jsonlite::fromJSON(httr::content(r, as = "text", encoding = "UTF-8")))
}

#' Return all records from a search
#'
#' @param ... name = value pairs of query parameters.
#'
#' @return  nested data.frame of results
.dsc_search_all = function(...) {
  query = list(...)
  query[["limit"]] = "1000"  # optimisitic...
  results = do.call(.dsc_search_single_page, query)
  
  # TODO: empty search results TODO: total < limit results
  
  limit = results$limit
  total = results$total
  offset = results$offset
  
  all_results = list(results$data)
  
  for (offset in seq(offset + limit, total - 1, limit)) {
    query[["offset"]] = offset
    
    results = do.call(.dsc_search_single_page, query)
    all_results = c(all_results, list(results$data))
  }
  return(jsonlite::rbind_pages(all_results))
}

#' DSC search results
#'
#' @param ... A named list of query parameters.
#'   See the DSC Web API documentation for details on query parameters:
#'   https://www.drugshortagescanada.ca/blog/52
#' @param single_page If TRUE only returns the first page of results as a JSON
#'   object. This is useful only if you care about the metadata about the search
#'   returned by the database. See the API documentation for details.
#' @param flattened If TRUE returns a flat-ish data.frame of the results using
#'   jsonlite::flatten. This does not apply if single_page = TRUE
#' @return Results as a data.fram, or JSON object if single_page = T
#' @export
#'
#' @examples
#' results = dsc_search(term = 'venlafaxine')
dsc_search = function(..., single_page = F, flattened = T) {
  if (single_page) {
    results = do.call(.dsc_search_single_page, list(...))
  } else {
    results = do.call(.dsc_search_all, list(...))
    if (flattened) {
      return(jsonlite::flatten(results))
    }
  }
  return(results)
}
