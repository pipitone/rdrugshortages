#' Login
#'
#' This function logs in to drugshortagescanada.ca via the Web API and returns
#' an authorization token if successful.
#'
#' @param email Your email address used to log into the site
#' @param password Your password
#' @export
#' @examples
#' token = dsc_login("bill.gates@microsoft.com", "passw0rd!")
dsc_login = function (email, password) {
  r = httr::POST("https://www.drugshortagescanada.ca/api/v1/login",
           body = list(
             "email" = "jon@pipitone.ca",
             "password" = "CZjwVE3grhXyqw$r9EjqRp"
           ))
  httr::stop_for_status(r)
  return(httr::headers(r)$`auth-token`)
}


#' Return a single page of search results
#'
#' @param authtoken Authorization token obtained from dsc_login()
#' @param query A named list() of query parameters. See the web API
#'   documentation for details
#'
#' @return A JSON blog of search results. The 'data' field contains the matching records.
#' @export
#'
#' @examples
#' token = login(email, password)
#' results = dsc_search_single_page(token, query=list(term="venlafaxine"))
dsc_search_single_page = function(authtoken, query) {
  if (missing(query)) {
    query = list()
  }
  r = httr::GET("https://www.drugshortagescanada.ca/api/v1/search",
          httr::add_headers("auth-token" = authtoken),
          query = query)
  httr::stop_for_status(r)
  return(jsonlite::fromJSON(httr::content(r,as="text",encoding="UTF-8")))
}

#' Return all records from a search as a JSON object
#'
#' @param authtoken
#' @param query
#'
#' @return
#' @export
#'
#' @examples
dsc_search_json = function(authtoken, query) {
  if (missing(query)) {
    query = list()
  }
  query[["limit"]] = "1000"  # optimisitic...
  results = dsc_search_single_page(authtoken, query)
  #TODO: empty search results
  #TODO: total < limit results

  limit = results$limit
  total = results$total
  offset = results$offset

  all_results = list(results$data)

  for (offset in seq(offset+limit,total-1,limit)) {
    query[["offset"]] = offset

    results = do.call(dsc_search_single_page,list(authtoken,query))
    all_results = c(all_results,list(results$data))
  }
  return(jsonlite::rbind_pages(all_results))
}

#' Return all search results as a flat-ish data.frame
#'
#' @param authtoken
#' @param query
#'
#' @return
#' @export
#' @examples
#'
dsc_search = function(authtoken, query) {
  jsonlite::flatten(dsc_search_json(authtoken, query))
}
