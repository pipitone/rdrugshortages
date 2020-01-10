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
#' @param query A named list() of query parameters.
#'
#' @return A JSON blog of search results. The 'data' field contains the matching records.
#' @examples
.dsc_search_single_page = function(authtoken, query) {
  if (missing(query)) {
    query = list()
  }
  r = httr::GET("https://www.drugshortagescanada.ca/api/v1/search",
          httr::add_headers("auth-token" = authtoken),
          query = query)
  httr::stop_for_status(r)
  return(jsonlite::fromJSON(httr::content(r,as="text",encoding="UTF-8")))
}

#' Return all records from a search
#'
#' @param authtoken
#' @param query
#'
#' @return
#'
#' @examples
.dsc_search_all = function(authtoken, query) {
  if (missing(query)) {
    query = list()
  }
  query[["limit"]] = "1000"  # optimisitic...
  results = .dsc_search_single_page(authtoken, query)
  #TODO: empty search results
  #TODO: total < limit results

  limit = results$limit
  total = results$total
  offset = results$offset

  all_results = list(results$data)

  for (offset in seq(offset+limit,total-1,limit)) {
    query[["offset"]] = offset

    results = do.call(.dsc_search_single_page,list(authtoken,query))
    all_results = c(all_results,list(results$data))
  }
  return(jsonlite::rbind_pages(all_results))
}

#' DSC search results
#'
#'
#' @param authtoken Authorization token obtained from dsc_login()
#' @param query A named list() of query parameters.
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
#' token = login(email, password)
#' results = dsc_search(token, query=list(term="venlafaxine"))
dsc_search = function(authtoken, query = list(), single_page = F, flattened = T) {
  if (single_page) {
    results = .dsc_search_single_page(authtoken, query)
  } else {
    results = .dsc_search_all(authtoken, query)
    if (flattened) {
      return(jsonlite::flatten(results))
    }
  }
  return(results)
}
