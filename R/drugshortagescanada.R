#' Login to the drugshortagescanada.ca web API
#'
#' This function logs in to the DSC web API and stores the temporary
#' authentication token so that it can be used by other API calls.
#'
#' You shouldn't usually need to call this function directly, as dsc_search()
#' will automatically do so.
#'
#' The username and password can be passed into the function explicitly, or can
#' be set via environment variables in .Renviron, or directly like so:
#'
#'   Sys.setenv('dsc.email' = 'bill.gates@microsoft.com')
#'   Sys.setenv('dsc.password' = 'passw0rd!')
#'
#'   dsc_login()
#'
#' @param email Your email address used to log into the DSC
#' @param password Your password
#' @param reuse_authtoken If an authentication token has already been retrieved
#'   this session, then return it without logging in again via the API
#' @return authentication token
#' @export
dsc_authtoken <- function(email, password, reuse_authtoken = F) {
    if (reuse_authtoken & !is.null(.state$dsc.authtoken)) {
        return(.state$dsc.authtoken)
    }
    if (missing(email)) {
        email <- Sys.getenv("dsc.email", unset = NA)
    }
    if (missing(password)) {
        password <- Sys.getenv("dsc.password", unset = NA)
    }
    if (is.na(email) | is.na(password)) {
        stop("email and/or password not provided.")
    }
    r <- httr::POST("https://www.drugshortagescanada.ca/api/v1/login", body = list(email = email, password = password))
    httr::stop_for_status(r, httr::content(r, as = "text"))
    authtoken <- httr::headers(r)$`auth-token`
    if (is.null(authtoken)) {
        stop("auth-token not provided by endpoint")
    }
    .state$dsc.authtoken <- authtoken

    authtoken
}

#' A single page of search results
#'
#' @param ... name = value pairs of query parameters.
#' @return An R representation of JSON search results via jsonlite::fromJSON
#'   records.
.dsc_search_once <- function(...) {
    authtoken <- dsc_authtoken(reuse_authtoken = T)
    r <- httr::GET("https://www.drugshortagescanada.ca/api/v1/search", httr::add_headers(`auth-token` = authtoken), query = list(...))
    httr::stop_for_status(r, httr::content(r, as = "text"))
    return(jsonlite::fromJSON(httr::content(r, as = "text", encoding = "UTF-8")))
}

#' Return all records from a search
#'
#' @param ... name = value pairs of query parameters.
#' @param max_pages Stop after this many pages of results
#'
#' @return  nested data.frame of results
.dsc_search_all <- function(..., max_pages = Inf) {
    query <- list(...)

    if (is.null(query$limit)) {
        query$limit <- 1000  # some optimistically large number. 0 doesn't work.
    }

    results <- do.call(.dsc_search_once, query)
    all_results <- list(results$data)
    pages <- 1

    while (results$remaining > 0 & pages < max_pages) {
        query$offset <- results$offset + nrow(results$data)
        results <- do.call(.dsc_search_once, query)
        all_results <- c(all_results, list(results$data))
        pages <- pages + 1
    }

    if (!length(unlist(all_results))) {
        return(as.data.frame(all_results))
    } else {
        return(jsonlite::rbind_pages(all_results))
    }
}

#' Query the DSC
#'
#' The search API is documented here:
#' https://www.drugshortagescanada.ca/blog/52
#'
#'
#'
#' @param ... A named list of query parameters.
#'   See the DSC Web API documentation for details on query parameters:
#'   https://www.drugshortagescanada.ca/blog/52
#' @param format The default 'tidy' returns a tibble with columns converted to
#'   the appropriate data types. Use 'json' to get results as returned by
#'   the API in JSON format after using jsonlite::rbind_pages to concatenate
#'   multiple pages of results.
#' @param max_pages Limit the number of requests made to the API. The number of
#'   results returned will depend on the total results, and the limit and offset
#'   parameters if supplied.
#' @return Results formatted according to format
#' @export
dsc_search <- function(..., format = "tidy", max_pages = Inf) {
    v1_params <- c("limit", "offset", "orderby", "order", "filter_status", "term", "din", "report_id")
    query <- list(...)
    unknown_params <- setdiff(names(query), v1_params)
    if (length(unknown_params) > 0) {
        unknown <- paste(unknown_params, collapse = ", ")
        allowed <- paste(v1_params, collapse = ", ")
        stop(paste0("Unexpected search parameter(s): ", unknown, "\nAllowed parameters are: ", allowed))
    }

    if (!(format %in% c("tidy", "json"))) {
        stop("unknown format")
    }

    results <- do.call(.dsc_search_all, list(..., max_pages = max_pages))
    return(switch(format, tidy = .dsc_tidy_results(results), json = results))
}

#' Tidy up DSC search results
#'
#' Accepts the raw JSON-to-data.frame search results object and converts columns
#' to their appropriate data types (dates, factors, etc)
#'
#' @param results search results as data.frame
#' @return tibble
.dsc_tidy_results <- function(results) {
    dplyr::as_tibble(jsonlite::flatten(results))
}
