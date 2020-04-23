context("DSC Search")
library(rdrugshortages)

test_that <- function(desc, code) {
    Sys.unsetenv("dsc.email")
    Sys.unsetenv("dsc.password")
    .state$dsc.authtoken <- "token"
    testthat::test_that(desc, code)
}

with_mock_api({
    test_that("Search should attempt to get an authtoken if one is not set", {
        .state$dsc.authtoken <- NULL
        expect_error(dsc_search(report_id = 75200), "*.password not provided")
    })

    test_that("We can get a single report", {
        results <- dsc_search(report_id = 75200)
        expect_is(results, "data.frame")
        expect_equal(nrow(results), 1)
    })

    test_that("We can get a single page of results", {
        results <- dsc_search(max_pages = 1)
        expect_is(results, "data.frame")
        expect_equal(nrow(results), 50)
    })

    test_that("We can get a multiple pages of results", {
        results <- dsc_search(max_pages = 2)
        expect_is(results, "data.frame")
        expect_equal(nrow(results), 100)
    })

    test_that("We can get a single page of json results", {
        results <- dsc_search(format = "json", max_pages = 1)
        expect_is(results, "data.frame")
        expect_equal(nrow(results), 50)
    })

    test_that("An error is thrown if we specify an unknown format", {
        expect_error(dsc_search(format = "csv"), "unknown format")
    })

    test_that("We can return 0 results", {
        results <- dsc_search(din = "NaN")
        expect_equal(nrow(results), 0)
    })

    test_that("All API v1 parameters can be used", {
        results <- dsc_search(max_pages = 1,
                              orderby = "id",
                              order = "asc",
                              filter_status = "resolved",
                              term = "venlafaxine",
                              din = 1,
                              report_id = 1)
        expect_equal(nrow(results), 0)
    })

    test_that("Using non-API v1 parameters causes an error", {
        expect_error(dsc_search(page = 1), "Unexpected search parameter")
        expect_error(dsc_search(total = 1), "Unexpected search parameter")
        expect_error(dsc_search(remaining = 1), "Unexpected search parameter")
        expect_error(dsc_search(data = 1), "Unexpected search parameter")
        expect_error(dsc_search(total_pages = 1), "Unexpected search parameter")
        expect_error(dsc_search(not_a_parameter = 1), "Unexpected search parameter")
    })
})
