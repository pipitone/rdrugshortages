context("DSC authtoken")
test_that <- function(desc, code) {
    Sys.unsetenv("dsc.email")
    Sys.unsetenv("dsc.password")
    .state$dsc.authtoken <- NULL
    testthat::test_that(desc, code)
}

with_mock_api({
    test_that("No credentials causes an error", {
        expect_error(dsc_authtoken(), "*.password not provided")
    })
    test_that("We can login with explicit credentials", {
        authtoken <- dsc_authtoken(email = "email", password = "password")
        expect_equal(authtoken, "0c55f947024dd8e764d162dc2f5ccb83")
    })
    test_that("We can login with email/pass from environment variables", {
        Sys.setenv(dsc.email = "email")
        Sys.setenv(dsc.password = "password")
        authtoken <- dsc_authtoken()
        expect_equal(authtoken, "0c55f947024dd8e764d162dc2f5ccb83")
    })
})

test_that("Existing authtoken can be reused without making a request", {
    with_mock_api({
        dsc_authtoken(email = "email", password = "password")
    })
    without_internet({
        authtoken <- dsc_authtoken(reuse_authtoken = T)
        expect_equal(authtoken, "0c55f947024dd8e764d162dc2f5ccb83")
    })
})
