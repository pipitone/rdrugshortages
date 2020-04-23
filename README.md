# rdrugshortages

Access drug shortage data in R. This package currently supports accessing the following databases

1. [Drug Shortages Canada](https://drugshortagescanada.ca)
    Canada-wide mandatory shortage reporting database. You must create an
    account to access the database via the API but you can search and export
    subsets of the data on the site.
    
1. [Drug Product Database](https://www.canada.ca/en/health-canada/services/drugs-health-products/drug-products/drug-product-database.html)
    This database, by Health Canada, provides a current and historical snapshot
    of all available drug products, and includes supplementary information such
    as manufacturer information. There are no shortage reports in this database,
    but it can be used in conjunction with shortage reports by cross-referencing
    the Drug Identification Number (DIN) field.

## Installation Instructions

```r
install.packages('rdrugshortages')

# Or, install the latest development version:
devtools::install_github('pipitone/rdrugshortages')
```

## Use

As mentioned above, you must create an account with Drug Shortages Canada (DSC)
in order to access the database via the API.  Once you have done this, you can
put your credentials in your
[`.Renviron`](https://www.dartistics.com/renviron.html) file which gets loaded
every time you start R:

```
# ~/.Renviron
dsc.email = "youremail@domain.com"
dsc.password = "yourpassword"
```

You can check that this worked by typing the following into a new R session: 

```r
> Sys.getenv('dsc.email')
[1] "youremail@domain.com"
```

You can access the search API via the function `dsc_search()`. This function
will automatically attempt to log in to with your account credentials when you
first use it, and then return the results of the search: 

```r
> results = dsc_search(term="diltiazem")
> head(results)
# A tibble: 6 x 93
     id created_date din   company_name atc_number atc_description discontinuation… updated_date status
  <int> <chr>        <chr> <chr>        <chr>      <chr>           <chr>            <chr>        <chr> 
1 81418 2019-04-11T… 0237… TEVA CANADA… C08DB      SELECTIVE CALC… 2019-03-15T00:0… 2019-04-11T… disco…
2 82552 2019-04-26T… 0237… TEVA CANADA… C08DB      SELECTIVE CALC… NA               2019-04-26T… activ…
3 82555 2019-04-26T… 0237… TEVA CANADA… C08DB      SELECTIVE CALC… NA               2019-04-26T… activ…
4 61340 2018-09-12T… 0237… TEVA CANADA… C08DB01    SELECTIVE CALC… NA               2018-10-11T… activ…
5  9901 2017-05-10T… 0237… ACTAVIS PHA… C08DB      SELECTIVE CALC… NA               2017-09-19T… resol…
6 81381 2019-04-11T… 0237… TEVA CANADA… C08DB      SELECTIVE CALC… 2019-01-09T00:0… 2019-04-11T… disco…
```
The result set returned by `dsc_search()` is a
[`tibble`](https://tibble.tidyverse.org/) which is based on the JSON blob
returned by the API.

The full specification for the DSC API is available [here](https://www.drugshortagescanada.ca/blog/52). 

Note that `rdrugshortages` assembles the entire result set by making multiple
requests to the API when there are more results than can be returned by the API
in a single request. You should be aware that API has been rate-limited to 1000
requests per hour.

To download the entire DSC database, simple run `dsc_search()` without any
search terms.
