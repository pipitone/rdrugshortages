#' Downloads the DPD extracts and unzips them
#'
#' This function downloads all of the available sections (current, inactive,
#' etc..) of the current DPD extract.
#'
#' @param data_dir  target directory to download to
#' @export
dpd_download <- function(data_dir = "data/") {
    dpd_base_url <- "https://www.canada.ca/content/dam/hc-sc/documents/services/drug-product-database/"

    if (!dir.exists(data_dir)) {
        dir.create(data_dir)
    }

    for (f in c("allfiles.zip", "allfiles_ia.zip", "allfiles_ap.zip", "allfiles_dr.zip")) {
        destfile <- paste0(data_dir, f)
        url <- paste0(dpd_base_url, f)
        utils::download.file(url, destfile, quiet = T)
        utils::unzip(destfile, exdir = data_dir)
    }
}

#' Loads the DPD into memory from a downloaded extract
#'
#' Each section of the DPD extract is merged together on a per-table basis.
#'
#' @param data_dir    target directory where extract is stored. Use
#'   dpd_download() to fetch and unpack the latest extract.
#' @return A named list of each table as a data.frame
#' @export
#' @importFrom magrittr %>%
dpd_load <- function(data_dir = "data/") {
    tables <- list(
      comp = c("DRUG_CODE", "MFR_CODE", "COMPANY_CODE", "COMPANY_NAME", "COMPANY_TYPE", "ADDRESS_MAILING_FLAG", "ADDRESS_BILLING_FLAG", "ADDRESS_NOTIFICATION_FLAG",  "ADDRESS_OTHER", "SUITE_NUMBER", "STREET_NAME", "CITY_NAME", "PROVINCE", "COUNTRY", "POSTAL_CODE", "POST_OFFICE_BOX", "PROVINCE_F", "COUNTRY_F"),
      form = c("DRUG_CODE", "PHARM_FORM_CODE", "PHARMACEUTICAL_FORM", "PHARMACEUTICAL_FORM_F"),
      ingred = c("DRUG_CODE", "ACTIVE_INGREDIENT_CODE", "INGREDIENT",  "INGREDIENT_SUPPLIED_IND", "STRENGTH", "STRENGTH_UNIT", "STRENGTH_TYPE", "DOSAGE_VALUE", "BASE", "DOSAGE_UNIT", "NOTES", "INGREDIENT_F", "STRENGTH_UNIT_F", "STRENGTH_TYPE_F", "DOSAGE_UNIT_F"),
      package = c("DRUG_CODE", "UPC", "PACKAGE_SIZE_UNIT", "PACKAGE_TYPE", "PACKAGE_SIZE", "PRODUCT_INFORMATION",  "PACKAGE_SIZE_UNIT_F", "PACKAGE_TYPE_F"),
      pharm = c("DRUG_CODE", "PHARMACEUTICAL_STD"),
      drug = c("DRUG_CODE", "PRODUCT_CATEGORIZATION", "CLASS",  "DRUG_IDENTIFICATION_NUMBER", "BRAND_NAME", "DESCRIPTOR", "PEDIATRIC_FLAG", "ACCESSION_NUMBER", "NUMBER_OF_AIS", "LAST_UPDATE_DATE", "AI_GROUP_NO", "CLASS_F", "BRAND_NAME_F", "DESCRIPTOR_F"),
      route = c("DRUG_CODE", "ROUTE_OF_ADMINISTRATION_CODE", "ROUTE_OF_ADMINISTRATION", "ROUTE_OF_ADMINISTRATION_F"),
      sched = c("DRUG_CODE", "SCHEDULE", "SCHEDULE_F"),
      status = c("DRUG_CODE", "CURRENT_STATUS_FLAG", "STATUS", "HISTORY_DATE", "STATUS_F", "LOT_NUMBER",  "EXPIRATION_DATE"),
      ther = c("DRUG_CODE", "TC_ATC_NUMBER", "TC_ATC", "TC_AHFS_NUMBER", "TC_AHFS", "TC_ATC_F", "TC_AHFS_F"),
      vet = c("DRUG_CODE",  "VET_SPECIES", "VET_SUB_SPECIES", "VET_SPECIES_F"))

    dpd <- list()
    for (table_name in names(tables)) {
        headers <- tables[[table_name]]

        dpd[[table_name]] <- dir(data_dir, pattern = paste0(table_name, ".*.txt$"), full.names = T) %>%
          purrr::keep(function(x) file.size(x) > 0) %>%
          purrr::map(readr::read_csv,
            col_names = headers,
            locale = readr::locale("en",  date_format = "%d-%b-%Y"),
            col_types = purrr::map(headers, function(x) dplyr::case_when(endsWith(x, "_DATE") ~ "D", x == "DRUG_CODE" ~ "i", T ~ "c")) %>%
              stats::setNames(headers), progress = F) %>%
          purrr::reduce(rbind)
    }

    return(dpd)
}
