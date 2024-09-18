
#' Title: Process Bilateral Obligation Account Lines
#'
#' @param file file containing raw data
#'
#' @return A tibble with the cleaned data
#' @export
#'
#' @examples
#' \dontrun{
#'  df <- clean_phoenix_bi_oblg_acc_lines(file)
#'  }

clean_phoenix_bi_oblg_acc_lines <- function(file){

temp <- file |>
    janitor::clean_names() |>
    dplyr::filter(funding_office_code == "IHO",
           fund_crcy_avail_for_subobl_amt > 1) |>

    #step 1:  keep relevant columns
    dplyr::select(document_number, actg_line, program_area,distribution,
           program_element, program_element_name, program_sub_element, fund_status, fund_fully_expired_year,
           fund_cancelled_year, fund_crcy_avail_for_subcommit_amt, fund_crcy_subcmmt_unsubobl_amt,
           fund_crcy_avail_for_subobl_amt, bfy_fund) |>
    dplyr::mutate(funding_year = stringr::str_extract(bfy_fund, "(?<=/)[0-9]{4}"),
                  funding_year = paste0("FY", funding_year)) |>

    #rename old program_areas to match the new
    dplyr::left_join(blingr::data_program_element_map, by = c("program_element" = "old_program_element")) |>
    dplyr::mutate(program_area = dplyr::if_else(is.na(new_program_area), program_area, new_program_area)) |>
    dplyr::select(-new_program_area) |>

    #add program area names
    dplyr::left_join(blingr::data_program_area_name_map, by = "program_area") |>
    dplyr::select(document_number, actg_line, fund_crcy_avail_for_subcommit_amt,fund_crcy_subcmmt_unsubobl_amt,
           fund_crcy_avail_for_subobl_amt, funding_year, bfy_fund, program_area_name,
           program_area, program_element_name, program_element, program_sub_element, distribution, fund_status, fund_fully_expired_year,
           fund_cancelled_year) |>
    dplyr::arrange(program_area) |>
    dplyr::rename('Document Number' = document_number,
           'Actg Line' = actg_line,
           'Avail for Subcommit Amt' = fund_crcy_avail_for_subcommit_amt,
           'SubcmMt Unsubobl Amt' = fund_crcy_subcmmt_unsubobl_amt,
           'Avail for Subobl Amt' = fund_crcy_avail_for_subobl_amt,
           'Funding Year' = funding_year,
           'BFY/Fund' = bfy_fund,
           'Program Area Name' = program_area_name,
           'Program Area' = program_area,
           'Program Element' = program_element,
           'Program Element Name' = program_element_name,
           'Program Sub Element' = program_sub_element,
           'Distribution' = distribution,
           'Fund Status' = fund_status,
           'Fund Fully Expired Year' = fund_fully_expired_year,
           'Fund Cancelled Year' = fund_cancelled_year)
}
