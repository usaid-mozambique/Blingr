
#' Title: Process Bilateral Obligation Account Lines
#'
#' @param path Path to the file containing the raw data
#'
#' @return A tibble with the cleaned data
#' @export
#'
#' @examples
#' \dontrun{
#'  df <- process_bilateral_oblg_acc_lines(path)
#'  }

process_bilateral_oblg_acc_lines <- function(path){

bilateral_oblg_acc_lines_raw <- readxl::read_excel(path)

bi_ablg_acc_lines_clean <- bilateral_oblg_acc_lines_raw |>
    janitor::clean_names() |>
    dplyr::filter(funding_office_code == "IHO",
           fund_crcy_avail_for_subobl_amt > 1) |>

    #step 1:  keep relevant columns
    dplyr::select(document_number, actg_line, program_area,distribution,
           program_element, program_sub_element, fund_status, fund_fully_expired_year,
           fund_cancelled_year, fund_crcy_avail_for_subcommit_amt, fund_crcy_subcmmt_unsubobl_amt,
           fund_crcy_avail_for_subobl_amt, bfy_fund) |>
    dplyr::mutate(funding_year = stringr::str_extract(bfy_fund, "(?<=/)[0-9]{4}"),
           #renaming old program areas to new
           program_area = dplyr::case_when(program_element == "A047" ~ "HL.1",
                                    program_element == "A048"~ "HL.2",
                                    program_element == "A049"~ "HL.3",
                                    program_element == "A050"~ "HL.4",
                                    program_element == "A051"~ "HL.5",
                                    program_element == "A052"~ "HL.6",
                                    program_element == "A053"~ "HL.7",
                                    program_element == "A054"~ "HL.8",
                                    program_element == "A142"~ "HL.9",
                                    program_element == "A141"~ "PO.2",
                                    program_element == "A140"~ "PO.1",
                                    TRUE ~ program_area),
           program_area_name = dplyr::case_when(program_area == "HL.1" ~ "HIV/AIDS",
                                         program_area == "HL.6" ~ "MCH",
                                         program_area == "HL.4"~ "GHS" ,
                                         program_area == "HL.9"~ "Nutrition",
                                         program_area == "HL.7"~  "FP/RH",
                                         program_area == "HL.2" ~ "TB",
                                         program_area == "HL.3" ~ "Malaria",
                                         program_area == "HL.8" ~ "WASH",
                                         program_area == "PO.1"~ "PD&L",
                                         program_area == "PO.2" ~"A&O",
                                         program_area == "DR.4" ~ "Civil Society",
                                         program_area == "DR.6" ~ "Human Rights",
                                         program_area == "DR.3" ~ "Political Competition and Consensus-Building",
                                         TRUE ~ as.character(program_area)),) |>
    dplyr::select(document_number, actg_line, fund_crcy_avail_for_subcommit_amt,fund_crcy_subcmmt_unsubobl_amt,
           fund_crcy_avail_for_subobl_amt, funding_year, bfy_fund, program_area_name,
           program_area, program_element, distribution, fund_status, fund_fully_expired_year,
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
           'Distribution' = distribution,
           'Fund Status' = fund_status,
           'Fund Fully Expired Year' = fund_fully_expired_year,
           'Fund Cancelled Year' = fund_cancelled_year)
}
