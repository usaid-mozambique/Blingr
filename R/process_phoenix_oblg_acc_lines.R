#' Create a clean dataset from phoneix obligation accounting lines dataset
#'
#' @param path path to the phoenix obligation accounting lines dataset
#' @param award_numbers a vector of award numbers to filter the dataset
#' @param OBLIGATION_TYPE_FILTER a vector of obligation types to filter the dataset
#' @param DISTRIBUTION_FILTER a vector of distribution to filter the dataset
#'
#' @return a clean dataset
#' @export
#'
#' @examples
#' \dontrun{
#'  df <- process_phoenix_oblg_acc_lines(phoenix_path, award_numbers,
#'  obligation_type_filter, distribution_filter)
#'  }

process_phoenix_oblg_acc_lines <- function(path, award_numbers,OBLIGATION_TYPE_FILTER,DISTRIBUTION_FILTER){

    temp <- readxl::read_xlsx(path) |>
        janitor::clean_names() |>
        dplyr::select(award_number, document_number, obligation_type, program_area, document_date,
               obligation_amt, disbursement_amt, unliquidated_amt, outstanding_advance_amt,
               fund_status, distribution, program_element) |>

    dplyr::mutate(award_number = dplyr::case_when(
        award_number %in% award_numbers ~ award_number,
        TRUE ~ document_number),
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
                                      program_area == "DR.4" ~ "Civil Society",
                                      program_area == "DR.6" ~ "Human Rights",
                                      program_area == "DR.3" ~ "Political Competition and Consensus-Building",
                                      TRUE ~ as.character(program_area)),
        period_month = lubridate::floor_date(document_date, unit = "month"),
    ) |>
        dplyr::filter(award_number %in% award_numbers,
               obligation_type %in% OBLIGATION_TYPE_FILTER,
               distribution %in% DISTRIBUTION_FILTER) |>
        dplyr::select(-c(document_number, obligation_type, distribution, program_element,
                  document_date)) |>
        dplyr::group_by(award_number, program_area, fund_status, program_area_name, period_month) |>
        dplyr::summarise(dplyr::across(dplyr::everything(), sum), .groups = "drop")

    return(temp)

}
