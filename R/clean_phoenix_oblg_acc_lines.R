#' Create a clean dataset from phoneix obligation accounting lines dataset
#'
#' @param award_numbers a vector of award numbers to filter the dataset
#' @param OBLIGATION_TYPE_FILTER a vector of obligation types to filter the dataset
#' @param DISTRIBUTION_FILTER a vector of distribution to filter the dataset
#' @param file  a dataset to clean
#'
#' @return a clean dataset
#' @export
#'
#' @examples
#' \dontrun{
#'  df <- process_phoenix_oblg_acc_lines(file, award_numbers,
#'  obligation_type_filter, distribution_filter)
#'  }

clean_phoenix_oblg_acc_lines <- function(file, award_numbers,OBLIGATION_TYPE_FILTER, DISTRIBUTION_FILTER){

    temp <- suppressWarnings(readxl::read_xlsx(file)) |>
        janitor::clean_names() |>
        dplyr::select(award_number, document_number, obligation_type, program_area, document_date,
                      obligation_amt, disbursement_amt, unliquidated_amt, outstanding_advance_amt,
                      fund_status, distribution, program_element) |>
        dplyr::mutate(award_number = dplyr::case_when(
            award_number %in% active_awards_number ~ award_number,
            TRUE ~ document_number),
            period_month = lubridate::floor_date(document_date, unit = "month")
        )|>
        dplyr::filter(award_number %in% active_awards_number,
                      obligation_type %in% OBLIGATION_TYPE_FILTER,
                      distribution %in% DISTRIBUTION_FILTER) |>

        dplyr::left_join(blingr::data_program_element_map, by = c("program_element" = "old_program_element")) |>
        dplyr::mutate(program_area = dplyr::case_when(!is.na(new_program_area) ~ new_program_area,
                                                      TRUE ~ program_area)) |>
        dplyr::left_join(blingr::data_program_area_name_map, by = "program_area") |>
        dplyr::select(-c(document_number, obligation_type, distribution,
                         document_date, new_program_area, program_element)) |>

        dplyr::group_by(award_number, program_area, fund_status, program_area_name, period_month) |>
        dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(.x, na.rm = TRUE)), .groups = "drop")

    return(temp)
}


