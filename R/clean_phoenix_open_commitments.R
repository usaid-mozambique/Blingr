#' Clean Phoenix Open Commitments dataset
#'
#' @param file A dataset with Phoenix Open Commitments data
#'
#' @return A cleaned dataset
#' @export
#'
#' @examples
#' \dontrun{
#'  df <- clean_phoenix_open_commitments(file)
#'  }


clean_phoenix_open_commitments <- function(file){
    temp <- file |>
        janitor::clean_names() |>
        tidyr::drop_na(document_number) |>
        dplyr::select(commitment_type, document_number, actg_line,
                      open_commitment_amt, bfy_fund, operating_unit, program_area,
                      distribution, program_element, original_date, bilateral_obl_number,
                      bilateral_actg_line, commitment_header_description,
                      fund_status, commitment_source) |>
        dplyr::mutate( original_date = lubridate::as_date(original_date),
                       outstanding_days_of_cmmt = as.numeric(lubridate::today() - original_date),
                       cmmt_less_than_30_days = dplyr::if_else(outstanding_days_of_cmmt <= 30, open_commitment_amt, 0),
                       cmmt_amt_between_31_to_90_days = dplyr::if_else(outstanding_days_of_cmmt > 30 & outstanding_days_of_cmmt <= 90, open_commitment_amt, 0),
                       cmmt_amt_between_91_to_180_days = dplyr::if_else(outstanding_days_of_cmmt > 90 & outstanding_days_of_cmmt <= 180, open_commitment_amt, 0),
                       cmmt_amt_above_180_days = dplyr::if_else(outstanding_days_of_cmmt > 180, open_commitment_amt, 0),
                       activity = ""
        ) |>
        dplyr::filter(operating_unit == "MOZAMBIQUE") |>
        tidyr::drop_na(document_number) |>

        #rename old program_areas to match the new
        dplyr::left_join(blingr::data_program_element_map, by = c("program_element" = "old_program_element")) |>
        dplyr::mutate(program_area = dplyr::if_else(is.na(new_program_area), program_area, new_program_area)) |>
        dplyr::select(-new_program_area) |>

        #add program area names
        dplyr::left_join(blingr::data_program_area_name_map, by = "program_area") |>
        dplyr::select(activity, commitment_type, document_number, actg_line,
                      open_commitment_amt, bfy_fund, operating_unit, program_area, program_area_name,
                      distribution, program_element, original_date,
                      outstanding_days_of_cmmt, cmmt_less_than_30_days,
                      cmmt_amt_between_31_to_90_days, cmmt_amt_between_91_to_180_days,
                      cmmt_amt_above_180_days, bilateral_obl_number,
                      bilateral_actg_line, commitment_header_description,
                      fund_status, commitment_source)
    return(temp)
}

