#' Create cleaned phoenix pipeline dataset with only relevant award numbers
#'
#' @param all_award_number list of award numbers
#' @param PHOENIX_PIPELINE_PATH  path to phoenix pipeline files
#' @param obligation_type_filter list of obligation types
#' @param distribution_filter  list of distribution codes
#'
#' @return cleaned phoenix pipeline dataset
#' @export
#'
#' @examples
#' \dontrun{
#'  df <- process_phoenix_pipeline(phoenix_pipeline_path, award_number,
#'  obligation_type_filter, distribution_filter)
#'  }
clean_phoenix_pipeline <- function(PHOENIX_PIPELINE_PATH, all_award_number,
                                     obligation_type_filter, distribution_filter){

    #adjust column names
    lookup <- c("last_qtr_adj_amt" = "last_qtr_accrual_amt")

    temp <- readxl::read_xlsx(PHOENIX_PIPELINE_PATH,
                              col_types = "text") |>
        janitor::clean_names() |>
        dplyr::rename_with(~lookup[.x], dplyr::any_of(names(lookup))) |>
        dplyr::filter(obligation_type %in% obligation_type_filter,
                      distribution %in% distribution_filter) |>
        dplyr::select(document_amt, disbursement_amt,
                      undisbursed_amt, last_qtr_accrual_amt, document_number,
                      program_area, award_number, program_element,
                      bilateral_obl_number
        ) |>

        dplyr::mutate(filename = basename(PHOENIX_PIPELINE_PATH),
                      period = stringr::str_extract(filename,"^[^_]+" ),
                      document_amt = as.numeric(document_amt),
                      disbursement_amt = as.numeric(disbursement_amt),
                      undisbursed_amt = as.numeric(undisbursed_amt),
                      last_qtr_accrual_amt = as.numeric(last_qtr_accrual_amt),
                      award_number = dplyr::case_when(
                          award_number %in% all_award_number ~ award_number,
                          TRUE ~ document_number)
        ) |>

        # update program elements to new coding
        dplyr::left_join(
            blingr::data_program_element_map,
            by = c("program_element" = "old_program_element")
        ) |>

        # update program areas to new coding
        dplyr::mutate(program_area = dplyr::case_when(
            !is.na(new_program_area) ~ new_program_area,
            TRUE ~ program_area
        )) |>
        dplyr::select(-new_program_area) |>

        #add program_area_name
        dplyr::left_join(blingr::data_program_area_name_map, by = "program_area") |>

        # update program area

        dplyr::filter(award_number %in% all_award_number) |>
        dplyr::group_by(award_number, period, program_area, program_area_name, bilateral_obl_number) |>
        dplyr::summarise(dplyr::across(dplyr::where(is.numeric), sum), .groups = "drop")


    return(temp)
}




