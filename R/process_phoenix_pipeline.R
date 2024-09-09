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
process_phoenix_pipeline <- function(PHOENIX_PIPELINE_PATH, all_award_number,
                                     obligation_type_filter, distribution_filter){

    #adjust column names
    lookup <- c("last_qtr_adj_amt" = "last_qtr_accrual_amt")

    temp <- readxl::read_xlsx(PHOENIX_PIPELINE_PATH,
                              col_types = "text") |>
        janitor::clean_names() |>
        dplyr::rename_with(~lookup[.x], dplyr::any_of(names(lookup))) |>
        dplyr::filter(obligation_type %in% obligation_type_filter,
                      distribution %in% distribution_filter) |>
        dplyr::select(document_amt,
                      obligation_amt, subobligation_amt, disbursement_amt,
                      undisbursed_amt, last_qtr_accrual_amt,document_number,
                      program_area, award_number, program_element,pipeline_amt,
                      bilateral_obl_number
        ) |>

        dplyr::mutate(filename = basename(PHOENIX_PIPELINE_PATH),
                      period = stringr::str_extract(filename,"^[^_]+" ),
                      document_amt = as.numeric(document_amt),
                      disbursement_amt = as.numeric(disbursement_amt),
                      undisbursed_amt = as.numeric(undisbursed_amt),
                      last_qtr_accrual_amt = as.numeric(last_qtr_accrual_amt),
                      pipeline_amt = as.numeric(pipeline_amt),
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
                      award_number = dplyr::case_when(
                          award_number %in% all_award_number ~ award_number,
                          TRUE ~ document_number) ,
                      total_disbursement_outlays = disbursement_amt + last_qtr_accrual_amt

        ) |>

        dplyr::filter(award_number %in% all_award_number) |>
        dplyr::group_by(award_number, period, program_area, bilateral_obl_number) |>
        dplyr::summarise(dplyr::across(dplyr::where(is.numeric), sum), .groups = "drop")

    return(temp)
}

