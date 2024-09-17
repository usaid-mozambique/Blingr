#' Combine all open commitments datasets
#'
#' @param file A dataset with Phoenix Open Commitments data
#'
#' @return A cleaned dataset
#' @export
#'
#' @examples
#' \dontrun{
#'  df <- create_history_open_commitments(file)
#'  }
create_history_open_commitments <- function(file) {

    temp <- readxl::read_xlsx(file) |>
        janitor::clean_names() |>
        tidyr::drop_na(document_number) |>
        dplyr::mutate(
            filename = basename(file),
            period = stringr::str_extract(filename, "^[^_]+"),
            period = lubridate::ym(period)
        ) |>
        dplyr::select(-c(filename)) |>
        dplyr::mutate( original_date = lubridate::as_date(original_date),
                       outstanding_days_of_cmmt = as.numeric(period - original_date),
                       cmmt_less_than_30_days = dplyr::if_else(outstanding_days_of_cmmt <= 30, open_commitment_amt, 0),
                       cmmt_amt_between_31_to_90_days = dplyr::if_else(outstanding_days_of_cmmt > 30 & outstanding_days_of_cmmt <= 90, open_commitment_amt, 0),
                       cmmt_amt_between_91_to_180_days = dplyr::if_else(outstanding_days_of_cmmt > 90 & outstanding_days_of_cmmt <= 180, open_commitment_amt, 0),
                       cmmt_amt_above_180_days = dplyr::if_else(outstanding_days_of_cmmt > 180, open_commitment_amt, 0),
        ) |>
        dplyr::select(period, dplyr::everything())

    return(temp)
}
