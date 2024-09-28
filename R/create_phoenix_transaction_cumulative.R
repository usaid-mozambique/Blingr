
#' Create cumulative totals to the complete transaction dataset
#'
#' @param phoenix_transaction_df cleaned phoenix transaction dataset
#'
#' @return a complete transaction dataset with cumulative totals
#' @export
#'
#' @examples
#' \dontrun{
#'  df <- create_phoenix_transaction_cumulative(transaction_df)
#'  }
create_phoenix_transaction_cumulative <- function(phoenix_transaction_df){

    temp <- phoenix_transaction_df |>

    dplyr::group_by(award_number, period, program_area, fiscal_year) |>
        dplyr::summarise(dplyr::across(dplyr::where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

    phoenix_transaction_disbursement_fy <- phoenix_transaction_df |>
        dplyr::select(award_number, fiscal_year,  program_area, transaction_disbursement, period) |>
        dplyr::arrange(award_number, fiscal_year)  |>
        dplyr::group_by(award_number, fiscal_year, program_area) |>
        dplyr::mutate(cumulative_transaction_disbursement_fy = cumsum(transaction_disbursement))  |>
        dplyr::ungroup() |>
        dplyr::select(-c(fiscal_year, transaction_disbursement))

    temp <- phoenix_transaction_df |>
        dplyr::left_join(phoenix_transaction_disbursement_fy, by = c("award_number", "period", "program_area"))

    return(temp)
}

