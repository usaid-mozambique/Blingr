
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
        dplyr::mutate(transaction_disbursement = tidyr::replace_na(transaction_disbursement, 0)) |>   #remove any NA values before calculating
        dplyr::group_by(award_number, program_area, fiscal_year) |>
        dplyr::arrange(period) |>
        dplyr::mutate(
            transaction_disbursement_cumulative_FY = cumsum(transaction_disbursement)  # Calculate cumulative sum within the year
        )  |>
        dplyr::ungroup() |>
        dplyr::select(-c(fiscal_year)) |>
        dplyr::group_by(award_number, program_area) |>
        dplyr::mutate(
            transaction_disbursement_cumulative_total = cumsum(transaction_disbursement)  # Calculate cumulative sum across the years
        ) |>
        dplyr::ungroup()

    return(temp)
}
