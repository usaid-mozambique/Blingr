
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

    # Step 1: Summarize numeric columns grouped by relevant keys
    temp <- phoenix_transaction_df |>
        dplyr::group_by(award_number, program_area, fiscal_year) |>
        dplyr::arrange(period) |>
        dplyr::mutate(
            transaction_disbursement_cumulative = cumsum(transaction_disbursement)  # Calculate cumulative sum within the year
        )  |>
        dplyr::ungroup() |>
        dplyr::select(-c(fiscal_year, fiscal_quarter))

    return(temp)
}
