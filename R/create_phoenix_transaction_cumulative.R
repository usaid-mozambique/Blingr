
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
    temp_cumulative <- phoenix_transaction_df |>
        dplyr::select(fiscal_year, program_area, award_number, transaction_disbursement) |>
        dplyr::group_by(award_number, program_area, fiscal_year) |>
        dplyr::summarise(cumulative_transaction_disbursement_fy = sum(transaction_disbursement, na.rm = TRUE), .groups = "drop")

    # Step 3: Merge cumulative disbursement data back into the summary
    temp <- phoenix_transaction_df |>
        dplyr::left_join(temp_cumulative,
                         by = c("award_number", "program_area", "fiscal_year"))

    return(temp)
}

