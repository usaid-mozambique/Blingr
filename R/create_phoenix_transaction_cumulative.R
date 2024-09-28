
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
        dplyr::group_by(award_number, period, program_area, fiscal_year) |>
        dplyr::summarise(dplyr::across(dplyr::where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

    # Step 2: Calculate cumulative transaction disbursement per fiscal year
    phoenix_transaction_disbursement_fy <- phoenix_transaction_df |>
        dplyr::select(award_number, fiscal_year, program_area, transaction_disbursement, period) |>
        dplyr::arrange(award_number, fiscal_year, period)  |>
        dplyr::group_by(award_number, fiscal_year, program_area) |>
        dplyr::mutate(cumulative_transaction_disbursement_fy = cumsum(transaction_disbursement)) |>
        dplyr::ungroup()

    # Step 3: Merge cumulative disbursement data back into the summary
    temp <- temp |>
        dplyr::left_join(phoenix_transaction_disbursement_fy,
                         by = c("award_number", "program_area", "fiscal_year"),
                         suffix = c("", ".y")) |>
        dplyr::select(-transaction_disbursement.y)  # Remove this column after merging

    return(temp)
}

