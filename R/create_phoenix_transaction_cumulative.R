
#' Create cumulative totals to the complete transaction dataset
#'
#' @param phoenix_transaction_df cleaned phoenix transaction dataset
#' @param calendar_type calendar type to be used either fiscal or calendar
#'
#' @return a complete transaction dataset with cumulative totals
#' @export
#'
#' @examples
#' \dontrun{
#'  df <- create_phoenix_transaction_cumulative(transaction_df, fiscal)
#'  }
create_phoenix_transaction_cumulative <- function(phoenix_transaction_df, calendar_type = "fiscal"){

    calendar_type <- ifelse(calendar_type == "fiscal", "fiscal_year", "calendar_year")

    # Convert the 'calendar_type' string into a symbol
    calendar_type_sym <- dplyr::sym(calendar_type)

    temp <- phoenix_transaction_df |>

    dplyr::group_by(award_number, period, program_area, !!calendar_type_sym) |>
        dplyr::summarise(dplyr::across(dplyr::where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")

    phoenix_transaction_disbursement_fy <- phoenix_transaction_df |>
        dplyr::select(award_number, !!calendar_type_sym,  program_area, transaction_disbursement, period) |>
        dplyr::arrange(award_number, !!calendar_type_sym)  |>
        dplyr::group_by(award_number, !!calendar_type_sym, program_area) |>
        dplyr::mutate(cumulative_transaction_disbursement_fy = cumsum(transaction_disbursement))  |>
        dplyr::ungroup() |>
        dplyr::select(-c(!!calendar_type_sym, transaction_disbursement))

    temp <- phoenix_transaction_df |>
        dplyr::left_join(phoenix_transaction_disbursement_fy, by = c("award_number", "period", "program_area")) |>
        dplyr::select(-c(!!calendar_type_sym))

    return(temp)
}
?sym
