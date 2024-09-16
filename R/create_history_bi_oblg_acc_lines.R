#' Create history of bi_oblg_acc_lines
#'
#' @param file A dataset with Phoenix Open Commitments data
#'
#' @return A cleaned dataset
#' @export
#'
#' @examples
#' \dontrun{
#'  df <- create_history_bi_oblg_acc_lines(file)
#'  }

create_history_bi_oblg_acc_lines <- function(file) {

    temp <- readxl::read_xlsx(file) |>
        dplyr::select(-dplyr::starts_with("...")) |>  # Remove auto-generated column names
        tidyr::drop_na("Document Number") |>

        dplyr::mutate(
            filename = basename(file),
            Period = stringr::str_extract(filename, "^[^_]+"),
            Period = lubridate::ym(Period),
            `Actg Line` = as.numeric(`Actg Line`),
            `Fund Fully Expired Year` = as.numeric(`Fund Fully Expired Year`),
            `Fund Cancelled Year` = as.numeric(`Fund Cancelled Year`),
        ) |>
        dplyr::select(-c(filename, Total)) |>
        dplyr::select(Period, dplyr::everything())
    # Determine the total number of columns
    num_cols <- ncol(temp)

    # Convert all columns from 16th onwards to numeric using all_of()
    temp <- temp |>
        dplyr::mutate(dplyr::across(16:num_cols, as.numeric)) |>

        # Pivoting data to long format
        tidyr::pivot_longer(cols = -dplyr::all_of(1:15), names_to = "Mechanism", values_to = "Value") |>
        dplyr::mutate(Value = as.numeric(Value))

    return(temp)
}
