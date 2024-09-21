#' Create history of bi_oblg_acc_lines
#'
#' @param file A dataset with Phoenix Open Commitments data
#' @param is_pepfar TRUE if a PEPFAR dataset, FALSE if a non_pepfar dataset
#'
#' @return A cleaned dataset
#' @export
#'
#' @examples
#' \dontrun{
#'  df <- create_history_bi_oblg_acc_lines(file)
#'  }

create_history_bi_oblg_acc_lines <- function(file, is_pepfar) {
    # Read the file
    if (is_pepfar) {
        temp_read <- readxl::read_xlsx(file, skip = 10)

    } else {
        temp_read <- readxl::read_xlsx(file)
    }

    temp <- temp_read |>
        dplyr::select(-dplyr::starts_with("...")) |>  # Remove auto-generated column names
        dplyr::filter(!is.na(`Document Number`),
                      `Document Number` != "",
                      `Document Number` != "Comments") |>

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
    temp_all <- temp |>
        dplyr::mutate(dplyr::across(25:num_cols, as.numeric)) |>

        # Pivoting data to long format
        tidyr::pivot_longer(
            cols = -dplyr::all_of(1:24),
            names_to = "Mechanism",
            values_to = "Value"
        ) |>
        dplyr::mutate(Value = as.numeric(Value)) |>
        tidyr::drop_na(Value)


    #add comments
    temp_comments <- temp_read |>
        dplyr::filter(`Document Number` == "Comments") |>
        dplyr::select(-c(1:23)) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
        tidyr::pivot_longer(cols = dplyr::everything(),
                     names_to = "Mechanism",
                     values_to = "Comments")


    temp_all <- temp_all |>
        dplyr::left_join(temp_comments, by = c("Mechanism"))


    return(temp_all)
}


