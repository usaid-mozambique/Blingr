#' Function cleans the bi_oblg_acc_lines file that has been updated by the team
#'
#' @param file bi_oblg_acc_lines file that has been updated by team
#' @param is_pepfar TRUE or FALSE depending on it if's PEPFAR
#'
#' @return A tibble with the updated bi_oblg_acc_lines
#' @export
#'
#' @examples
#' \dontrun{
#'  df <- create_bi_oblg_acc_lines_updated_all(file, TRUE)
#'  }

create_bi_oblg_acc_lines_updated_all <- function(file, is_pepfar){

    # Read the file
    if (is_pepfar) {
        temp_read <- readxl::read_xlsx(file,
                                       skip = 10,
                                       col_types = "text")

    } else {
        temp_read <- readxl::read_xlsx(file,
                                       col_types = "text")
    }

    temp <- temp_read|>
        dplyr::filter(!is.na(`Document Number`),
                      `Document Number` != "") |>
        dplyr::select(-dplyr::starts_with("...")) |>  # Remove auto-generated column names
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

    return(temp)

}
