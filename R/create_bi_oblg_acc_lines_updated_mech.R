#' Creates a dataset from the updated bi_oblg_acc_lines file that has has mechanisms and comments as columns
#'
#' @param file bi_oblg_acc_lines file that has been updated by team
#' @param is_pepfar TRUE or FALSE depending on it if's PEPFAR
#'
#' @return A tibble with the updated bi_oblg_acc_lines
#' @export
#'
#' @examples
#' \dontrun{
#'  df <- create_bi_oblg_acc_lines_updated_mech(file, TRUE)
#'  }

create_bi_oblg_acc_lines_updated_mech <- function(file, is_pepfar) {
    # Read the file

    temp <- blingr::create_bi_oblg_acc_lines_updated_all(file, is_pepfar)

    num_cols <- ncol(temp)

    #make them numeric
    temp_mech <- temp |>
        dplyr::filter( `Document Number` != "Comments") |>  #remove comments line
        dplyr::mutate(dplyr::across(22:num_cols, as.numeric)) |>
        dplyr::select(Period, `Document Number`,`Program Area`, `Program Element`, `Program Sub-Element`,22:num_cols) |>
        tidyr::pivot_longer(cols = -c(1:5), names_to = "Mechanism", values_to = "Value") |>
        tidyr::drop_na(Value)


    #add comments
    temp_comments <- temp |>
        dplyr::filter(`Document Number` == "Comments") |>
        dplyr::select(22:num_cols) |>
        dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) |>
        tidyr::pivot_longer(cols = dplyr::everything(),
                            names_to = "Mechanism",
                            values_to = "Comments")


    temp_all <- temp_mech |>
        dplyr::left_join(temp_comments, by = c("Mechanism"))

    return(temp_all)
}
