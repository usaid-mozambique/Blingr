#' Creates a dataset from the updated bi_oblg_acc_lines file that excludes the mechanism details
#'
#' @param file bi_oblg_acc_lines file that has been updated by team
#' @param is_pepfar TRUE or FALSE depending on it if's PEPFAR
#'
#' @return A tibble with the updated bi_oblg_acc_lines
#' @export
#'
#' @examples
#' \dontrun{
#' df <- create_bi_oblg_acc_lines_updated_no_mech(file, TRUE)
#' }

create_bi_oblg_acc_lines_updated_no_mech <- function(file, is_pepfar) {

    temp <- blingr::create_bi_oblg_acc_lines_updated_all(file, is_pepfar)

    temp <- temp |>
        dplyr::select(1:24) |>
        dplyr::filter(!is.na(`Document Number`),
                      `Document Number` != "",
                      `Document Number` != "Comments")

    return(temp)


}
