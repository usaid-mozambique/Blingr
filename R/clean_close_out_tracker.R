#' Clean data in the closeout tracker
#'
#' @param file closeout tracker file
#'
#' @return a cleaned dataset
#' @export
#'
#' @examples
#' \dontrun{
#'  df <- clean_close_out_tracker(file)
#'  }

clean_close_out_tracker <- function(file){

    temp <- readxl::read_xlsx(file,
                              sheet = "IHO- Priority",
                              skip = 1) |>
        janitor::clean_names() |>
        dplyr::mutate(award_number = stringr::str_trim(award_number),
               filename = basename(file),
               period = stringr::str_extract(filename, "^[^_]+"),
        ) |>
        dplyr::select(award_number, to_be_deobligated, status, period) |>
        tidyr::drop_na(award_number)

    return(temp)
}
