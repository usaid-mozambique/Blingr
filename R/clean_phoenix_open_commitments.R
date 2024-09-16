#' Clean Phoenix Open Commitments dataset
#'
#' @param file A dataset with Phoenix Open Commitments data
#'
#' @return A cleaned dataset
#' @export
#'
#' @examples
#' \dontrun{
#'  df <- clean_phoenix_open_commitments(file)
#'  }


clean_phoenix_open_commitments <- function(file){
    temp <- file |>
        janitor::clean_names() |>
        dplyr::select(commitment_type, document_number, actg_line,
                      open_commitment_amt, bfy_fund, operating_unit, program_area,
                      distribution, program_element, original_date, bilateral_obl_number,
                      bilateral_actg_line, commitment_header_description,
                      fund_status, commitment_source)
    return(temp)
}
