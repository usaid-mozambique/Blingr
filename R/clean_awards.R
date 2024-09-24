#' Create a clean dataset from Active Awards google sheet (template from USAID Mozambique)
#'
#' @param sheetname name of the sheet.  Either Active Awards or Expired Awards
#' @param file filename
#'
#' @return A clean dataset with the following columns:sub_sector, activity_name, award_number, total_estimated_cost,
#'start_date, end_date, u_s_org_local, aor_cor_or_activity_manager,
#'period, funding_type, pepfar_funding
#' @export
#'
#' @examples
#' \dontrun{
#'  df <- clean_ctive_awards("path/to/Active Awards.xlsx", "Active Awards")
#'  }
clean_awards <- function(file, sheetname){


    temp <- readxl::read_xlsx(file,
                              sheet = sheetname,
                              skip = 1) |>
        janitor::clean_names() |>
        dplyr::filter(sector == "IHO") |>
        dplyr::mutate(award_number = stringr::str_trim(award_number),
                      filename = basename(file),
                      period = stringr::str_extract(filename, "^[^_]+"),
                      sub_sector = dplyr::recode(sub_sector, "AFG" = "HTHS",
                                                 "PCMD" = "Family Health",
                                                 "FAMILY HEALTH" = "Family Health"),
                      u_s_org_local = dplyr::recode(u_s_org_local, "International" = "US/Int'l",
                                                    "US" = "US/Int'l")
        ) |>
        dplyr::mutate_if(is.numeric, ~replace_na(., 0)
        ) |>
        dplyr::select(dplyr::any_of(c(sub_sector, activity_name, award_number, total_estimated_cost,
                      start_date, end_date, u_s_org_local, aor_cor_or_activity_manager,
                      period, funding_type, pepfar_funding))) |>
        tidyr::drop_na(award_number)

    return(temp)
}

