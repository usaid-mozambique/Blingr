#' Create a clean dataset from Active Awards google sheet (template from USAID Mozambique)
#'
#' @param ACTIVE_AWARDS_PATH Path to the Active Awards excel sheet
#'
#' @return A clean dataset with the following columns:sub_sector, activity_name, award_number, total_estimated_cost,
#'start_date, end_date, u_s_org_local, aor_cor_or_activity_manager,
#'period, funding_type, pepfar_funding
#' @export
#'
#' @examples
#' \dontrun{
#'  df <- process_active_awards("path/to/Active Awards.xlsx")
#'  }
clean_active_awards <- function(ACTIVE_AWARDS_PATH){


    temp <- readxl::read_xlsx(ACTIVE_AWARDS_PATH,
                              sheet = "Active Awards",
                              skip = 1) |>
        janitor::clean_names() |>
        dplyr::filter(sector == "IHO") |>
        dplyr::mutate(award_number = stringr::str_trim(award_number),
                      filename = basename(ACTIVE_AWARDS_PATH),
                      period = stringr::str_extract(filename, "^[^_]+"),
                      sub_sector = dplyr::recode(sub_sector, "AFG" = "HTHS",
                                                 "PCMD" = "Family Health",
                                                 "FAMILY HEALTH" = "Family Health"),
                      u_s_org_local = dplyr::recode(u_s_org_local, "International" = "US/Int'l",
                                                    "US" = "US/Int'l")
        ) |>
        dplyr::mutate_if(is.numeric, ~replace_na(., 0)
        ) |>
        dplyr::select(sub_sector, activity_name, award_number, total_estimated_cost,
                      start_date, end_date, u_s_org_local, aor_cor_or_activity_manager,
                      period, funding_type, pepfar_funding) |>
        tidyr::drop_na(award_number)

    return(temp)
}
