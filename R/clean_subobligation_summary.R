#' Title: Process Subobligation Summary
#'
#' @param SUBOBLIGATION_SUMMARY_PATH Path to the file containing the raw data
#'
#' @return A tibble with the cleaned data
#' @export
#'
#' @examples
#' \dontrun{
#'  df <- clean_subobligation_summary("path/to/subobligation_summary.xlsx")
#'  }
clean_subobligation_summary <- function(SUBOBLIGATION_SUMMARY_PATH) {
    lookup <- c("unliquidated_obligations_beg_fy" = "unliquidated_obligations_at_the_beginning_of_the_fy")

    temp <- readxl::read_xlsx(SUBOBLIGATION_SUMMARY_PATH) |>
        dplyr::mutate(filename = basename(SUBOBLIGATION_SUMMARY_PATH)) |>
        janitor::clean_names() |>
        dplyr::rename_with( ~ lookup[.x], dplyr::any_of(names(lookup))) |>
        dplyr::mutate(
            award_number = stringr::str_trim(award_number),
            period = stringr::str_extract(filename, "^[^_]+")

        ) |>
        dplyr::filter(implementing_mechanism != "Total") |>
        dplyr::select(-c(filename, implementing_mechanism)) |>
        tidyr::drop_na(award_number) |>
        dplyr::rename("program_area_name" = "program_area") |>


        #add program area names
        dplyr::left_join(blingr::data_program_area_name_map, by = "program_area_name") |>

        dplyr::mutate(
            planned_subobligations_for_the_next_months = as.numeric(planned_subobligations_for_the_next_months),
            total_obligations_this_fy = as.numeric(total_obligations_this_fy),
            projected_monthly_burn_rate = as.numeric(projected_monthly_burn_rate),
            un_sub_obligated_funds = as.numeric(un_sub_obligated_funds),
            planned_sub_oblig_date = lubridate::ymd(planned_sub_oblig_date),
            approved_budget_cop_op = as.numeric(approved_budget_cop_op),
            unliquidated_obligations_at_the_beginning_of_the_fy = as.numeric(unliquidated_obligations_at_the_beginning_of_the_fy),
            dplyr::across(dplyr::where(is.numeric), ~ ifelse(is.na(.), NA_real_, .)),

        )  |>
        dplyr::mutate_if(is.numeric, ~ replace_na(., 0)) |>
        dplyr::select(-c(program_area_name))

    temp_comments <- temp |>
        dplyr::select(award_number, period, comments) |>
        tidyr::drop_na(comments) |>
        dplyr::distinct()

    temp <- temp |>
        dplyr::select(-comments) |>
        dplyr::left_join(temp_comments, by = c("award_number", "period"))



    return(temp)
}

