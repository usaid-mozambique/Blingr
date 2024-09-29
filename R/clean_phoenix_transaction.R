#' Process phoenix transaction dataset
#'
#' @param distribution_filter distriction to be included
#' @param file path to the file containing the raw data
#' @param all_award_number all relevant award numbers
#'
#' @return cleaned phoenix transaction dataset
#' @export
#'
#' @examples
#' \dontrun{
#'  df <- clean_phoenix_transaction(phoenix_transaction_path, award_number,
#'   distribution_filter)
#'  }


clean_phoenix_transaction <- function(file,
                                      all_award_number,
                                      distribution_filter){

    temp <- readxl::read_xlsx(file, col_types = "text") |>
        janitor::clean_names() |>
        dplyr::select(
            obl_document_number,
            transaction_date,
            transaction_amt,
            transaction_event,
            transaction_event_type,
            program_element,
            distribution,
            program_area
        ) |>
        dplyr::filter(distribution %in% distribution_filter,
                      obl_document_number %in% all_award_number) |>
        dplyr::mutate(
            transaction_amt = as.numeric(transaction_amt),
            transaction_date = lubridate::as_date(as.numeric(transaction_date) - 1, origin = "1899-12-30"),
            transaction_date_quarter = lubridate::floor_date(transaction_date, "quarter"),
            transaction_date_month = lubridate::floor_date(transaction_date, "month"),
            transaction_disbursement = dplyr::case_when(transaction_event == "DISB" ~ transaction_amt, .default = NA_real_),
            transaction_obligation = dplyr::case_when(
                transaction_event_type == "OBLG_SUBOB" ~ transaction_amt,
                transaction_event_type == "OBLG_UNI" ~ transaction_amt,
                .default = NA_real_
            ),
            #    avg_monthly_exp_rate = transaction_disbursement / 3  # get monthly average
        ) |>
        #rename old program_areas to match the new
        dplyr::left_join(
            blingr::data_program_element_map,
            by = c("program_element" = "old_program_element")
        ) |>
        dplyr::mutate(program_area = dplyr::if_else(is.na(new_program_area), program_area, new_program_area)) |>
        dplyr::select(-new_program_area) |>

        #add program area names
        dplyr::left_join(blingr::data_program_area_name_map, by = "program_area") |>
        dplyr::select(
            -c(
                program_element,
                transaction_event_type,
                transaction_event
            )
        ) |>

        dplyr::mutate(
            fiscal_transaction_date = lubridate::`%m+%`(transaction_date_quarter, months(3)),
            period = paste0(
                "FY",
                lubridate::year(fiscal_transaction_date) %% 100,
                "Q",
                lubridate::quarter(fiscal_transaction_date)
            )
        ) |>
        dplyr::rename(award_number = obl_document_number) |>
        dplyr::group_by(award_number, period, program_area, program_area_name, transaction_date_month) |>
        dplyr::summarise(dplyr::across(dplyr::where(is.numeric), ~ sum(., na.rm = TRUE)), .groups = "drop") |>

        tidyr::separate(
            period,
            into = c("fiscal_year", "fiscal_quarter"),
            sep = "Q",
            convert = TRUE,
            remove = FALSE
        ) |>
        dplyr::mutate(fiscal_year = as.numeric(stringr::str_sub(fiscal_year, 3, 4)) + 2000,
                      fiscal_quarter = as.numeric(fiscal_quarter)) |>
        dplyr::group_by(award_number,
                        transaction_date_month,
                        fiscal_year,
                        fiscal_quarter,
                        period,
                        program_area,
                        program_area_name) |>
        dplyr::summarise(dplyr::across(dplyr::where(is.numeric), ~ sum(., na.rm = TRUE)), .groups = "drop")


    return(temp)
}
