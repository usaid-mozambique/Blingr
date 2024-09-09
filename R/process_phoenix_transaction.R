#' Process phoenix transaction dataset
#'
#' @param PHOENIX_TRANSACTION_PATH folder path to phoneix transaction datasets
#' @param all_award_number all award numbers of interest
#' @param distribution_filter distriction to be included
#'
#' @return cleaned phoenix transaction dataset
#' @export
#'
#' @examples
#' \dontrun{
#'  df <- process_phoenix_transaction(phoenix_trnsaction_path, award_number,
#'   distribution_filter)
#'  }


process_phoenix_transaction <- function(PHOENIX_TRANSACTION_PATH, all_award_number, distribution_filter){

    temp <- readxl::read_xlsx(PHOENIX_TRANSACTION_PATH,
                              col_types = "text") |>
        janitor::clean_names() |>
        dplyr::select(award_number, document_number, obl_document_number,
               transaction_date, transaction_amt, transaction_event,
               transaction_event_type, program_element, distribution,
               program_area) |>
        dplyr::filter(distribution %in% distribution_filter) |>
        dplyr::mutate(
            transaction_amt = as.numeric(transaction_amt),
            transaction_date = lubridate::as_date(as.numeric(transaction_date) - 1, origin = "1899-12-30"),
            transaction_date = lubridate::floor_date(transaction_date, unit = "quarter"),
            #  count = nrows(PHOENIX_TRANSACTION_PATH),
            program_area = dplyr::case_when(program_element == "A047" ~ "HL.1",
                                     program_element == "A048" ~ "HL.2",
                                     program_element == "A049" ~ "HL.3",
                                     program_element == "A050" ~ "HL.4",
                                     program_element == "A051" ~ "HL.5",
                                     program_element == "A052" ~ "HL.6",
                                     program_element == "A053" ~ "HL.7",
                                     program_element == "A054" ~ "HL.8",
                                     program_element == "A142" ~ "HL.9",
                                     program_element == "A141" ~ "PO.2",
                                     program_element == "A140" ~ "PO.1",
                                     TRUE ~ program_area),

            award_number = dplyr::case_when(
                award_number %in% active_award_number ~ award_number,
                document_number %in% active_award_number ~ document_number,
                TRUE ~ obl_document_number ),
            transaction_disbursement = dplyr::case_when(transaction_event == "DISB" ~ transaction_amt,
                                                 .default = NA_real_),
            transaction_obligation = dplyr::case_when(transaction_event_type == "OBLG_SUBOB" ~ transaction_amt,
                                               transaction_event_type == "OBLG_UNI" ~ transaction_amt,
                                               .default = NA_real_),
            avg_monthly_exp_rate = transaction_disbursement/3
        ) |>
        dplyr::filter(award_number %in% active_award_number) |>
        dplyr::select(-c(program_element, transaction_event_type, transaction_event,
                  document_number, obl_document_number
        )) |>

        #create at month level and add a mutate column with a 1.

        #    temp_2 <- temp |>
        #      select(award_number, transaction_date, transaction_disbursement) |>
        #      drop_na(transaction_disbursement) |>
        #      filter(transaction_disbursement != 0) |>
        #      mutate(month_level = transaction_date %m+% months(3),
        #             count = 1) |>
        #      group_by(award_number, month_level) |>


        dplyr::mutate(fiscal_transaction_date = lubridate::`%m+%`(transaction_date, months(3)),
               period = paste0("FY", lubridate::year(fiscal_transaction_date) %% 100,
                               "Q", lubridate::quarter(fiscal_transaction_date))) |>
        dplyr::group_by(award_number, period, program_area) |>
        dplyr::summarise(dplyr::across(dplyr::where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop") |>

        tidyr::separate(period, into = c("fiscal_year", "quarter"), sep = "Q", convert = TRUE, remove = FALSE) |>
        dplyr::mutate(
            fiscal_year = as.numeric(stringr::str_sub(fiscal_year, 3, 4)) + 2000,
            quarter = as.numeric(quarter)
        ) |>
        dplyr::group_by(award_number, fiscal_year, quarter, period, program_area) |>
        dplyr::summarise(dplyr::across(dplyr::where(is.numeric), ~sum(., na.rm = TRUE)), .groups = "drop")


    return(temp)
}


