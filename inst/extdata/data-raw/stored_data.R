data_program_area_name_map <-

tibble::tribble(
    ~program_area,                             ~program_area_name,
           "HL.1",                                     "HIV/AIDS",
           "HL.2",                                           "TB",
           "HL.3",                                      "Malaria",
           "HL.4",                                          "GHS",
           "HL.5",                  "Health Systems Strenghening",
           "HL.6",                                          "MCH",
           "HL.7",                                        "FP/RH",
           "HL.8",                                         "WASH",
           "HL.9",                                    "Nutrition",
           "PO.1",                                         "PD&L",
           "DR.1",       "Conflict Mitigation and Reconciliation",
           "DR.2",                     "Democracy and Governance",
           "DR.3", "Political Competition and Consensus-Building",
           "DR.4",                                "Civil Society",
           "DR.5",                           "Peace and Security",
           "DR.6",                                 "Human Rights",
           "PO.2",                                          "A&O"
    )
usethis::use_data(data_program_area_name_map, overwrite = TRUE)

data_program_element_map <-
tibble::tribble(
    ~old_program_element, ~new_program_area,
                  "A047",            "HL.1",
                  "A048",            "HL.2",
                  "A049",            "HL.3",
                  "A050",            "HL.4",
                  "A051",            "HL.5",
                  "A052",            "HL.6",
                  "A053",            "HL.7",
                  "A054",            "HL.8",
                  "A142",            "HL.9",
                  "A141",            "PO.2",
                  "A140",            "PO.1"
    )

usethis::use_data(data_program_element_map, overwrite = TRUE)
