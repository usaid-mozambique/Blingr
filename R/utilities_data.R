
# New DOAG
vars_new_doag <- c(
    "656-DOAG-656-22-020-DRG",
    "656-DOAG-656-22-019-EDU",
    "656-DOAG-656-22-019-IH",
    "656-DOAG-656-22-021-NUT",
    "656-DOAG-656-22-020-EG",
    "656-DOAG-656-22-021-ENV",
    "656-DOAG-656-22-021-WASH"
)

#shorter versions of program area names
program_area_names <- c(
    "HL.1" = "HIV/AIDS",
    "HL.2" = "TB",
    "HL.3" = "Malaria",
    "HL.4" = "GHS",
    "HL.5" = "Health Systems Strengthening",
    "HL.6" = "MCH",
    "HL.7" = "FP/RH",
    "HL.8" = "WASH",
    "HL.9" = "Nutrition",
    "PO.1" = "PD&L",
    "DR.1" = "Conflict Mitigation and Reconciliation",
    "DR.2" = "Democracy and Governance",
    "DR.3" = "Political Competition and Consensus-Building",
    "DR.4" = "Civil Society",
    "DR.5" = "Peace and Security",
    "DR.6" = "Human Rights"
)

#Rename program elements to new program area IDs
program_element_mapping <- c(
    "A047" ~ "HL.1",
    "A048"~ "HL.2",
    "A049"~ "HL.3",
    "A050"~ "HL.4",
    "A051"~ "HL.5",
    "A052"~ "HL.6",
    "A053"~ "HL.7",
    "A054"~ "HL.8",
    "A142"~ "HL.9",
    "A141"~ "PO.2",
    "A140"~ "PO.1"
)

