############################################################
#                                                          #
#                      Clean raw data                      #
#                                                          #
############################################################
# Load packages
library(dplyr)
library(forcats)

# Load raw data
raw <- readxl::read_excel('./raw-data/deidentified-data-august-2017.xlsx')

# Quick view
dim(raw)
names(raw)
head(raw)
tail(raw)
glimpse(raw)

# Preliminary clean
df <- raw %>%
    # Remove extra rows at the end of the dataframe
    filter(!is.na(`Patient ID`)) %>%
    # Fix column names
    rename(ID = `Patient ID`,
           visit_number = `Visit number`,
           visit_day = `Time of visit (Days)`,
           age_years = `Age (at time of visit)`,
           mass_kg = `Mass (kgs)`,
           height_m = `Height`,
           sex = Gender,
           hivsn_present = `HIV-SN Development`,
           pain = Pain,
           pain_score = `Pain score`,
           diabetes_record = Diabetes,
           syphillis_record = Syphillis,
           hepatitisB_record = `Hep B`,
           vitaminB12_record = `Vit B12 Def`,
           CD4_cell.ul = `CD4 count (cells/uL)`,
           viral_load_copies.ml = `Viral load (copies/mL)`,
           hba1c_percent = `HbA1c (%)`,
           vitaminB12_pmol.l = `Vit B12 (pmol/L)`,
           consumes_alcohol = `Alcohol consumption`,
           alcohol_units.week = `Converted to Standard units (per week)`,
           TB_current = `Current TB infection`,
           pyridoxine_prophylaxis = `Pyridoxine Prophylaxis (25mg)`,
           rifafour_treatment = `Rifafour Rx`,
           ARV_regimen = `ARV regimen`) %>%
    # Fix mixed class columns to a single format before correcting column class:
    # alcohol_units.week, hba1c_percent, viral_load_copies.ml, CD4_cells.ul, 
    # vitaminB12_pmol.l
    mutate(alcohol_units.week = ifelse(alcohol_units.week == 'Nil',
                                       yes = '0',
                                       no = alcohol_units.week),
           hba1c_percent = ifelse(hba1c_percent == 'R',
                                  yes = NA,
                                  no = hba1c_percent),
           viral_load_copies.ml = ifelse(viral_load_copies.ml == 'R',
                                         yes = NA,
                                         no = ifelse(viral_load_copies.ml == 'LDL',
                                                     yes = '0',
                                                     no = viral_load_copies.ml)),
           CD4_cell.ul = ifelse(CD4_cell.ul == 'R',
                                yes = NA,
                                no = CD4_cell.ul),
           vitaminB12_pmol.l = ifelse(vitaminB12_pmol.l == 'R',
                                      yes = NA,
                                      no = vitaminB12_pmol.l)) %>%
    # Fix column classes
    mutate(visit_number = as.integer(visit_number),
           visit_day = as.integer(visit_day),
           hivsn_present = fct_recode(as_factor(hivsn_present),
                                      yes = '1',
                                      no = '0'),
           pain = fct_recode(as_factor(pain), 
                             yes = '1',
                             no = '0'),
           diabetes_record = fct_recode(as_factor(diabetes_record), 
                                        yes = '1',
                                        no = '0'),
           syphillis_record = fct_recode(as_factor(syphillis_record), 
                                         yes = '1',
                                         no = '0'),
           hepatitisB_record = fct_recode(as_factor(hepatitisB_record), 
                                          yes = '1',
                                          no = '0'),
           vitaminB12_record = fct_recode(as_factor(vitaminB12_record),
                                          no = '0'),
           CD4_cell.ul = as.numeric(CD4_cell.ul),
           viral_load_copies.ml = as.numeric(viral_load_copies.ml),
           hba1c_percent = round(as.numeric(hba1c_percent), 2),
           vitaminB12_pmol.l = as.numeric(vitaminB12_pmol.l),
           consumes_alcohol = fct_recode(as_factor(consumes_alcohol),
                                         yes = '1',
                                         no = '0'),
           alcohol_units.week = as.integer(alcohol_units.week),
           TB_current = fct_recode(as_factor(TB_current),
                                   no = '0',
                                   yes = '1'),
           pyridoxine_prophylaxis = fct_recode(as_factor(pyridoxine_prophylaxis),
                                               no = '0',
                                               yes = '1',
                                               prophylaxis = '2'),
           rifafour_treatment = fct_recode(as_factor(rifafour_treatment),
                                           no = '0',
                                           yes = '1',
                                           prophylaxis = '2'),
           ARV_regimen = fct_recode(as_factor(ARV_regimen),
                                    TDF_FTC_EFV = '1a')) %>%
    # Log raw viral load values, after converting all 0 values to 1
    mutate(viral_load_copies.ml = ifelse(viral_load_copies.ml == 0,
                                         yes = 1,
                                         no = viral_load_copies.ml),
           viral_load_copies.ml = log10(viral_load_copies.ml))

# Add new columns
df <- df %>%
    # Does HBa1c indicate diabetes (> 7%)?
    mutate(diabetic_hba1c = as.factor(ifelse(hba1c_percent > 7,
                                             yes = 'yes',
                                             no = 'no'))) %>%
    # Does the vit B12 values indicate deficiency (< 141 pmol.l)?
    mutate(vitaminB12_deficiency = as.factor(ifelse(vitaminB12_pmol.l < 141,
                                                    yes = 'yes',
                                                    no = 'no'))) %>%
    # Force everyones' visits into a month-by-month sequence
    # Using a 15-day window around each month (i.e., round to nearest month)
    mutate(visit_months = case_when(
        visit_day <= 15 ~ 0,
        visit_day > 15 & visit_day <= 45 ~ 1,
        visit_day > 45 & visit_day <= 75 ~ 2,
        visit_day > 75 & visit_day <= 105 ~ 3,
        visit_day > 105 & visit_day <= 135 ~ 4,
        visit_day > 135 & visit_day <= 165 ~ 5,
        visit_day > 165 & visit_day <= 195 ~ 6,
        visit_day > 195 & visit_day <= 225 ~ 7,
        visit_day > 225 & visit_day <= 255 ~ 8,
        visit_day > 255 & visit_day <= 285 ~ 9
    )) 

# Export data to rds and csv
readr::write_csv(x = df, 
                 path = './data/clean_data.csv')

readr::write_rds(x = df,
                 path = './data/clean_data.rds')

# Clean up
rm(list = ls())