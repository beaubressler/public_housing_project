### 
# Takes the 1977 Picture of Subsidized Household data provided to me by Yana Kucheva 
# and produces a dataset with the variables I want:
# For each project, I want 
# 1. Total population
# 2. Total population by race
###

# Preliminaries ----
library(tidyverse)
library(haven)

# Set file paths
raw_data_dir <- "data/raw/pic77/pic77_kucheva/"
int_data_dir <- "data/derived/public_housing/intermediate/hud/"

# Read the Stata file
raw_data <- read_dta(file.path(raw_data_dir, "pic77.dta"))


# Data cleaning -----
# Label variables
data <- raw_data %>%
  # select relevant variables
  select(LSTATE, LPROJ1, LPROJ2, LPROJ3, LOCPAT, LSTAGE, LDESIN, TSTATE, LUNTTO,
         LWHTTO, LBLCTO, LINDTO, LHSPTO, LASITO, LMINTO,
         LYEAR, LPLACE, LCD, LSMSA, LMETRO, LPROJC, LPOP, LNONDW, LUNTMD, LUNTDE,
         LUNTOT, LPRACC, LUMGMT, LUNTEL, LWHTEL, LBLCEL, 
         LINDEL, LHSPEL, LASIEL, LMINEL, LUNTED, LWHTED, LBLCED, LINDED, LHSPED,
         LASIED, LMINED, LUNTVA, LUNTAV, 
         TPLACE, TELDI, TNELDI, TELDIR, TNELDR, TREP, TINCMD, TINCMN, 
         TINCSD, TINCLO, TINCHI, TPERMD, TPERMN, TPERSD, TC20MD, TC20MN, TELDER,
         TELDIH, T1C2A, TFSBW, THSBW, TRENMD, TRENMN, TRENSD, TRENLO, TRENHI, 
         TRIMD, TRIMN, TRISD, TRILO, TRIHI, TRI35, TRIAMD, 
         TRIAMN, TRIASD, TRIALD, TRIAHI, TRIA35, TAGEMD, TAGEMN, TAGESD, TWRKMN,
         TWRKSD, TWHIT, TBLAC, THISP, TINDI, TASIA, 
         TMNRTY) %>% 
mutate(
    LYEAR = labelled(LYEAR, label = "Reporting Period: Last 2 digits of year"),
    LPLACE = labelled(LPLACE, label = "HUD Locality Code"),
    LCD = labelled(LCD, label = "Congressional District (from 1970s)"),
    LSTAGE = labelled(LSTAGE, label = "Project Stage"),
    LDESIN = labelled(LDESIN, label = "Initial Project Designation (Elderly)"),
    LSMSA = labelled(LSMSA, label = "SMSA Code"),
    LMETRO = labelled(LMETRO, label = "Center City Status"),
    LPROJC = labelled(LPROJC, label = "Project Code"),
    LPOP = labelled(LPOP, label = "Locality Population (1970 Census)"),
    LNONDW = labelled(LNONDW, label = "Units For Non-Dwelling purposes"),
    LUNTMD = labelled(LUNTMD, label = "Units For Modernization"),
    LUNTDE = labelled(LUNTDE, label = "Units For Demolition"),
    LUNTOT = labelled(LUNTOT, label = "Other units (boarded up, etc.)"),
    LPRACC = labelled(LPRACC, label = "Units Programmed-ACC"),
    LUMGMT = labelled(LUMGMT, label = "Units Under Management"),
    LUNTEL = labelled(LUNTEL, label = "Total units leased to employees & subsidized tenants"),
    LWHTEL = labelled(LWHTEL, label = "Total units leased - White non-minority"),
    LBLCEL = labelled(LBLCEL, label = "Total units leased - Black"),
    LINDEL = labelled(LINDEL, label = "Total units leased - American Indian"),
    LHSPEL = labelled(LHSPEL, label = "Total units leased - Hispanic"),
    LASIEL = labelled(LASIEL, label = "Total units leased - Oriental"),
    LMINEL = labelled(LMINEL, label = "Total units leased - other Minority"),
    LUNTTO = labelled(LUNTTO, label = "Total subsidized tenants (households)"),
    LWHTTO = labelled(LWHTTO, label = "# of subsidized households, White non-minority"),
    LBLCTO = labelled(LBLCTO, label = "# of subsidized households, Black"),
    LINDTO = labelled(LINDTO, label = "# of subsidized households, American Indian"),
    LHSPTO = labelled(LHSPTO, label = "# of subsidized households, Hispanic"),
    LASITO = labelled(LASITO, label = "# of subsidized households, Oriental"),
    LMINTO = labelled(LMINTO, label = "# of subsidized households, Other Minority"),
    LUNTED = labelled(LUNTED, label = "Total subsidized households where head or spouse is age 62+ or has disability"),
    LWHTED = labelled(LWHTED, label = "# of White non-minority subsidized households where head or spouse is age 62+ or has disability"),
    LBLCED = labelled(LBLCED, label = "# of Black subsidized households where head or spouse is age 62+ or has disability"),
    LINDED = labelled(LINDED, label = "# of American Indian subsidized households where head or spouse is age 62+ or has disability"),
    LHSPED = labelled(LHSPED, label = "# of Hispanic subsidized households where head or spouse is age 62+ or has disability"),
    LASIED = labelled(LASIED, label = "# of Oriental subsidized households where head or spouse is age 62+ or has disability"),
    LMINED = labelled(LMINED, label = "# of other minority subsidized households where head or spouse is age 62+ or has disability"),
    LUNTVA = labelled(LUNTVA, label = "Total units vacant"),
    LUNTAV = labelled(LUNTAV, label = "Units Available for Occupancy"),
    TSTATE = labelled(TSTATE, label = "State Code"),
    TPLACE = labelled(TPLACE, label = "Locality"),
    TELDI = labelled(TELDI, label = "# of Households Moving In who are Elderly, Handicapped, or Disabled (Unweighted)"),
    TNELDI = labelled(TNELDI, label = "# of other Households Moving In (Unweighted)"),
    TELDIR = labelled(TELDIR, label = "# of Households Being Re-Examined Who are Elderly, handicapped, or Disabled (Unweighted)"),
    TNELDR = labelled(TNELDR, label = "# of other Households Being Re-Examined (Unweighted)"),
    TREP = labelled(TREP, label = "Percent reported"),
    TINCMD = labelled(TINCMD, label = "One tenth of Median Gross Income"),
    TINCMN = labelled(TINCMN, label = "One tenth of Mean Gross Income"),
    TINCSD = labelled(TINCSD, label = "One tenth of Standard Deviation of Gross Income"),
    TINCLO = labelled(TINCLO, label = "Households Below $4,000 Gross Income as % of All Households with Gross Income Reported"),
    TINCHI = labelled(TINCHI, label = "Households At or Above $8,000 Gross Income as % of All Households with Gross Reported"),
    TPERMD = labelled(TPERMD, label = "Median Household Size"),
    TPERMN = labelled(TPERMN, label = "Mean Household Size"),
    TPERSD = labelled(TPERSD, label = "Standard Deviation of Household Size"),
    TC20MD = labelled(TC20MD, label = "Median Number of Minors"),
    TC20MN = labelled(TC20MN, label = "Mean Number of Minors"),
    TELDER = labelled(TELDER, label = "Elderly Households as % of All Households with Elderly Status Reported"),
    TELDIH = labelled(TELDIH, label = "Elderly, Handicapped & Disabled Households as % of All Households with these Statuses Reported"),
    TFSBW = labelled(TFSBW, label = "Female-headed Households as % of All Households with Sex, Size, & Number of Minors Reported"),
    THSBW = labelled(THSBW, label = "Households with Husband & Wife Present as % of All Households with Status Reported"),
    TRENMD = labelled(TRENMD, label = "Median Gross Rent per month"),
    TRENMN = labelled(TRENMN, label = "Mean Gross Rent per month"),
    TRENSD = labelled(TRENSD, label = "Standard Deviation of Gross Rent"),
    TRENLO = labelled(TRENLO, label = "Households Below $50 Gross Rent as % of All Households with Gross Rent Reported"),
    TRENHI = labelled(TRENHI, label = "Households At or Above $150 Gross Rent as % of All Households with Gross Rent Reported"),
    TRIMD = labelled(TRIMD, label = "Median Ratio of Gross Annual Rent to Gross Annual Income"),
    TRIMN = labelled(TRIMN, label = "Mean Ratio of Gross Annual Rent to Gross Annual Income"),
    TRISD = labelled(TRISD, label = "Standard Deviation of Ratio of Gross Annual Rent to Gross Annual Income"),
    TRILO = labelled(TRILO, label = "Households with Rent/Gross Income Ratio under 20%, as % of All Households with Rent & Income Reported"),
    TRIHI = labelled(TRIHI, label = "Households with Rent/Gross Income Ratio At or Above 25% as % of All Households with Rent & Income Reported"),
    TRI35 = labelled(TRI35, label = "Households with Rent/Gross Income Ratio at or over 35% as % of All Households with Rent & Income Reported"),
    TRIAMD = labelled(TRIAMD, label = "Median Annual Rent-to-Adj. Income Ratio"),
    TRIAMN = labelled(TRIAMN, label = "Mean Annual Rent-to-Adj. Income Ratio"),
    TRIASD = labelled(TRIASD, label = "Standard Deviation Annual Rent-to-Adj. Income Ratio"),
    TRIALD = labelled(TRIALD, label = "Households with Rent/Adj. Income Ratio Under 20% of All Households with Rent & Income Reported"),
    TRIAHI = labelled(TRIAHI, label = "Households with Rent/Adj. Income Ratio Greater than 25% of All familes with rent & Income Reported"),
    TRIA35 = labelled(TRIA35, label = "Households with Rent/Adj. Income Ratio Greater than 35% of All Households with Rent & Income Reported"),
    TAGEMD = labelled(TAGEMD, label = "Median Age of Household Head"),
    TAGEMN = labelled(TAGEMN, label = "Mean Age of Household Head"),
    TAGESD = labelled(TAGESD, label = "Standard Deviation of Age of Household Head"),
    TWRKMN = labelled(TWRKMN, label = "Mean Number of Workers"),
    TWRKSD = labelled(TWRKSD, label = "Standard Deviation of Number of Workers"),
    TWHIT = labelled(TWHIT, label = "White non-minority as % of Households with Race Reported"),
    TBLAC = labelled(TBLAC, label = "Blacks as % of Households with Race Reported"),
    THISP = labelled(THISP, label = "Hispanics as % of Households with Race Reported"),
    TINDI = labelled(TINDI, label = "American Indians as % of Households with Race Reported"),
    TASIA = labelled(TASIA, label = "Orientals as % of Households with Race Reported"),
    TMNRTY = labelled(TMNRTY, label = "Other Minorities as % of Households with Race Reported"))

                      
                      
# Create LOCPAT_code variable
data <- data %>%
  mutate(
    LOCPAT_code = case_when(
      LOCPAT == "A1" ~ 1,
      LOCPAT == "A2" ~ 2,
      LOCPAT == "A3" ~ 3,
      LOCPAT == "A4" ~ 4,
      LOCPAT == "A5" ~ 5,
      LOCPAT == "A6" ~ 6,
      LOCPAT == "B0" ~ 7,
      LOCPAT == "BO" ~ 7, # typo in data
      LOCPAT == "B1" ~ 8,
      LOCPAT == "C1" ~ 9,
      LOCPAT == "C2" ~ 10,
      LOCPAT == "C3" ~ 11,
      LOCPAT == "C4" ~ 12,
      LOCPAT == "C5" ~ 13,
      LOCPAT == "C6" ~ 14,
      LOCPAT == "C7" ~ 15,
      LOCPAT == "D1" ~ 16,
      LOCPAT == "D2" ~ 17,
      LOCPAT == "99" ~ 18,
      TRUE ~ NA_real_
    )
  )

# Label LOCPAT_code
LOCPAT_code_labels <- c(
  "All White non-minority", "All Black", "All American Indians",
  "All Hispanics", "All Oriental", "All Other Minorities",
  "Multi-Site", "Other Limitations", "White non-minority & Black",
  "White non-minority & American Indian", "White & Hispanic",
  "White non-minority & Oriental", "White non-minority & Other Minorities",
  "White non-minority, Black & Hispanic only",
  "White non-minority, & any other combination not indicated above",
  "No White non-minority & Some Black", "No White non-minority & No Black", "N/A"
)

data$LOCPAT_code <- labelled(
  data$LOCPAT_code,
  label = "Occupancy Pattern Code",
  labels = setNames(1:18, LOCPAT_code_labels)
)

# LOCPAT label variable
data <-
  data %>% 
  mutate(LOCPAT_label = case_when(
    LOCPAT == "A1" ~ "All White non-minority",
    LOCPAT == "A2" ~ "All Black",
    LOCPAT == "A3" ~ "All American Indians",
    LOCPAT == "A4" ~ "All Hispanics",
    LOCPAT == "A5" ~ "All Oriental",
    LOCPAT == "A6" ~ "All Other Minorities",
    LOCPAT == "B0" ~ "Multi-Site",
    LOCPAT == "BO" ~ "Multi-Site", # typo in data
    LOCPAT == "B1" ~ "Other Limitations",
    LOCPAT == "C1" ~ "White non-minority & Black",
    LOCPAT == "C2" ~ "White non-minority & American Indian",
    LOCPAT == "C3" ~ "White & Hispanic",
    LOCPAT == "C4" ~ "White non-minority & Oriental",
    LOCPAT == "C5" ~ "White non-minority & Other Minorities",
    LOCPAT == "C6" ~ "White non-minority, Black & Hispanic only",
    LOCPAT == "C7" ~ "White non-minority, & any other combination not indicated above",
    LOCPAT == "D1" ~ "No White non-minority & Some Black",
    LOCPAT == "D2" ~ "No White non-minority & No Black",
    LOCPAT == "99" ~ "N/A",
    TRUE ~ NA_character_
  ))


# Label LDESIN
data$LDESIN <- labelled(
  data$LDESIN,
  label = "Initial Project Designation (Elderly)",
  labels = c("All Elderly" = 0, "Some Elderly" = 1, "No Elderly" = 2, "N/A" = 9)
)

# create LDESIN label variable
data <-
  data %>% 
  mutate(LDESIN_label = case_when(
    LDESIN == 0 ~ "All Elderly",
    LDESIN == 1 ~ "Some Elderly",
    LDESIN == 2 ~ "No Elderly",
    LDESIN == 9 ~ "N/A",
    TRUE ~ NA_character_
  ))

# Data edits ----

data <- 
  data %>% 
  # create project codes
  mutate(project_code = paste(LSTATE, LPROJ1, LPROJ2, sep ="-"),
          # create full project code, includes LPROJ3 if it exists
          project_code_full = ifelse(LPROJ3 == "", project_code, paste(project_code, LPROJ3, sep = "-")))

# If value of any variable = 9999, set as NA
data[data == 9999] <- NA




# estimate population size based on mean household size (TPERMN) and number of households (LUNTTO)
# can also do this for black and white population (assuming that mean household size is the same)
data <-
  data %>% 
  # if value of TPERMN (average household size) is approx 99.99, or less than 0, set to missing
  mutate(TPERMN = ifelse(round(TPERMN, 2) == 99.99, NA, TPERMN),
         TPERMN = ifelse(TPERMN < 0, NA, TPERMN)) 


## 10/14/2025: Impute missing household size based on LOCPAT (occupancy pattern code aka race) and LDESIN (elderly designation)
# These two explain a lot of the variation in household size, and give me population estimates

# create simplified LOCPAT categotries
data <- data %>%
  mutate(
    locpat_simple = case_when(
      LOCPAT == "A1" ~ "All White",
      LOCPAT == "A2" ~ "All Black",
      LOCPAT %in% c("A3", "A4", "A5", "A6") ~ "All Other Race",
      LOCPAT %in% c("C1", "C2", "C3", "C4", "C5", "C6", "C7") ~ "Mixed",
      LOCPAT %in% c("D1", "D2") ~ "No White",
      TRUE ~ "Other"
    )
  )

#3. Calculate mean household size for each LDESIN × LOCPAT cell:
imputation_means <- data %>%
  filter(!is.na(TPERMN)) %>%
  group_by(LDESIN_label, locpat_simple) %>%
  summarise(
    mean_hhsize_impute = mean(TPERMN),
    .groups = "drop"
  )

  
# 4. Merge the means back and impute:
data <- data %>%
  left_join(
    imputation_means,
    by = c("LDESIN_label", "locpat_simple")
  ) %>%
  mutate(
    tpermn_imputed = is.na(TPERMN) & !is.na(mean_hhsize_impute),
    TPERMN = ifelse(is.na(TPERMN), mean_hhsize_impute, TPERMN)
  ) 
  

data <- data  %>%
  mutate(
    proj_total_population_estimate = round(TPERMN * LUNTTO, 0),
    proj_white_population_estimate = round(TPERMN * LWHTTO, 0),
    proj_black_population_estimate = round(TPERMN * LBLCTO, 0),
  )  %>% 
  # calculate the percentage of households that is white or black
  mutate(
    proj_white_share = LWHTTO / LUNTTO,
    proj_black_share = LBLCTO / LUNTTO
  ) %>% 
  dplyr::rename(average_household_size = TPERMN,
                proj_total_households = LUNTTO,
                proj_white_households = LWHTTO,
                proj_black_households = LBLCTO,
                white_share = proj_white_share,
                black_share = proj_black_share)


##collapse data 
# collapsed_data <- 
#   data %>% 
#   group_by(project_code) %>% 
#   summarise(proj_total_population_estimate = sum(proj_total_population_estimate, na.rm = TRUE),
#             proj_white_population_estimate = sum(proj_white_population_estimate, na.rm = TRUE),
#             proj_black_population_estimate = sum(proj_black_population_estimate, na.rm = TRUE),
#             total_households = sum(LUNTTO, na.rm = TRUE),
#             LWHTTO = sum(LWHTTO, na.rm = TRUE),
#             LBLCTO = sum(LBLCTO, na.rm = TRUE)) %>% 
#   mutate(
#     proj_white_share = LWHTTO / LUNTTO,
#     proj_black_share = LBLCTO / LUNTTO
#   ) %>% 
#   dplyr::rename(total_households = LUNTTO,
#                 white_households = LWHTTO,
#                 black_households = LBLCTO)
            
            


### Output data ----
# output 2 dataset: 
# 1. The full cleaned dataset
# 2. The dataset with only project codes, and proj variables

# 1. Full cleaned dataset
write_dta(data, paste0(int_data_dir, "pic77_data_cleaned_full.dta"))

# 2. Dataset with only the variables needed for the analysis
# project code, population by race and race shares, occupancy pattern, and elderly designation
data_output <- data %>%
  select(LSTATE, contains("project_code"), contains("proj_"), average_household_size,
         black_share, white_share,
         LOCPAT_code, LOCPAT_label, LDESIN, LDESIN_label,
         tpermn_imputed, locpat_simple, mean_hhsize_impute)

write_dta(data_output, paste0(int_data_dir, "pic77_data_for_analysis.dta"))


# ============================================================================
# IMPUTATION VALIDATION (Hold-out test)
# ============================================================================

cat("\n=== HOUSEHOLD SIZE IMPUTATION VALIDATION ===\n\n")

# Hold-out validation: Test imputation quality on 20% of observed data
set.seed(123)
validation_data <- data %>%
  filter(!is.na(average_household_size), !tpermn_imputed) %>%
  mutate(
    holdout = sample(c(TRUE, FALSE), n(), replace = TRUE, prob = c(0.2, 0.8)),
    hhsize_actual = average_household_size,
    hhsize_test = ifelse(holdout, NA, average_household_size)
  )

# Recalculate means WITHOUT holdout observations
validation_means <- validation_data %>%
  filter(!holdout) %>%
  group_by(LDESIN_label, locpat_simple) %>%
  summarise(mean_hhsize_validation = mean(hhsize_test, na.rm = TRUE), .groups = "drop")

# Impute holdout values
validation_data <- validation_data %>%
  left_join(validation_means, by = c("LDESIN_label", "locpat_simple")) %>%
  mutate(hhsize_imputed = ifelse(holdout, mean_hhsize_validation, hhsize_test))

# Calculate errors for holdout sample
holdout_results <- validation_data %>%
  filter(holdout) %>%
  mutate(
    error = hhsize_imputed - hhsize_actual,
    abs_error = abs(error),
    pct_error = abs_error / hhsize_actual * 100
  )

# Summary statistics
cat("HOLDOUT VALIDATION RESULTS (n =", nrow(holdout_results), "):\n\n")
cat("Error distribution:\n")
cat("  Mean error:", round(mean(holdout_results$error, na.rm = TRUE), 3), "\n")
cat("  Median error:", round(median(holdout_results$error, na.rm = TRUE), 3), "\n\n")

cat("Absolute error:\n")
cat("  Mean:", round(mean(holdout_results$abs_error, na.rm = TRUE), 3), "\n")
cat("  Median:", round(median(holdout_results$abs_error, na.rm = TRUE), 3), "\n\n")

cat("Percent error:\n")
cat("  Mean:", round(mean(holdout_results$pct_error, na.rm = TRUE), 1), "%\n")
cat("  Median:", round(median(holdout_results$pct_error, na.rm = TRUE), 1), "%\n\n")

# R²
r_squared <- cor(holdout_results$hhsize_actual,
                 holdout_results$hhsize_imputed,
                 use = "complete.obs")^2
cat("R² (imputed vs actual):", round(r_squared, 3), "\n\n")

cat("Interpretation:\n")
cat("  - Mean error near 0 indicates unbiased imputation\n")
cat("  - Median % error shows typical prediction accuracy\n")
cat("  - R² shows proportion of variance explained\n\n")

# Coverage summary
cat("=== COVERAGE SUMMARY ===\n\n")
cat("Projects with households (total > 0):", sum(data$proj_total_households > 0, na.rm = TRUE), "\n")
cat("  - With observed household size:", sum(!data$tpermn_imputed & data$proj_total_households > 0, na.rm = TRUE),
    sprintf("(%.1f%%)\n", 100 * mean(!data$tpermn_imputed[data$proj_total_households > 0])))
cat("  - With imputed household size:", sum(data$tpermn_imputed & data$proj_total_households > 0, na.rm = TRUE),
    sprintf("(%.1f%%)\n", 100 * mean(data$tpermn_imputed[data$proj_total_households > 0], na.rm = TRUE)))
cat("  - With population estimates:", sum(!is.na(data$proj_total_population_estimate) & data$proj_total_households > 0, na.rm = TRUE),
    sprintf("(%.1f%%)\n\n", 100 * mean(!is.na(data$proj_total_population_estimate[data$proj_total_households > 0]))))

cat("=== VALIDATION COMPLETE ===\n\n")
