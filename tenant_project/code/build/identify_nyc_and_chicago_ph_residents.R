# The following code links individuals in the 1940 Census to public housing project
# in Chicago and NYC

# Chicago: Address list from Brad Hunt
# NYC: Addresses from NYCHA Address Lists

# Preliminaries -----

library(tidyverse)
library(here)
library(readxl)

# directories
bh_directory <- here("data", "documents", "chicago", "data_from_brad_hunt")

grf_dir_raw <- here("data", "raw", "urban_transitions", "geographic_reference_file")
grf_dir_derived <- here("data", "derived", "geographic_reference_file")

nycha_addresses_dir <- here("tenant_project", "data", "derived", "public_housing_addresses", "working")

output_dir <- here("tenant_project", "data", "derived", "public_housing_residents")

# Chicago --------


# Read in brad Hunt's raw addresses
cha_projects_raw <- read_xlsx(here(bh_directory, "CHA_projects_hand_cleaned.xlsx")) %>% 
  select(-`...8`, -`...9`)

cha_projects <- cha_projects_raw %>%
  filter(!is.na(project_code)) 



# Function to expand an address with a numeric range.
expand_address_range <- function(address) {
  # Define patterns:
  # Pattern for a range such as "670-80 E. 38th Pl.".
  range_pattern <- "^\\s*(\\d+)-(\\d+)(\\s+.*)$"
  # Pattern for a single number address (e.g. "123 Main St.").
  single_pattern <- "^\\s*(\\d+)(\\s+.*)$"
  
  # If the address matches a range...
  if (str_detect(address, range_pattern)) {
    m <- str_match(address, range_pattern)  # m is a matrix with full match and groups
    start_str <- m[2]
    end_str <- m[3]
    remainder <- m[4]
    
    start <- as.integer(start_str)
    # If the ending number is in shorthand (fewer digits than the start)
    if (nchar(end_str) < nchar(start_str)) {
      prefix <- substr(start_str, 1, nchar(start_str) - nchar(end_str))
      end_full <- as.integer(paste0(prefix, end_str))
    } else {
      end_full <- as.integer(end_str)
    }
    # Create a sequence from start to end_full
    seq_numbers <- seq(start, end_full)
    # Return a vector of addresses with each expanded number.
    return(paste0(seq_numbers, remainder))
    
  } else if (str_detect(address, single_pattern)) {
    # Return the address after trimming whitespace if it matches the single number pattern.
    return(trimws(address))
    
  } else {
    # If no matching pattern, simply return the trimmed address.
    return(trimws(address))
  }
}

# Function to process a single row of the dataset.
# It splits the address field on "/" and expands any numeric ranges.
expand_addresses <- function(row) {
  # Split the address field by "/" (this returns a character vector)
  addrs <- str_split(row$address, pattern = "/")[[1]]
  addrs <- trimws(addrs)
  
  # Expand each address component
  expanded_list <- lapply(addrs, expand_address_range)
  # Flatten the list into a vector of address strings
  all_addresses <- unlist(expanded_list)
  
  # For each expanded address, replicate the row, updating the address field.
  new_rows <- lapply(all_addresses, function(addr) {
    new_row <- row
    new_row$address <- addr
    return(new_row)
  })
  
  # Combine the new rows into a data frame.
  return(bind_rows(new_rows))
}

## Clean CHA project addresses -----
# 1. Read in your Excel file (replace "data.xls" with your filename)
chicago_projects <- cha_projects %>% 
  filter(!is.na(address))

# 2. Apply the address expansion to each row.
# We'll loop over rows and combine the results.
expanded_data <- lapply(1:nrow(chicago_projects), function(i) {
  expand_addresses(chicago_projects[i, ])
})

# Combine all expanded rows into one cleaned data frame.
cleaned_cha_projects <- bind_rows(expanded_data)

# remove suffix for join
remove_suffix <- function(address) {
  str_replace(address, regex("\\s+(road|ave|st|pl|pkwy|blvd)$", ignore_case = TRUE), "")
  }

# 3. Clean the project addresses
cleaned_cha_projects_pre_1940 <- 
  cleaned_cha_projects %>% 
  filter(DOFA <= 1940) %>% 
  mutate(address = address %>%
           str_replace_all("E\\.", "E") %>%
           str_replace_all("W\\.", "W") %>%
           str_replace_all("N\\.", "N") %>%
           str_replace_all("S\\.", "S") %>%
           str_replace_all("N\\s+", "N ") %>%
           str_replace_all(regex("\\b[Rr]d\\.?\\b"), "Road") %>%
           str_replace_all(regex("\\b[Aa]ve\\.?\\b"), "Ave") %>%
           str_replace_all(regex("\\b[Ss]t\\.?\\b"), "St") %>% 
           str_replace_all(regex("\\b[Pp]kwy\\.?\\b"), "Pkwy")) %>% 
  # remove "." from the end of the address
  mutate(address = str_replace_all(address, "\\.$", "")) %>%
  mutate(address = str_trim(address)) %>% 
  # remove period and anything after
  mutate(address = str_replace_all(address, "\\..*", "")) %>%
  # remove suffixes
  mutate(address_clean = remove_suffix(address)) %>% 
  select(DOFA, address_clean, project_code, development, area) %>%
  distinct()

# Manual fixes:
# replace roosevelt with W roosevelt
# replace taylor with W taylor
# replace lytle with S lytle
# replace Ada with S ada 
# Replace Racine with S Racine
# Grenshaw with W Grenshaw
# Hoyne with N Hoyne
# Cabrini with W Cabrini
# Diversey with W Diversey
# Leavitt with N Leavitt

cleaned_cha_projects_pre_1940 <- 
  cleaned_cha_projects_pre_1940 %>% 
  mutate(address_clean = case_when(
    str_detect(address_clean, "Roosevelt") & !str_detect(address_clean, "W Roosevelt") ~ str_replace(address_clean, "Roosevelt", "W Roosevelt"),
    str_detect(address_clean, "Taylor") & !str_detect(address_clean, "W Taylor") ~ str_replace(address_clean, "Taylor", "W Taylor"),
    str_detect(address_clean, "Lytle") & !str_detect(address_clean, "S Lytle") ~ str_replace(address_clean, "Lytle", "S Lytle"),
    str_detect(address_clean, "Ada$") & !str_detect(address_clean, "S Ada$") ~ str_replace(address_clean, "Ada", "S Ada"),
    str_detect(address_clean, "Racine$") & !str_detect(address_clean, "S Racine$") ~ str_replace(address_clean, "Racine", "S Racine"),
    str_detect(address_clean, "Grenshaw$") & !str_detect(address_clean, "W Grenshaw$") ~ str_replace(address_clean, "Grenshaw", "W Grenshaw"),
    str_detect(address_clean, "Hoyne$") & !str_detect(address_clean, "N Hoyne$") ~ str_replace(address_clean, "Hoyne", "N Hoyne"),
    str_detect(address_clean, "Cabrini$") & !str_detect(address_clean, "W Cabrini$") ~ str_replace(address_clean, "Cabrini", "W Cabrini"),
    str_detect(address_clean, "Diversey$") & !str_detect(address_clean, "W Diversey$") ~ str_replace(address_clean, "Diversey", "W Diversey"),
    str_detect(address_clean, "Leavitt$") & !str_detect(address_clean, "N Leavitt$") ~ str_replace(address_clean, "Leavitt", "N Leavitt"),
    TRUE ~ address_clean
  )) %>% 
  distinct()



## Read in Chicago cleaned full count addresses ----
grf_1940 <- 
  read_csv(here(grf_dir_derived, "grf_1940_histid_street.csv"))

grf_1940_chicago <-
  grf_1940 %>% 
  filter(b_city == "ChicagoIL")

# drop if street number is missing
chicago_grf_clean <- 
  grf_1940_chicago %>% 
  filter(!is.na(b_hn)) %>% 
  filter(!is.na(b_stfull)) %>%
  mutate(street_and_num = paste(b_hn, b_stfull, sep = " ")) %>% 
  # remove suffixes
  mutate(address_clean = remove_suffix(street_and_num))

## Merge together -----
chicago_ph_residents_1940 <-
  chicago_grf_clean %>% 
  select(histid, address_clean) %>% 
  left_join(cleaned_cha_projects_pre_1940, by = "address_clean") %>% 
  filter(!is.na(DOFA)) %>% 
  select(histid, address_clean, area) %>% 
  dplyr::rename(project = area)


# NYCHA -----
# ny 5-181A	First Houses	1936 (weirdly challenging to link)

# 114, 118, 126, 130, 136, 138, 251, 253, 255 E 3rd St
# 29, 31, 33 

# ny 5-42	Harlem River Houses	1937 (weirdly missing)
# 5-41 Williamsburg Houses 
# ny-5-1	Red Hook	1939 (Doable)
# ny-5-2	Queens-Bridge	1940 (doable, queensbridge)


## NYCHA addresses -----
# Red hook, queensbridge, First Houses, Harlem River Houses, and Williamsburg

nycha_addresses <- 
  read_csv(here(nycha_addresses_dir, "pre_1940_nyc_ph_addresses.csv"))

nycha_addresses_clean <-
  nycha_addresses %>% 
  # remove suffixes: Street, Avenue, Boulevard
  mutate(address_clean = str_remove_all(street_address, "\\s+(Street|Avenue|Boulevard|Road|Place)$")) %>% 
  # manual cleaning
  # replace Ten Eyck with Teneyck
  mutate(address_clean = str_replace_all(address_clean, "Ten Eyck", "Teneyck"),
         # replace Centre Mall with Center mall
         # note: Centre street and center mall in the grf data are, I think, basically the same
         # so 100 center mall should map to 100 centre street in the GRF
         address_clean = str_replace_all(address_clean, "Centre Mall", "Center Mall"),
         #A C Powell Avenue was originally 7th Ave
         address_clean = str_replace_all(address_clean, "A C Powell", "7th"),
         # Replace East with E
         address_clean = str_replace_all(address_clean, "East", "E"),
         # Replace West with W
         address_clean = str_replace_all(address_clean, "West", "W"),
         # Remove Avenue from the address: Avenue A sometimes appear as A Ave
         address_clean = str_replace_all(address_clean, "Avenue ", ""),
         # Replace 3 numbers + letter with the 3 numbers
         address_clean = str_replace_all(address_clean, "\\b([0-9]{3})[a-zA-Z]\\b", "\\1")
         ) %>% 
  # trim whitespace
  mutate(address_clean = str_trim(address_clean)) %>% 
  distinct()



## prep NYC full count addresses -----
grf_1940_nyc <- 
  grf_1940 %>% 
  filter(b_city %in% c("ManhattanNY", "BrooklynNY", "BronxNY", "StatenIslandNY")) %>% 
  # drop if missing address
  filter(!is.na(b_hn), !is.na(b_stfull)) %>%
  mutate(b_hn = as.character(b_hn)) %>%
  mutate(street_and_num = paste(b_hn, b_stfull, sep = " "))


### Queen's -----
# Use alternative version of Queens NY street addresses

grf_file <- paste0("QueensNY_1940.csv")
grf_path <- here(grf_dir_raw, 1940, grf_file)


grf_queens <- read_csv(grf_path, show_col_types = FALSE) 


grf_queens <-
  grf_queens %>% 
  mutate(b_hn = if_else(
    str_detect(m_hn, "^\\d+\\s\\d+$"),
    str_replace(m_hn, " ", "-"),
    m_hn
  ),
  # remove "2nd, 3rd" or anything like that
  b_hn = str_replace_all(b_hn, "\\b\\d+(st|nd|rd|th)\\b", ""),
  # replace / with hyphen
  b_hn = str_replace_all(b_hn, "/", "-"),
  # Extract the first hyphenated number, or plain number if no hyphen exists
  b_hn = str_extract(b_hn, "\\d+-\\d+|\\d+"),
  # if there is a block of 4 numbers and a space, replace it with 2 numbers - 2 numbers
  b_hn = str_replace_all(b_hn, "^(\\d{2})(\\d{2})$", "\\1-\\2")
  ) %>% 
  # drop if missing hn or street
  filter(!is.na(b_hn), !is.na(b_stfull)) %>% 
  mutate(street_and_num = paste(b_hn, b_stfull, sep = " "))

### combine -----
# Combine grfs
grf_nyc_1940_full <- 
  bind_rows(grf_1940_nyc, grf_queens) 

grf_nyc_1940_clean <-
  grf_nyc_1940_full %>% 
  # remove suffixes
  mutate(address_clean = remove_suffix(street_and_num)) 

# manual fixes 
grf_nyc_1940_clean <- 
  grf_nyc_1940_clean %>% 
  # if it ends with centre, replace with center mall
  mutate(address_clean = case_when(
    str_detect(address_clean, "Centre$") ~ str_replace(address_clean, "Centre$", "Center Mall"),
    TRUE ~ address_clean
  ))




## Merge GRF and NYCHA addresses ----
nyc_ph_residents_1940 <- 
  grf_nyc_1940_clean %>% 
  select(histid, address_clean) %>% 
  left_join(nycha_addresses_clean %>% select(DEVELOPMENT, address_clean) %>% distinct(), by = "address_clean") %>% 
  filter(!is.na(DEVELOPMENT)) %>% 
  select(histid, address_clean, DEVELOPMENT) %>%
  dplyr::rename(project = DEVELOPMENT)

# which do i not get:
# 
y <- 
  nycha_addresses_clean %>% 
  left_join(
    grf_nyc_1940_clean %>% select(histid, address_clean), 
    by = "address_clean"
  ) %>%
  filter(is.na(histid)) %>% 
  select(-histid)


# Combine both cities -----
combined_ph_residents_1940 <- 
  bind_rows(chicago_ph_residents_1940,
            nyc_ph_residents_1940) 


# Output ----
write_csv(combined_ph_residents_1940, 
         here(output_dir, "nyc_and_chicago_ph_residents_1940.csv"))



