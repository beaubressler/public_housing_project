library()

full_count_dir <- "data/raw/ipums/full_count/"

ddi <- read_ipums_ddi(paste0(census_raw_data_path,"/usa_00007.xml"))
raw_data <- read_ipums_micro(ddi)
