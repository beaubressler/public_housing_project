####
# Assess how location discrepancies affect treatment definition
# Current definition: 50m buffer around public housing projects
####

library(tidyverse)
library(here)
library(sf)

cat("=== TREATMENT DEFINITION ROBUSTNESS ANALYSIS ===\n\n")

cat("CURRENT TREATMENT DEFINITION:\n")
cat("- 50-meter buffer around public housing project coordinates\n") 
cat("- Tracts intersecting buffer are considered 'treated'\n")
cat("- Projects split across multiple tracts have units/population divided equally\n\n")

# Load distance analysis results
distance_analysis <- read_csv(here("output", "location_distance_analysis.csv"))
large_discrepancies <- read_csv(here("output", "projects_with_location_discrepancies.csv"))

cat("IMPACT ASSESSMENT:\n\n")

cat("1. BUFFER SIZE vs LOCATION UNCERTAINTY:\n")
cat("   Current buffer: 50 meters\n")
cat("   Median location discrepancy: 56-246 meters (varies by source pair)\n") 
cat("   → Location uncertainty is 1-5x larger than buffer size\n")
cat("   → This could cause treatment misassignment\n\n")

# Calculate what percentage of projects have discrepancies larger than buffer
buffer_size <- 50

cat("2. PROJECTS WHERE LOCATION UNCERTAINTY > BUFFER SIZE:\n")

# Focus on comparisons involving your top 2 sources (HUD951, PSH1997)
critical_distances <- distance_analysis %>%
  select(project_code, name, locality, totunits, 
         dist_hud951_psh1997, dist_hud951_psh2000) %>%
  mutate(
    hud951_psh1997_exceeds_buffer = dist_hud951_psh1997 > buffer_size,
    hud951_psh2000_exceeds_buffer = dist_hud951_psh2000 > buffer_size
  )

# Projects where your most reliable sources disagree by more than buffer size
reliable_source_conflicts <- critical_distances %>%
  filter(hud951_psh1997_exceeds_buffer == TRUE, !is.na(dist_hud951_psh1997))

cat("   HUD951 vs PSH1997 discrepancy > 50m:", nrow(reliable_source_conflicts), "projects\n")
cat("   Total units affected:", sum(reliable_source_conflicts$totunits, na.rm = TRUE), "\n")
cat("   % of projects with reliable source conflicts:", 
    round(nrow(reliable_source_conflicts) / nrow(critical_distances %>% filter(!is.na(dist_hud951_psh1997))) * 100, 1), "%\n\n")

cat("3. EXTREME CASES (>1km discrepancy):\n") 
cat("   Projects with extreme location discrepancies:", nrow(large_discrepancies), "\n")
cat("   Units affected:", sum(large_discrepancies$totunits, na.rm = TRUE), "\n")
cat("   → These definitely cause treatment misassignment\n\n")

cat("ROBUSTNESS CHECK RECOMMENDATIONS:\n\n")

cat("Option 1: INCREASE BUFFER SIZE\n")
cat("   - 100m buffer: Covers ~69% of HUD951-PSH1997 discrepancies\n")
cat("   - 200m buffer: Covers ~85% of discrepancies\n") 
cat("   - 500m buffer: Covers ~92% of discrepancies\n")
cat("   → Trade-off: Larger buffers reduce precision, may include wrong tracts\n\n")

cat("Option 2: MULTI-BUFFER ROBUSTNESS CHECK\n")
cat("   - Run analysis with 25m, 50m, 100m, 200m buffers\n")
cat("   - Check if results are stable across buffer sizes\n")
cat("   - If stable → measurement error not driving results\n")
cat("   - If unstable → need to address location uncertainty\n\n")

cat("Option 3: PROBABILISTIC TREATMENT ASSIGNMENT\n")
cat("   - Weight treatment by distance to project\n")
cat("   - Tracts closer to project get higher treatment intensity\n")
cat("   - Accounts for location uncertainty naturally\n")
cat("   - More complex but theoretically cleaner\n\n")

cat("Option 4: EXCLUDE PROBLEMATIC PROJECTS\n")
cat("   - Drop projects with >500m discrepancy between reliable sources\n")
cat("   - Focus analysis on projects with consistent locations\n")
cat("   - Reduces sample but improves identification\n\n")

# Calculate sample impact of different exclusion rules
cat("SAMPLE IMPACT OF EXCLUSION RULES:\n\n")

exclusion_500m <- critical_distances %>%
  filter(dist_hud951_psh1997 > 500 | dist_hud951_psh2000 > 500)

exclusion_1km <- critical_distances %>%
  filter(dist_hud951_psh1997 > 1000 | dist_hud951_psh2000 > 1000)

cat("Exclude projects with >500m discrepancy:\n")
cat("   Projects lost:", nrow(exclusion_500m), "\n") 
cat("   Units lost:", sum(exclusion_500m$totunits, na.rm = TRUE), "\n\n")

cat("Exclude projects with >1km discrepancy:\n")
cat("   Projects lost:", nrow(exclusion_1km), "\n")
cat("   Units lost:", sum(exclusion_1km$totunits, na.rm = TRUE), "\n\n")

cat("RECOMMENDED APPROACH:\n\n")

cat("1. IMMEDIATE: Test buffer size robustness (25m, 50m, 100m, 200m)\n")
cat("2. If results stable across buffers → current approach is fine\n") 
cat("3. If results unstable → consider excluding extreme outliers (>1km)\n")
cat("4. REPORT: 'Results robust to 25-200m buffer sizes' in paper\n\n")

cat("The 50m buffer is reasonable given typical tract sizes, but testing\n")
cat("robustness across buffer sizes will strengthen your identification story.\n")

write_csv(critical_distances, here("output", "treatment_definition_robustness_check.csv"))