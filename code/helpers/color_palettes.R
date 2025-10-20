# Color Palettes for Event Study Plots
# Using Okabe-Ito colorblind-friendly palette

# Okabe-Ito palette (8 colors, all distinguishable by colorblind individuals)
okabe_ito <- c(
  orange = "#E69F00",
  sky_blue = "#56B4E9",
  bluish_green = "#009E73",
  yellow = "#F0E442",
  dark_blue = "#0072B2",
  vermillion = "#D55E00",
  reddish_purple = "#CC79A7",
  black = "#000000"
)

# Main treatment group colors (for event studies)
treatment_colors <- c(
  treated = okabe_ito[["orange"]],
  control = okabe_ito[["sky_blue"]],
  spillover = okabe_ito[["bluish_green"]]
)

# Time series colors
time_series_colors <- c(
  "Treated Tracts" = okabe_ito[["orange"]],
  "Nearby neighborhoods" = okabe_ito[["bluish_green"]],
  "Matched Controls" = okabe_ito[["sky_blue"]]
)

# Outcome overlay colors (for multiple outcomes on same plot)
outcome_colors <- c(
  okabe_ito[["orange"]],      # First outcome
  okabe_ito[["sky_blue"]],    # Second outcome
  okabe_ito[["bluish_green"]] # Third outcome
)

outcome_colors_4 <- c(
  okabe_ito[["orange"]],
  okabe_ito[["sky_blue"]],
  okabe_ito[["bluish_green"]],
  okabe_ito[["yellow"]]
)

outcome_colors_5 <- c(
  okabe_ito[["orange"]],
  okabe_ito[["sky_blue"]],
  okabe_ito[["bluish_green"]],
  okabe_ito[["yellow"]],
  okabe_ito[["dark_blue"]]
)

# Heterogeneity-specific palettes

# Project size (3 groups: small, medium, large)
project_size_colors <- c(
  okabe_ito[["sky_blue"]],    # Small
  okabe_ito[["bluish_green"]], # Medium
  okabe_ito[["orange"]]        # Large
)

# Tipping analysis (3 groups: very low, medium, high black share)
# Warm gradient for racial composition
tipping_colors <- c(
  okabe_ito[["yellow"]],      # Very low
  okabe_ito[["orange"]],      # Medium
  okabe_ito[["vermillion"]]   # High
)

# Income level (2 groups: low, high)
income_colors <- c(
  okabe_ito[["sky_blue"]],    # Low income
  okabe_ito[["dark_blue"]]    # High income
)

# NYC vs Non-NYC (2 groups)
nyc_colors <- c(
  okabe_ito[["orange"]],      # NYC
  okabe_ito[["sky_blue"]]     # Non-NYC
)

# Urban renewal status (2 groups)
urban_renewal_colors <- c(
  okabe_ito[["vermillion"]],  # Urban renewal
  okabe_ito[["bluish_green"]] # Non-urban renewal
)

# High-rise status (2 groups)
highrise_colors <- c(
  okabe_ito[["vermillion"]],  # High-rise
  okabe_ito[["dark_blue"]]    # Non-high-rise
)

# Early vs late projects (2 groups)
timing_colors <- c(
  okabe_ito[["bluish_green"]], # Early
  okabe_ito[["orange"]]        # Late
)
