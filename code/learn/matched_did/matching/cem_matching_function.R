# CEM matching function with anti-CBSA and optimal 1:1 matching using LSAP

perform_cem_with_anti_cbsa_nn <- function(
  data,
  treatment_year,
  match_vars,
  group_type = "treated",
  pre_treatment_decades = 2,
  time_invariant_vars = c("asinh_distance_from_cbd"),
  exact_match_vars = NULL,
  cem_cutpoints = NULL,
  cem_grouping = NULL,
  nn_distance = "mahalanobis",
  n_bins = 3,
  ridge = 1e-6
) {

  # ---------- 1) Pre-treatment years ----------
  pre_years <- if (pre_treatment_decades == 1) {
    treatment_year - 10
  } else {
    c(treatment_year - 10, treatment_year - 20)
  }

  # ---------- 2) Filter to treated-at-t and donor_pool at pre-years ----------
  matching_data <- data %>%
    filter(
      (location_type == group_type & treatment_year == !!treatment_year) |
      (location_type == "donor_pool" & year %in% pre_years)
    ) %>%
    mutate(category_most_overlap = as.character(category_most_overlap))

  # ---------- 3) Wide panel of pre-treatment covariates ----------
  tv <- setdiff(match_vars, time_invariant_vars)
  wide <- matching_data %>%
    select(GISJOIN_1950, city, cbd, cbsa_title, county_id,
           redlined_binary_80pp, ur_binary_5pp, category_most_overlap,
           location_type, year, all_of(c(tv, time_invariant_vars))) %>%
    pivot_wider(
      names_from = year,
      values_from = all_of(tv),
      names_glue = "year_{year}_{.value}"
    )

  # Variables actually used in matching
  match_cols <- c(
    as.vector(outer(pre_years, tv, function(y, v) paste0("year_", y, "_", v))),
    time_invariant_vars,
    exact_match_vars %||% character()
  )

  # Complete cases only on matching vars
  wide <- wide %>%
    filter(complete.cases(across(all_of(match_cols)))) %>%
    mutate(treatment_group = as.integer(location_type == group_type))

  # ---------- 4) CEM ----------
  # Auto-coarsen numeric vars by quantiles if not specified
  if (is.null(cem_cutpoints)) {
    numeric_vars <- match_cols[map_lgl(match_cols, ~ is.numeric(wide[[.x]]))]
    probs <- seq(0, 1, length.out = n_bins + 1)[2:n_bins]
    cem_cutpoints <- setNames(
      lapply(numeric_vars, function(v) quantile(wide[[v]], probs = probs, na.rm = TRUE)),
      numeric_vars
    )
  }

  form <- as.formula(paste("treatment_group ~", paste(match_cols, collapse = " + ")))

  m.out <- matchit(
    formula   = form,
    data      = wide,
    method    = "cem",
    cutpoints = cem_cutpoints,
    grouping  = cem_grouping,
    k2k       = FALSE
  )

  message(sprintf("CEM: %s tracts in %d — treated in support: %d | controls in support: %d | strata: %d",
                  group_type, treatment_year,
                  sum(m.out$weights[wide$treatment_group == 1] > 0),
                  sum(m.out$weights[wide$treatment_group == 0] > 0),
                  length(unique(na.omit(m.out$subclass)))))

  matched <- wide[m.out$weights > 0, ]
  matched$subclass <- m.out$subclass[m.out$weights > 0]

  # ---------- 5) Anti-exact on CBSA within each stratum ----------
  matched <- matched %>%
    group_by(subclass) %>%
    filter(n() > 1) %>%
    group_modify(~{
      tr <- filter(.x, treatment_group == 1)
      co <- filter(.x, treatment_group == 0)
      if (nrow(tr) == 0 || nrow(co) == 0) return(.x[0, ])
      co2 <- co %>% filter(!cbsa_title %in% unique(tr$cbsa_title))
      if (nrow(co2) == 0) return(.x[0, ])
      bind_rows(tr, co2)
    }) %>%
    ungroup()

  if (nrow(matched) == 0L) {
    warning("After anti-CBSA filtering, no strata have both treated and controls.")
    return(matched[0, ])
  }

  # ---------- 6) 1:1 nearest neighbor within stratum using LSAP ----------
  dist_cols <- grep("^year_", names(matched), value = TRUE)
  dist_cols <- union(dist_cols, time_invariant_vars)

  # Standardize for distance stability
  matched <- matched %>%
    mutate(across(all_of(dist_cols), ~ as.numeric(scale(.x))))

  nn_mahalanobis <- function(stratum_df) {
    tr <- stratum_df %>% filter(treatment_group == 1)
    co <- stratum_df %>% filter(treatment_group == 0)
    if (nrow(tr) == 0 || nrow(co) == 0) return(stratum_df[0, ])

    Xtr <- as.matrix(tr[, dist_cols, drop = FALSE])
    Xco <- as.matrix(co[, dist_cols, drop = FALSE])

    if (nn_distance == "mahalanobis") {
      S <- cov(rbind(Xtr, Xco))
      S <- S + diag(ridge, ncol(S))
      Sinv <- solve(S)

      cost <- outer(seq_len(nrow(Xtr)), seq_len(nrow(Xco)), Vectorize(function(i, j) {
        d <- Xtr[i, ] - Xco[j, ]
        sum((d %*% Sinv) * d)
      }))

    } else { # Euclidean
      cost <- outer(seq_len(nrow(Xtr)), seq_len(nrow(Xco)), Vectorize(function(i, j) {
        sum((Xtr[i, ] - Xco[j, ])^2)
      }))
    }

    # Pad to square and solve assignment
    nT <- nrow(Xtr); nC <- nrow(Xco)
    big <- max(cost) + 1e6
    if (nT > nC) cost <- cbind(cost, matrix(big, nT, nT - nC))
    if (nC > nT) cost <- rbind(cost, matrix(big, nC - nT, nC))
    assign <- solve_LSAP(cost)

    pairs <- tibble(tr_idx = seq_len(nT), co_idx = as.integer(assign[seq_len(nT)])) %>%
      filter(co_idx <= nC)

    out <- pmap_dfr(pairs, function(tr_idx, co_idx) {
      d <- sqrt(cost[tr_idx, co_idx])
      bind_rows(
        tr[tr_idx, ] %>% mutate(.role = "treated", .distance = d),
        co[co_idx, ] %>% mutate(.role = "control", .distance = d)
      )
    }) %>%
      mutate(.pair_id = paste0(first(stratum_df$subclass), "_", row_number()))
    out
  }

  pairs <- matched %>%
    group_split(subclass, .keep = TRUE) %>%
    map_dfr(nn_mahalanobis) %>%
    arrange(subclass, .pair_id, desc(.role))

  message(sprintf("After cross-city filtering and 1:1 matching: %d treated | %d controls",
                  sum(pairs$treatment_group == 1),
                  sum(pairs$treatment_group == 0)))

  # Add treatment year and group type information
  pairs$matched_treatment_year <- treatment_year
  pairs$group_type <- group_type
  pairs$match_type <- "cem"

  # Get variable names for reshaping
  year_vars <- grep("^year_", names(pairs), value = TRUE)

  # Reshape back to long format
  pairs_long <- pairs %>%
    mutate(across(contains("category"), as.character)) %>%
    pivot_longer(
      cols = all_of(year_vars),
      names_to = c("year", ".value"),
      names_pattern = "year_([0-9]+)_(.+)"
    ) %>%
    mutate(year = as.integer(year))

  # Create subclass variable for compatibility
  pairs_long <- pairs_long %>%
    mutate(subclass = .pair_id)

  return(list(matched_data = pairs_long, m.out = m.out))
}
