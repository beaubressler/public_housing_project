save_event_study_plots <- function(reg_results_df, outcome_variables, results_dir, 
                                   prefix, title_suffix,
                                   category_filter = NULL,
                                   heterogeneity = NULL) {
  
  # Define the file name
  pdf_file_name <- here(results_dir, paste0(prefix, ".pdf"))
  
  # Open PDF device
  pdf(pdf_file_name, width = 10, height = 5)
  
  for (outcome in outcome_variables) {
    # Create the plot
    plot <- create_event_study_plot(
      reg_results_df = reg_results_df,
      dep_var = outcome,
      title = paste0("Matched DiD: Effect of public housing on ", outcome, title_suffix),
      category_filter = category_filter,
      heterogeneity = heterogeneity  # This determines if it's a heterogeneity analysis
    )
    
    print(plot)
  }
  
  # Close the PDF device
  dev.off()
}
