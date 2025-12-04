# Google Ngram visualization for public housing
# Shows rise and fall of public housing in American discourse

library(ngramr)
library(ggplot2)
library(here)

# Fetch ngram data for public housing only
ngram_data <- ngram("public housing",
                    corpus = "en-US-2019",
                    year_start = 1925,
                    year_end = 2019,
                    smoothing = 3)

# Scale frequency to per million for readability
ngram_data$freq_per_million <- ngram_data$Frequency * 1e6

# Policy milestones
milestones <- data.frame(
  year = c(1937, 1949, 1973, 1992),
  label = c("Housing Act\nof 1937", "Housing Act\nof 1949", "Nixon\nMoratorium", "HOPE VI")
)

max_freq <- max(ngram_data$freq_per_million)

# Base plot function
make_ngram_plot <- function(data, milestones, max_freq, for_slides = FALSE) {

  base_size <- if (for_slides) 16 else 11
  line_size <- if (for_slides) 1.5 else 1
  label_size <- if (for_slides) 4 else 2.8

  p <- ggplot(data, aes(x = Year, y = freq_per_million)) +
    geom_line(linewidth = line_size, color = "#2c7bb6") +
    geom_vline(xintercept = milestones$year, linetype = "dashed", alpha = 0.3) +
    annotate("text", x = milestones$year, y = max_freq * 0.95,
             label = milestones$label, hjust = -0.05, size = label_size, lineheight = 0.85) +
    scale_x_continuous(breaks = seq(1925, 2025, 25), limits = c(1925, 2025)) +
    labs(
      x = NULL,
      y = "Relative frequency"
    ) +
    theme_classic(base_size = base_size) +
    theme(
      axis.line = element_line(color = "grey70"),
      axis.ticks = element_line(color = "grey70"),
      plot.title = element_text(face = "bold"),
      plot.subtitle = element_text(color = "grey40")
    )

  if (!for_slides) {
    p <- p + labs(
      title = "\"Public Housing\" in American Books, 1925-2019",
      subtitle = "Source: Google Ngram Viewer (American English corpus)"
    )
  }

  return(p)
}

# Paper version
p_paper <- make_ngram_plot(ngram_data, milestones, max_freq, for_slides = FALSE)
print(p_paper)

ggsave(here("output", "figures", "exploratory", "google_ngram_public_housing.pdf"),
       p_paper, width = 8, height = 4.5)

# Slide version (larger text, no title - add in slides)
p_slides <- make_ngram_plot(ngram_data, milestones, max_freq, for_slides = TRUE)
print(p_slides)

ggsave(here("output", "figures", "exploratory", "google_ngram_public_housing_slides.pdf"),
       p_slides, width = 10, height = 5.5)
