# Packages
library(tidyverse)
library(patchwork)

# Read data
data <- read.csv("FILE",
                 stringsAsFactors = FALSE, check.names = FALSE) ##Add file path

# Ensure types are right
data <- data %>%
  mutate(
    test = as.character(test),
    category = as.character(category),
    count = as.numeric(count),
    percent = as.numeric(percent)
  )

make_funnel <- function(df, assay_color = "#66b2ff", rect_height = 10, spacing = 0.5) {
  df <- df %>% mutate(stage = row_number())
  
  rects <- df %>%
    mutate(
      y_min = ((n() - stage) * (rect_height + spacing)),
      y_max = y_min + rect_height,
      half_width = percent / 2,
      x_min = -half_width,
      x_max = half_width,
      y_mid = (y_min + y_max) / 2
    )
  
  ggplot(rects) +
    geom_rect(aes(xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max),
              fill = assay_color, alpha = 0.4, color = assay_color) +
    # Category labels on the left, values centered
    geom_text(aes(x = min(x_min) - max(percent) * 0.05, y = y_mid,
                  label = as.character(category)),
              hjust = 0.9, size = 5) +
    geom_text(aes(x = 0, y = y_mid,
                  label = paste0(count, " (", round(percent, 1), "%)")),
              vjust = 0.5, size = 4) +
    scale_x_continuous(NULL, breaks = NULL, labels = NULL) +
    scale_y_continuous(NULL, breaks = NULL, labels = NULL) +
    labs(title = unique(df$test)) +
    coord_fixed(ratio = 0.12, clip = "off") +   # tweak ratio to taste
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.margin = margin(10, 30, 10, 60)      # room for left labels
    )
}

# Split data by assay
rf_data   <- dplyr::filter(data, test == "RF")
acpa_data <- dplyr::filter(data, test == "ACPA")

# Create funnels
rf_funnel   <- make_funnel(rf_data,   "#66b2ff")
acpa_funnel <- make_funnel(acpa_data, "#99cc99")

# Combine side by side (patchwork)
rf_funnel | acpa_funnel

###chi sqr tests

##read in data
data <- read.csv("FILE",
                 stringsAsFactors = FALSE, check.names = FALSE)  ##Add file path

# chi sqr positive tests
chisq.test(data[, c("positive", "negative")], correct = FALSE)
# chi sqr referrals
chisq.test(data[, c("referred", "not_referred")], correct = FALSE)
# chi sqr false positive referrals
chisq.test(data[, c("false_positive", "true_positive")], correct = FALSE)

###Risk ratios
library(epitools)

# Risk ratio for positive test
rr <- riskratio(as.matrix(data[, c("positive", "negative")]))

rr

# Risk ratio for referrals
rr <- riskratio(as.matrix(data[, c("referred", "not_referred")]))

rr

# Risk ratio for false positive
rr <- riskratio(as.matrix(data[, c("false_positive", "true_positive")]))

rr

###sensitivity analysis- rejected referrals excluded
##read in data
data <- read.csv("FILE",
                 stringsAsFactors = FALSE, check.names = FALSE)  ##Add file path

# chi sqr false positive referrals
chisq.test(data[, c("false_positive", "true_positive")], correct = FALSE)

# Risk ratio for false positive
rr <- riskratio(as.matrix(data[, c("false_positive", "true_positive")]))

rr

