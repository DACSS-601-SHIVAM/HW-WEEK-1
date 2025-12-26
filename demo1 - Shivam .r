############################################################
# demo.R - Shivam
# Fragility Index 2023 – Data Import, Exploration, Analysis
############################################################

# -----------------------
# Start Fresh
# -----------------------
rm(list = ls())

# -----------------------
# Load Required Packages
# -----------------------
if (!require("rio")) {
  install.packages("rio")
}
library(rio)

if (!require("corrtable")) {
  install.packages("corrtable")
}
library(corrtable)

if (!require("sjPlot")) {
  install.packages("sjPlot")
}
library(sjPlot)

# -----------------------
# Read Data from URL
# -----------------------
linkGit <- "https://github.com/DACSS-Fundamentals/overview/raw/refs/heads/main/FSI-2023-DOWNLOAD.xlsx"
fragility23 <- rio::import(file = linkGit)

# -----------------------
# Exploratory Commands
# -----------------------

# Column names
names(fragility23)

# Data types and structure
str(fragility23)

# First 10 rows
head(fragility23, 10)

# Last 10 rows
tail(fragility23, 10)

# -----------------------
# Transformative Commands
# -----------------------

# Subset selected variables
keep <- grep(
  pattern = "Country|S1|P1|E2|Total",
  x = names(fragility23),
  value = TRUE
)

frag23_sub <- fragility23[, keep]

# Rename indicator columns
names(frag23_sub)[3:5] <- c("S1", "E2", "P1")

# -----------------------
# Filtering
# -----------------------

# Top ten countries by highest E2 score
top10_E2 <- tail(
  frag23_sub[order(-frag23_sub$E2), ],
  10
)

top10_E2$Country

# -----------------------
# Computations
# -----------------------

# Statistical summary
summary(frag23_sub)

# Worst quartile (75th percentile) of Total
q3_Total <- quantile(
  x = frag23_sub$Total,
  probs = 0.75,
  na.rm = TRUE
)

# Correlations among S1, E2, and P1
cor(
  x = frag23_sub[, -c(1, 2)],
  use = "complete.obs"
)

# Correlation matrix with significance
corrtable::correlation_matrix(
  df = frag23_sub[, -c(1, 2)]
)

# Regression: S1 on P1 and E2
model <- lm(S1 ~ P1 + E2, data = frag23_sub)
summary(model)

# -----------------------
# Plotting
# -----------------------

# Histogram of P1
hist(frag23_sub$P1)

# Scatter plot: S1 vs E2
plot(
  x = frag23_sub$S1,
  y = frag23_sub$E2
)

# Identify worst quartile countries
frag23_sub$worstQt <- frag23_sub$Total >= q3_Total

# Scatter plot with worst quartile highlighted
plot(
  frag23_sub$S1,
  frag23_sub$E2,
  pch = 20,
  col = as.factor(frag23_sub$worstQt)
)

# Regression visualization
plot_models(model)
