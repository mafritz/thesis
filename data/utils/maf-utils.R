library(here)
library(papaja)
library(see)
library(tidyverse)
library(apa)

# Common Utilities

maf.plot_means_comparison <- function(data, mappings) {
  ggplot(as_tibble(data), mappings) +
    geom_jitter(alpha = 0.3, size = 3) +
    stat_summary(fun.data = mean_cl_normal, geom = "errorbar", aes(group = group, color = group), width = 0.4) +
    stat_summary(fun = mean, geom = "point", aes(group = group, color = group), size = 5, shape = 15) +
    labs(
      x = NULL,
      y = "Number of emotions expressed"
    ) +
    theme(
      text = element_text(size = 16)
    ) +
    scale_color_okabeito(palette = "black_first") +
    NULL
}

maf.print_m_sd <- function(data, ci = FALSE, wrap_sd = TRUE) {
  # Mean
  if (ci) {
    m <- apa_print(t.test(data))$estimate
  } else {
    m <- paste0("$M=", mean(data) |> printnum(), "$")
  }

  # SD
  if (wrap_sd) {
    s <- paste0(" ($SD=", printnum(sd(data)), "$)")
  } else {
    s <- paste0(", $SD=", printnum(sd(data)), "$")
  }

  return(paste(m, s, sep = ""))
}

d <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

maf.print_m_sd(d, ci = TRUE, wrap_sd = FALSE)
maf.print_m_sd(d, ci = FALSE, wrap_sd = FALSE)
maf.print_m_sd(d, ci = FALSE, wrap_sd = TRUE)
maf.print_m_sd(d, ci = TRUE, wrap_sd = TRUE)
