## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
## Script:            Political Discrimination & Political Change - Figures
## Author(s):         Carlos A. Toruño Paniagua   (ctoruno@worldjusticeproject.org)
## Dependencies:      World Justice Project
## Creation date:     January 5th, 2025
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

trust_comparison_naive <- function(tsbd_data){
  
  ## Drawing boxplot
  fig_tsbd <- ggplot(
    data = tsbd_data %>% 
      filter(!is.na(dh_exp)) %>%
      group_by(dh_exp) %>%
      mutate(
        mean = mean(trt_score_scaled, na.rm = TRUE),
        median = median(trt_score_scaled, na.rm = TRUE),
        sd  = sd(trt_score_scaled, na.rm = TRUE),
        n  = n(),
        se = sd/sqrt(n),
        t_val  = qt(p = 0.975, df = n - 1),
        lower  = median - t_val * se,
        upper  = median + t_val * se
      ),
    aes(
      x = dh_exp,
      y = trt_score_scaled
    )
  ) +
    geom_boxplot() +
    geom_errorbar(
      aes(
        ymin = lower, 
        ymax = upper
      ), 
      width = 0.75,
      color = "#901C14",
      linewidth = 3
    ) +
    labs(
      y = "Trust in Political Institutions (Score)",
      x = "",
      # caption = paste(
      #   "Note: The figure shows the distribution of Trust in Political Institutions Index scores for individuals who",
      #   "experienced political discrimination\nor harassment (D/H) versus those who did not. Index scores are",
      #   "derived from Logistic PCA of trust responses across nine institutional categories: local authorities,\n",
      #   "national authorities, police, prosecutors, public defense attorneys, judges, magistrates, political parties,",
      #   "and Parliament members.\nRed areas show 95% confidence intervals of median scores."
      # )
    ) + 
    theme_minimal() +
    theme(
      plot.caption = element_text(hjust = 0),
      plot.caption.position = "plot",
      panel.grid.major.x = element_blank()
    )
  
  ## Saving boxplot
  ggsave(
    plot     = fig_tsbd,
    filename = glue("viz/fig_trust_comparison_naive.png"),
    width    = 8,
    height   = 6,
    units    = "in"
  ) 
  
  return(TRUE)
}


love_plot <- function(m.out1){
  
  ## Converting data
  df <- map_dfr(
    c("sum.all", "sum.matched"),
    function(data){
      as.data.frame(
        summary(
          m.out1, 
          addlvariables = ~ I(age^2) + I(polid^2)
        )[[data]][1:14,]
      ) %>%
        mutate(
          `Std. Mean Diff.` = abs(`Std. Mean Diff.`),
          process = data
        ) %>%
        rownames_to_column("variable")
    }
  ) %>%
    mutate(
      process = if_else(
        process == "sum.all", "Full Sample", "Matched Sample"
      )
    )
  
  ## Drawing Love plot
  fig <- ggplot(
    data = df,
    aes(
      x = `Std. Mean Diff.`,
      y = reorder(variable, `Std. Mean Diff.`),
      color = process,
      shape = process
    )
  ) +
    geom_point(
      size = 4
    ) +
    geom_vline(
      xintercept = 0.05,
      linetype = "dotted",
      color = "grey45"
    ) +
    geom_vline(
      xintercept = 0.10,
      linetype = "dashed",
      color = "grey45"
    ) +
    labs(
      x = "Absolute Std. Mean Diff."
    ) +
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      axis.title.y = element_blank()
    )
  
  ## Saving boxplot
  ggsave(
    plot     = fig,
    filename = glue("viz/loveplot.png"),
    width    = 8,
    height   = 6,
    units    = "in"
  ) 
  
  return(TRUE)
}
