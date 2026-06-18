---
title: "Angle, Area Boxplots"
output: pdf_document
date: "2026-06-18"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r cars}
summary(cars)
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

df <- data.frame(
  Branch     = c("1A","1B","1C","2A","2B","2C"),
  Tree       = c("A","B","C","A","B","C"),
  Stress     = c("Stressed","Stressed","Stressed","Healthy","Healthy","Healthy"),
  Mean_VARI  = c(0.033603, 0.083127, 0.101098, 0.107514, 0.115862, 0.138899),
  Mean_Area  = c(0.034329, 0.031569, 0.047586, 0.105532, 0.092913, 0.095712),
  Mean_RMS   = c(0.018360, 0.031557, 0.014669, 0.036528, 0.030013, 0.009465),
  Mean_Angle = c(36.5, 19.9, 40.3, 103.7, 109.2, 107.2)
)
df$Stress <- factor(df$Stress, levels = c("Healthy","Stressed"))

df_long <- df %>%
  select(Branch, Tree, Stress, Mean_Angle, Mean_Area, Mean_RMS) %>%
  pivot_longer(cols = c(Mean_Angle, Mean_Area, Mean_RMS),
               names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = recode(Variable,
                           Mean_Angle = "Leaf angle (°)",
                           Mean_Area  = "Leaf area (model units)",   # no ^2
                           Mean_RMS   = "Rugosity (RMS)"
  ))
make_panel <- function(var_name, show_legend = FALSE) {
  sub_df <- df_long %>% filter(Variable == var_name)
  lab    <- tlabels %>% filter(Variable == var_name) %>% pull(label)
  y_max  <- max(sub_df$Value)
  y_min  <- min(sub_df$Value)
  y_ann  <- y_max + (y_max - y_min) * 0.08
  
  ggplot(sub_df, aes(x = Stress, y = Value)) +
    geom_boxplot(aes(fill = Stress), alpha = 0.5, outlier.shape = NA, width = 0.55) +
    geom_line(aes(group = Tree, color = Tree), linewidth = 1.8) +
    geom_point(aes(color = Tree), size = 7) +
    annotate("text", x = 1.5, y = y_ann + (y_max - y_min) * 0.05,
             label = lab, size = 7, color = "black") +
    scale_fill_manual(values = c("Healthy" = "#2ecc71", "Stressed" = "#e74c3c"),
                      name = "Stress status") +
    scale_color_manual(values = c("A" = "#e67e22", "B" = "#8e44ad", "C" = "#2980b9"),
                       name = "Tree pair") +
    scale_x_discrete(expand = expansion(add = 0.6)) +
    scale_y_continuous(limits = c(y_min - (y_max - y_min) * 0.03,
                                  y_ann  + (y_max - y_min) * 0.25)) +
    labs(title = var_name, x = NULL, y = NULL) +
    theme_bw(base_size = 18) +
    theme(legend.position = if (show_legend) "right" else "none",
          axis.text        = element_text(size = 18),
          plot.title       = element_text(size = 17, face = "bold", hjust = 0.5),
          plot.margin      = margin(2, 5, 2, 5))
}

p1 <- make_panel("Leaf angle (°)")
p2 <- make_panel("Leaf area (model units)", show_legend = TRUE)

final <- (p1 | p2) +
  plot_annotation(
    title = "Fig 2. Paired comparison of leaf geometry under stress",
    theme = theme_bw(base_size = 18)
  )

ggsave("boxplots_angle_area.png", final, width = 11, height = 6, dpi = 200)
