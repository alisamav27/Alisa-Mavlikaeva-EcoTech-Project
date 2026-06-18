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
  select(Branch, Tree, Stress, Mean_VARI, Mean_Angle, Mean_Area, Mean_RMS) %>%
  pivot_longer(cols = c(Mean_Angle, Mean_Area, Mean_RMS),
               names_to = "Variable", values_to = "Value") %>%
  mutate(Variable = recode(Variable,
                           Mean_Angle = "Leaf angle (°)",
                           Mean_Area  = "Leaf area (model units^2)",
                           Mean_RMS   = "Rugosity [RMS] (a.u.)"
  ))
# compute R² and p per variable for annotations
stats <- df_long %>%
  group_by(Variable) %>%
  summarise(
    r2 = summary(lm(Value ~ Mean_VARI))$r.squared,
    .groups = "drop"
  ) %>%
  mutate(label = paste0("R² = ", round(r2,3)))
p_scatter <- ggplot(df_long, aes(x = Mean_VARI, y = Value)) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1),
              color = "black", linetype = "dashed", linewidth = 0.7) +
  geom_line(aes(group = Tree, color = Tree), linewidth = 1) +
  geom_point(aes(color = Tree, shape = Stress), size = 7) +
  geom_text(aes(label = Branch, color = Tree),
            vjust = -1.2, size = 7, show.legend = FALSE) +
  geom_text(data = stats, aes(label = label),
            x = -Inf, y = Inf, hjust = -0.1, vjust = 1.3,
            size = 7, color = "black", inherit.aes = FALSE) +
  scale_color_manual(values = c("A"="#e67e22","B"="#8e44ad","C"="#2980b9"),
                     name = "Tree pair") +
  scale_shape_manual(values = c("Healthy"=16,"Stressed"=17),
                     name = "Stress status") +
  facet_wrap(~Variable, scales = "free_y") +
  labs(title = "Fig 1. Leaf geometry correlation with VARI (stress proxy)",
       x = "Mean VARI",
       y = NULL) +
  theme_bw(base_size = 17) +
  theme(axis.text  = element_text(size = 14),
        strip.text = element_text(size = 14, face = "bold"))


ggsave("VARI_geometry_scatter.png", p_scatter, width = 13, height = 6, dpi = 200)

