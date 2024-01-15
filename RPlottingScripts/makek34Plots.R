suppressPackageStartupMessages({

  library(ggplot2)
  library(phytools)
  library(ggtree)
  library(rentrez)
  library(RColorBrewer)
  library(gridExtra)
  library(ggmagnify)
  library(ggfx)
  library(cowplot)
})

source("/home/projects2/animalia_mito_external/soibeanPlots/makeComboPlots.R")

### Combo plot for k 3:
s <- make_tree("Saturniidae.new.dnd")
pp <- make_tree("Phocidae.new.dnd")
print("I can make the trees.")

#k3 <- make_soibean_plots("Saturniidae.new.dnd", "k3SaturniidaeResult30.mcmc", "47-33-20 Mixture of two emperor moths and one ancestral state at ~4x coverage.", s)
#ggsave("k3plot.png", plot = k3, width = 15, height = 7, dpi = 600)

k4 <- make_soibean_plots("Phocidae.new.dnd", "k4PhocaResult40.mcmc", "25-25-25-25 Mixture of four earless seal species at ~5.4x coverage.",pp)
ggsave("k4plot.png", plot = k4, width = 15, height = 7, dpi = 600)

k305 <- make_tree_plots("Saturniidae.new.dnd", "k3Saturniidae_05Result30.mcmc", "47-33-20 Mixture of two emperor moths and one ancestral state at ~2x coverage.")
ggsave("k305plot.png", plot = k305, width = 17, height = 7, dpi = 600)

k405 <- make_soibean_plots("Phocidae.new.dnd", "k4Phoca_05Result40.mcmc", "25-25-25-25 Mixture of four earless seal species ~2.7x coverage.", pp)
ggsave("k405plot.png", plot = k405, width = 15, height = 7, dpi = 600)

#k3025 <- make_tree_plots("Saturniidae.new.dnd", "k3Saturniidae_25Result30.mcmc", "47-33-20 Mixture of two emperor moths and one ancestral state at ~1x coverage.")
#ggsave("k325plot.png", plot = k3025, width = 15, height = 7, dpi = 600)

k4025 <- make_soibean_plots("Phocidae.new.dnd", "k4Phoca_025Result40.mcmc", "25-25-25-25 Mixture of four earless seal species at ~1.3x coverage.", pp)
ggsave("k425plot.png", plot = k4025, width = 15, height = 7, dpi = 600)

#k301 <- make_tree_plots("Saturniidae.new.dnd", "k3Saturniidae_01Result30.mcmc", "47-33-20 Mixture of two emperor moths and one ancestral state at ~0.4x coverage.")
#ggsave("k301plot.png", plot = k301, width = 15, height = 7, dpi = 600)

k401 <- make_soibean_plots("Phocidae.new.dnd", "k4Phoca_01Result40.mcmc", "25-25-25-25 Mixture of four earless seal species at ~0.55x coverage.", pp)
ggsave("k401plot.png", plot = k401, width = 15, height = 7, dpi = 600)


k3down <- plot_grid(
  plot_grid(k305, k3025, k301 nrow = 3, align = 'v'),
   ncol = 1,
   rel_heights = c(0.33, 0.33, 0.33)
)
ggsave("k3downall.png", plot = k3down, width = 15, height = 21, dpi = 600)

k4down <- plot_grid(
  plot_grid(k405, k4025, k401 nrow = 3, align = 'v'),
   ncol = 1,
   rel_heights = c(0.33, 0.33, 0.33)
)
ggsave("k4downall.png", plot = k4down, width = 15, height = 21, dpi = 600)
