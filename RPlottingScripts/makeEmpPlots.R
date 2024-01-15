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

args <- commandArgs(trailingOnly = TRUE)
source("/home/projects2/animalia_mito_external/soibeanPlots/makeComboPlots.R")
tree_file <- args[1]
p <- make_tree(tree_file)

input_file <- args[2]
prefix <- args[3]
label <- args[4]
out <- args[5]

kcurve <- process_and_plot(prefix)
p1 <- make_soibean_plots(tree_file, input_file, label, p)

f <- plot_grid(kcurve, p1, ncol = 2, rel_widths = c(0.25,0.75))
# Use ggdraw to add labels in custom positions
#final_plot <- ggdraw(f) +
#              draw_label("A", x = 0.1, y = 0.9, hjust = 0, vjust = 0, size = 20) +
#              draw_label("B", x = 0.6, y = 0.9, hjust = 0, vjust = 0, size = 20)
ggsave(out, plot = f, width = 25, heigh = 15, dpi = 600)

