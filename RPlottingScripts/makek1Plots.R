### MAKE K1 PLOTS

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

# Create banner plots
banner1 <- create_banner("Phylogenetic tree for the family Ursidae")
banner2 <- create_banner("~1x coverage")
banner3 <- create_banner("~0.5x coverage")
banner5 <- create_banner("~0.15x coverage")
banner6 <- create_banner("~0.1x coverage")
banner7 <- create_banner("~0.02x coverage")

tree_file <- "Ursidae.new.dnd"
if1 <- "ancN4_2_1000Result12.mcmc"
if2 <- "ancN4_2_500Result12.mcmc"
if3 <- "ancN4_2_250Result12.mcmc"
if4 <- "ancN4_2_125Result12.mcmc"
if5 <- "ancN4_2_75Result12.mcmc"
if6 <- "ancN4_2_50Result12.mcmc"
if7 <- "ancN4_2_10Result12.mcmc"

t2 <- k1make_tree_plots(tree_file, if2)
t3 <- k1make_tree_plots(tree_file, if3)
t5 <- k1make_tree_plots(tree_file, if5)
t6 <- k1make_tree_plots(tree_file, if6)
t7 <- k1make_tree_plots(tree_file, if7)

#tf1 <- "ancN4_4_1000Trace13.detail.mcmc"
tf2 <- "ancN4_2_500Trace12.detail.mcmc"
tf3 <- "ancN4_2_250Trace12.detail.mcmc"
#tf4 <- "ancN4_4_125Trace13.detail.mcmc"
tf5 <- "ancN4_2_75Trace12.detail.mcmc"
tf6 <- "ancN4_2_50Trace12.detail.mcmc"
tf7 <- "ancN4_2_10Trace12.detail.mcmc"

#p1 <- make_trace_plots(tf1)
p2 <- make_trace_plots(tf2)
p3 <- make_trace_plots(tf3)
#p4 <- make_trace_plots(tf4)
p5 <- make_trace_plots(tf5)
p6 <- make_trace_plots(tf6)
p7 <- make_trace_plots(tf7)

# Arranging individual sets of plots
c2 <- plot_grid(banner2, p2, t2, ncol = 3, rel_widths = c(0.5, 3, 7))
c3 <- plot_grid(banner3, p3, t3, ncol = 3, rel_widths = c(0.5, 3, 7))
c5 <- plot_grid(banner5, p5, t5, ncol = 3, rel_widths = c(0.5, 3, 7))
c6 <- plot_grid(banner6, p6, t6, ncol = 3, rel_widths = c(0.5, 3, 7))
c7 <- plot_grid(banner7, p7, t7, ncol = 3, rel_widths = c(0.5, 3, 7))

# Combining the above plots into one
downALL <- plot_grid(c2, c3, c5, c6, c7, nrow = 5, align = 'v')

# Arranging last set of plots
last <- plot_grid(banner1, tr, ncol = 2)

# Final arrangement
test <- plot_grid(last, downALL, ncol = 1, align = 'v')


ggsave("downsampleK11.png", plot = test, dpi = 600, width = 14, height = 21)
ggsave("downsampleK12.png", plot = test, dpi = 600, width = 16, height = 23)
ggsave("downsampleK13.png", plot = test, dpi = 600, width = 15, height = 22)
ggsave("downsampleK14.png", plot = test, dpi = 600, width = 13, height = 20)
