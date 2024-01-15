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
tree_file <- "Ursidae.new.dnd"
p <- make_tree(tree_file)

# args <- commandArgs(trailingOnly = TRUE)

# if (length(args) != 1) {
#   cat("Usage: Rscript script.R prefix tree file\n")
#   quit("no")
# }

prefix <- "BlackPanda"



files <- list.files(pattern = paste0(prefix, "\\d+Result20.mcmc$"))

p1 <- make_soibean_plots(tree_file,files[4], "95-55 Mixture of an American Black Bear and a Panda Bear", p)
p2 <- make_soibean_plots(tree_file,files[3], "85-15 Mixture of an American Black Bear and a Panda Bear", p)
p3 <- make_soibean_plots(tree_file,files[2], "75-25 Mixture of an American Black Bear and a Panda Bear", p)
p4 <- make_soibean_plots(tree_file,files[1], "5-45 Mixture of an American Black Bear and a Panda Bear", p)


blackpanda <- plot_grid(
  plot_grid(p1, p2, p3, p4, nrow = 4, align = 'v'), 
  ncol = 1, 
  rel_heights = c(0.25, 0.25, 0.25, 0.25)  # Adjust the relative height of plots and legend
)

# If you want to save the combined plot
ggsave("blackpanda.png", plot = blackpanda, width = 18, height = 29, dpi = 600)



prefix <- "CaveBrown"


files <- list.files(pattern = paste0(prefix, "\\d+Result20.mcmc$"))

p11 <- make_soibean_plots(tree_file,files[4], "95-5 Mixture of a Cave Bear and a Brown Bear", p)
p22 <- make_soibean_plots(tree_file,files[3], "85-15 Mixture of a Cave Bear and a Brown Bear", p)
p33 <- make_soibean_plots(tree_file,files[2], "75-25 Mixture of a Cave Bear and a Brown Bear", p)
p44 <- make_soibean_plots(tree_file,files[1], "55-45 Mixture of a Cave Bear and a Brown Bear", p)



cavebrown <- plot_grid(
   plot_grid(p11, p22, p33, p44, nrow = 4, align = 'v'), 
   ncol = 1, 
   rel_heights = c(0.25, 0.25, 0.25, 0.25)
)  # Adjust the relative height of plots and legend
ggsave("cavebrown.png", plot = cavebrown,width = 18, height = 29, dpi = 600)


prefix <- "TaiwanThibetBlackBear"


files <- list.files(pattern = paste0(prefix, "\\d+Result20.mcmc$"))
p111 <- make_soibean_plots(tree_file,files[4], "95-5 Mixture of a Taiwan and Thibettan Black Bear", p)
p222 <- make_soibean_plots(tree_file,files[3], "85-15 Mixture of a Taiwan and Thibettan Black Bear", p)
p333 <- make_soibean_plots(tree_file,files[2], "75-25 Mixture of a Taiwan and Thibettan Black Bear", p)
p444 <- make_soibean_plots(tree_file,files[1], "55-45 Mixture of a Taiwan and Thibettan Black Bear", p)

tibettaiwan <- plot_grid(
   plot_grid(p111, p222, p333, p444, nrow = 4, align = 'v'), 
   ncol = 1, 
   rel_heights = c(0.25, 0.25, 0.25, 0.25)
)  # Adjust the relative height of plots and legend
ggsave("tibettaiwan.png", plot = tibettaiwan, width = 18, height = 29, dpi = 600)


######### Black Panda downsampled 0.5
prefix <- "CaveBrown"


files <- list.files(pattern = paste0(prefix, "\\d+_0.5Result20.mcmc$"))
p1_05 <- make_soibean_plots(tree_file,files[1], "55-45 Mixture of an American Black Bear and a Panda Bear downsampled to ~1.3x coverage", p)
p2_05 <- make_soibean_plots(tree_file,files[2], "75-25 Mixture of an American Black Bear and a Panda Bear downsampled to ~1.3x coverage", p)
p3_05 <- make_soibean_plots(tree_file,files[3], "85-15 Mixture of an American Black Bear and a Panda Bear downsampled to ~1.3x coverage", p)
p4_05 <- make_soibean_plots(tree_file,files[4], "95-5 Mixture of an American Black Bear and a Panda Bear downsampled to ~1.3x coverage", p)


blackpanda_05 <- plot_grid(
  plot_grid(p4_05, p3_05, p2_05, p1_05, nrow = 4, align = 'v'), 
  ncol = 1, 
  rel_heights = c(0.25, 0.25, 0.25, 0.25)  # Adjust the relative height of plots and legend
)
ggsave("blackpanda_05.png", plot = blackpanda_05, width = 18, height = 29, dpi = 600)



############## Cave brown downsampled 0.5
prefix <- "BlackPanda"


files <- list.files(pattern = paste0(prefix, "\\d+_0.5Result20.mcmc$"))
p11_05 <- make_soibean_plots(tree_file,files[4], "95-5 Mixture of a Cave Bear and a Brown Bear downsampled to ~1.3x coverage", p)
p22_05 <- make_soibean_plots(tree_file,files[3], "85-15 Mixture of a Cave Bear and a Brown Bear downsampled to ~1.3x coverage", p)
p33_05 <- make_soibean_plots(tree_file,files[2], "75-25 Mixture of a Cave Bear and a Brown Bear downsampled to ~1.3x coverage", p)
p44_05 <- make_soibean_plots(tree_file,files[1], "55-45 Mixture of a Cave Bear and a Brown Bear downsampled to ~1.3x coverage", p)


cavebrown_05 <- plot_grid(
   plot_grid(p11_05, p22_05, p33_05, p44_05, nrow = 4, align = 'v'), 
   ncol = 1, 
   rel_heights = c(0.25, 0.25, 0.25, 0.25)
)  # Adjust the relative height of plots and legend
ggsave("cavebrown_05.png", plot = cavebrown_05, width = 18, height = 29, dpi = 600)

############## Asian bears downsampled 0.5 
prefix <- "TaiwanThibetBlackBear"


files <- list.files(pattern = paste0(prefix, "\\d+_0.5Result20.mcmc$"))
p111_05 <- make_soibean_plots(tree_file,files[4], "95-5 Mixture of a Taiwan and Thibettan Black Bear downsampled to ~1.3x coverage", p)
p222_05 <- make_soibean_plots(tree_file,files[3], "85-15 Mixture of a Taiwan and Thibettan Black Bear downsampled to ~1.3x coverage", p)
p333_05 <- make_soibean_plots(tree_file,files[2], "75-25 Mixture of a Taiwan and Thibettan Black Bear downsampled to ~1.3x coverage", p)
p444_05 <- make_soibean_plots(tree_file,files[1], "55-45 Mixture of a Taiwan and Thibettan Black Bear downsampled to ~1.3x coverage", p)

tibettaiwan <- plot_grid(
    plot_grid(p111, p222, p333, p444, nrow = 4, align = 'v'), 
    ncol = 1, 
    rel_heights = c(0.25, 0.25, 0.25, 0.25) 
)# Adjust the relative height of plots and legend
ggsave("tibettaiwan_05.png", plot = tibettaiwan, width = 18, height = 29, dpi = 600)

######### Black Panda downsampled 0.1
prefix <- "BlackPanda"


files <- list.files(pattern = paste0(prefix, "\\d+_0.1Result20.mcmc$"))
p1_01 <- make_soibean_plots(tree_file,files[1], "55-45 Mixture of a Cave Bear and a Brown Bear downsampled to ~0.25x coverage", p)
p2_01 <- make_soibean_plots(tree_file,files[2], "75-25 Mixture of a Cave Bear and a Brown Bear downsampled to ~0.25x coverage", p)
p3_01 <- make_soibean_plots(tree_file,files[3], "85-15 Mixture of a Cave Bear and a Brown Bear downsampled to ~0.25x coverage", p)
p4_01 <- make_soibean_plots(tree_file,files[4], "95-5 Mixture of a Cave Bear and a Brown Bear downsampled to ~0.25x coverage", p)


cavebrown_01 <- plot_grid(
  plot_grid(p4_01, p3_01, p2_01, p1_01, nrow = 4, align = 'v'), 
  ncol = 1, 
  rel_heights = c(0.25, 0.25, 0.25, 0.25)  # Adjust the relative height of plots and legend
)
ggsave("cavebrown_01.png", plot = cavebrown_01, width = 18, height = 29, dpi = 600)


############## Cave brown downsampled 0.5
prefix <- "BlackPanda"


files <- list.files(pattern = paste0(prefix, "\\d+_0.25Result20.mcmc$"))
p11_25 <- make_soibean_plots(tree_file,files[4], "95-5 Mixture of a Cave Bear and a Brown Bear downsampled to ~0.7x coverage", p)
p22_25 <- make_soibean_plots(tree_file,files[3], "85-15 Mixture of a Cave Bear and a Brown Bear downsampled to ~0.7x coverage", p)
p33_25 <- make_soibean_plots(tree_file,files[2], "75-25 Mixture of a Cave Bear and a Brown Bear downsampled to ~0.7x coverage", p)
p44_25 <- make_soibean_plots(tree_file,files[1], "55-45 Mixture of a Cave Bear and a Brown Bear downsampled to ~0.7x coverage", p)


cavebrown_25 <- plot_grid(
   plot_grid(p11_25, p22_25, p33_25, p44_25, nrow = 4, align = 'v'), 
   ncol = 1, 
   rel_heights = c(0.25, 0.25, 0.25, 0.25) 
) # Adjust the relative height of plots and legend
ggsave("cavebrown_25.png", plot = cavebrown_25, width = 18, height = 29, dpi = 600)

############## Asian bears downsampled 0.5 
prefix <- "TaiwanThibetBlackBear"


files <- list.files(pattern = paste0(prefix, "\\d+_0.25Result20.mcmc$"))
p111_25 <- make_soibean_plots(tree_file,files[4], "95-5 Mixture of a Taiwan and Thibettan Black Bear downsampled to ~0.7x coverage", p)
p222_25 <- make_soibean_plots(tree_file,files[3], "85-15 Mixture of a Taiwan and Thibettan Black Bear downsampled to ~0.7x coverage", p)
p333_25 <- make_soibean_plots(tree_file,files[2], "75-25 Mixture of a Taiwan and Thibettan Black Bear downsampled to ~0.7x coverage", p)
p444_25 <- make_soibean_plots(tree_file,files[1], "55-45 Mixture of a Taiwan and Thibettan Black Bear downsampled to ~0.7x coverage", p)

tibettaiwan_25 <- plot_grid(
    plot_grid(p111_25, p222_25, p333_25, p444_25, nrow = 4, align = 'v'), 
    ncol = 1, 
    rel_heights = c(0.25, 0.25, 0.25, 0.25)
)  
ggsave("tibettaiwan_25.png", plot = tibettaiwan_25, width = 18, height = 29, dpi = 600)

######### Black Panda downsampled 0.25
prefix <- "CaveBrown"


files <- list.files(pattern = paste0(prefix, "\\d+_0.25Result20.mcmc$"))
 p1_25 <- make_soibean_plots(tree_file,files[1], "55-45 Mixture of an American Black Bear and a Panda Bear downsampled to ~0.7x coverage", p)
 p2_25 <- make_soibean_plots(tree_file,files[2], "75-25 Mixture of an American Black Bear and a Panda Bear downsampled to ~0.7x coverage", p)
 p3_25 <- make_soibean_plots(tree_file,files[3], "85-15 Mixture of an American Black Bear and a Panda Bear downsampled to ~0.7x coverage", p)
 p4_25 <- make_soibean_plots(tree_file,files[4], "95-5 Mixture of an American Black Bear and a Panda Bear downsampled to ~0.7x coverage", p)


 blackpanda_025 <- plot_grid(
  plot_grid(p4_25, p3_25, p2_25, p1_25, nrow = 4, align = 'v'), 
   ncol = 1, 
   rel_heights = c(0.25, 0.25, 0.25, 0.25)  
)
 ggsave("blackpanda_025.png", plot = blackpanda_025, width = 17, height = 27, dpi = 600)


############### Asian bears downsampled 0.1
prefix <- "TaiwanThibetBlackBear"


files <- list.files(pattern = paste0(prefix, "\\d+_0.1Result20.mcmc$")) 
p111_1 <- make_tree_plots(tree_file,files[4], "95-5 Mixture of a Taiwan and Thibettan Black Bear downsampled to ~0.25x coverage")
p222_1 <- make_soibean_plots(tree_file,files[3], "85-15 Mixture of a Taiwan and Thibettan Black Bear downsampled to ~0.25x coverage", p)
p333_1 <- make_soibean_plots(tree_file,files[2], "75-25 Mixture of a Taiwan and Thibettan Black Bear downsampled to ~0.25x coverage", p)
p444_1 <- make_soibean_plots(tree_file,files[1], "55-45 Mixture of a Taiwan and Thibettan Black Bear downsampled to ~0.25x coverage", p)

tibettaiwan_1 <- plot_grid(
    plot_grid(p111_1, p222_1, p333_1, p444_1, nrow = 4, align = 'v'), 
    ncol = 1, 
    rel_heights = c(0.25, 0.25, 0.25, 0.25)
) 
ggsave("tibettaiwan_1.png", plot = tibettaiwan_1,width = 18, height = 29, dpi = 600)

######### Black Panda downsampled 0.25
prefix <- "CaveBrown"


files <- list.files(pattern = paste0(prefix, "\\d+_0.10Result20.mcmc$"))
 p1_25 <- make_soibean_plots(tree_file,files[1], "55-45 Mixture of an American Black Bear and a Panda Bear downsampled to ~0.25x coverage", p)
 p2_25 <- make_soibean_plots(tree_file,files[2], "75-25 Mixture of an American Black Bear and a Panda Bear downsampled to ~0.25x coverage", p)
 p3_25 <- make_soibean_plots(tree_file,files[3], "85-15 Mixture of an American Black Bear and a Panda Bear downsampled to ~0.25x coverage", p)
 p4_25 <- make_soibean_plots(tree_file,files[4], "95-5 Mixture of an American Black Bear and a Panda Bear downsampled to ~0.25x coverage", p)


 blackpanda_01 <- plot_grid(
  plot_grid(p4_25, p3_25, p2_25, p1_25, nrow = 4, align = 'v'), 
   ncol = 1, 
   rel_heights = c(0.25, 0.25, 0.25, 0.25)  
)
 ggsave("blackpanda_01.png", plot = blackpanda_01, width = 18, height = 29, dpi = 600)


### Combo plot for k 3:
s <- make_tree("Saturniidae.new.dnd")
pp <- make_tree("Phocidae.new.dnd")
print("I can make the trees.")

k3 <- make_soibean_plots("Saturniidae.new.dnd", "k3SaturniidaeResult30.mcmc", "47-33-20 Mixture of two emperor moths and one ancestral state at ~4x coverage.", s)
ggsave("k3plot.png", plot = k3, width = 15, height = 7, dpi = 600)

k4 <- make_soibean_plots("Phocidae.new.dnd", "k4PhocaResult40.mcmc", "25-25-25-25 Mixture of two emperor moths and one ancestral state at ~5.4x coverage.",pp)
ggsave("k4plot.png", plot = k4, width = 15, height = 7, dpi = 600)


k305 <- make_soibean_plots("Saturniidae.new.dnd", "k3Saturniidae_05Result30.mcmc", "47-33-20 Mixture of two emperor moths and one ancestral state at ~2x coverage.", s)
ggsave("k305plot.png", plot = k305, width = 15, height = 7, dpi = 600)

k405 <- make_soibean_plots("Phocidae.new.dnd", "k4Phoca_05Result40.mcmc", "25-25-25-25 Mixture of two emperor moths and one ancestral state at ~2.7x coverage.", pp)
ggsave("k405plot.png", plot = k405, width = 15, height = 7, dpi = 600)

k3025 <- make_soibean_plots("Saturniidae.new.dnd", "k3Saturniidae_25Result30.mcmc", "47-33-20 Mixture of two emperor moths and one ancestral state at ~1x coverage.", s)
ggsave("k325plot.png", plot = k3025, width = 15, height = 7, dpi = 600)

k4025 <- make_soibean_plots("Phocidae.new.dnd", "k4Phoca_025Result40.mcmc", "25-25-25-25 Mixture of two emperor moths and one ancestral state at ~1.3x coverage.", pp)
ggsave("k425plot.png", plot = k4025, width = 15, height = 7, dpi = 600)

k301 <- make_soibean_plots("Saturniidae.new.dnd", "k3Saturniidae_01Result30.mcmc", "47-33-20 Mixture of two emperor moths and one ancestral state at ~0.4x coverage.", s)
ggsave("k301plot.png", plot = k301, width = 15, height = 7, dpi = 600)

k401 <- make_soibean_plots("Phocidae.new.dnd", "k4Phoca_01Result40.mcmc", "25-25-25-25 Mixture of two emperor moths and one ancestral state at ~0.55x coverage.", pp)
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
