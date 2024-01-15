make_trace_plots <- function(trace_file){
  data <- read.csv(trace_file, header = F, sep = '\t')
  extracted <- sub("Trace.*", "", trace_file)
  print(extracted)
  accepted_data <- data[data$V5 == 'accepted', ]
  b <- length(accepted_data$V1) * 0.15
  trace_cut <- accepted_data[-(1:b), 1:2]
  trace <- accepted_data[, 1:2]
  
  trace$y <- as.numeric(trace$V2)
  trace$x <- seq(trace$V2)
  trace_cut$y <- as.numeric(trace_cut$V2)
  trace_cut$x <- seq(trace_cut$V2)
  
  
  ymax1 <- max(trace_cut$y, na.rm = TRUE) 
  ymin1 <- min(trace_cut$y, na.rm = TRUE) 
  xmax1 <- max(trace$x)
  xmin1 <- b
  sta <- (min(trace$y) - ymin1) * 0.2
  ran <- (ymax1 - ymin1)
  
  from <- c(xmin1, xmax1, (ymin1), (ymax1) )
  newymax <- ymin1 + sta
  newymin <- newymax - (ran*50)
  to <- c((xmin1 + (xmin1 *0.2)),  (xmax1-(xmin1 *0.2)), newymin, newymax)
  
    # Now create your plot
  p <- ggplot(trace, aes(x = x, y = y)) +
    geom_point(size = 0.5, color = "darkblue" ) +
    theme_bw() +
    labs(title = "Trace Plot of Log Likelihood", x = "Iteration", y = "Log Likelihood") + 
    geom_magnify(from = from, to = to, shadow = TRUE, colour = "darkgrey", target.linetype = 2, linewidth = 0.5, expand = 0.2)
  
  return(p)
  
  ggsave(paste0(extracted,"Trace.png"), plot = p, dpi = 600, height = 7, width = 10)
}
