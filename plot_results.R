#################   Load Packages   ################

library(ggplot2)
library(amt)
library(scales)
library(ggpubr)




#################   Load Data and Model   ##################

# dataAll: coyote gps data with environmental and social covariates
load("ssf_data.rdata")

# ssf_fit: model fit with AMT
load("ssf_fit.rdata")




###############   Calculate Plot Parameters   ##################

# function finds standardized human population density value based on actual val
unstand <- function(i) {
  val <- i - mean(na.omit(dataAll$popDens))
  val <- val/sd(na.omit(dataAll$popDens))
  return(val)
}

# human population density values to display on figures
vals <- c(0, 1150, 5000, 10000, 20000)

# determine breaks to pass to ggplot
brks <- do.call(rbind, lapply(1:length(vals), function(i) {
  return(unstand(vals[i]))
}))

# labels for x-axis
lbls <- c("0", "1,150 (avg)", "5,000", "10,000", "20,000")

# social and environmental variables
int_vars = c("natST", "distST", "agST", "medIncST", "propWST")
int_lbls = c("Natural habitat", "Disturbed habitat", "Agriculture", 
             "Median income", "Proportion white")



##################   Plot Function   #################

plot_int <-  function(var, hi_val, low_val, title, brks, txt_sz) {
  # low value predictions
  low <- data.frame(val = "low", medIncST = 0, propWST = 0, distST = 0, 
                    natST = 0, agST = 0, popDensST = seq(min(brks), max(brks), 
                                                  length.out = 100))
  low[, var] <- low_val
  low_ref <- data.frame(val = "low", medIncST = 0, propWST = 0, distST = 0, 
                        natST = 0, agST = 0, popDensST = 0)
  low_ref[, var] <- low_val
  low_df <- log_rss(fitAllAMT, low, low_ref, ci = "se")
  
  # high value predictions
  hi<- data.frame(val = "hi", medIncST = 0, propWST = 0, distST = 0, natST = 0, 
                       agST = 0, popDensST = seq(min(brks), max(brks), 
                                                 length.out = 100))
  hi[, var] <- hi_val
  hi_ref <- data.frame(val = "hi", medIncST = 0, propWST = 0, distST = 0, 
                       natST = 0, agST = 0, popDensST=0)
  hi_ref[, var] <- hi_val
  hi_df <- log_rss(fitAllAMT, hi, hi_ref, ci = "se")
  
  # combine for plotting
  df <- rbind(hi_df$df, low_df$df)
  
  # create plot
  plot <- ggplot(df, aes(x = popDensST_x1, y = log_rss, color = val_x1)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray") +
    geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "gray80", alpha = .5,
                linetype = 0) +
    geom_line(size = 1) +
    scale_color_manual(values=c("darkorange1","steelblue2"), 
                       labels = c("High (µ+σ)", "Low (µ-σ)")) +
    xlab("Population density (per km²)") +
    ylab("Relative selection strength") +
    labs(title = title, color = "") +
    theme_bw() + 
    theme(text = element_text(size = 14), 
          axis.text.x = element_text(angle = 45, hjust = 1, size = 12), 
          panel.grid.minor.x = element_blank(), 
          panel.grid.minor.y = element_blank(),
          legend.title=element_text(size=20),
          legend.text=element_text(size=14))
  
  plot + scale_x_continuous(breaks = brks, limits = range(brks), 
                               labels = lbls) + 
    scale_y_continuous(limits=c(-12, 0), breaks = seq(0, -12, by=-2))
}




################    Generate and Combine Plots    #####################

int_plts <- lapply(1:length(int_vars), function(i) {
  plot_int(int_vars[i], 1, -1, int_lbls[i], brks, txt_sz) 
})

figure <- ggarrange(plotlist = int_plts,
                    labels = c("A","B", "C", "D", "E"),
                    ncol = 3, nrow = 2, common.legend = TRUE, 
                    legend = "bottom")

ggexport(figure, filename = "rss_figure.png", width=900, height=700)
