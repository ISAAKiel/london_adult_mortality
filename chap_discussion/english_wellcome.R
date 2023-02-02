english_wellcome <- rbind(english_mortality_prep, wellcome_prep)
english_wellcome$data <- factor(english_wellcome$data, levels = unique(english_wellcome$data))
english_wellcome$start <- as.numeric(english_wellcome$start) 
english_wellcome$end <- as.numeric(english_wellcome$end) 

english_wellcome_plot <- ggplotGrob(
  ggplot(english_wellcome, aes(colour = data, shape = source) ) + 
    ylab("modal age & HDI low - HDI high")  + 
    xlab("year (from - to)") + ylim(2, 75) +
    scale_color_manual(values=pals::cols25()) +
    geom_errorbar(aes(x = (start + end) / 2, y = M, 
                    ymin = HDIlow, ymax=  HDIhigh), width=0, colour = "dark grey") +
    geom_errorbarh(aes(x = (start + end) / 2, y = M, 
                     xmax = start, xmin = end, height = 1)) +
    geom_point(aes(x = as.numeric(substr(year, 2, 5)), y = M), size= 3 ) + 
    geom_point(aes(x = (start + end) / 2, y = M), size= 3) +
    guides(size = "none",colour=guide_legend(ncol=1)) +
    scale_x_continuous (breaks = seq(1200, 1800, by = 200)) +
    theme(legend.position="none")
  )

english_wellcome_r <- rbind(english_mortality_prep_r, wellcome_prep_r)
english_wellcome_r$data <- factor(english_wellcome_r$data, levels = unique(english_wellcome_r$data))
english_wellcome_r$start <- as.numeric(english_wellcome_r$start) 
english_wellcome_r$end <- as.numeric(english_wellcome_r$end) 

english_wellcome_plot_r <- ggplotGrob(
  ggplot(english_wellcome_r, aes(colour = data, shape = source) ) +  
    ylab("modal age (corrected for population growth)")  + 
    xlab("year (from - to)") + ylim(2, 75) +
    scale_color_manual(values=pals::cols25()) +
    geom_errorbar(aes(x = (start + end) / 2, y = M, 
                    ymin = HDIlow, ymax=  HDIhigh), width=0, colour = "dark grey") +
    geom_errorbarh(aes(x = (start + end) / 2, y = M, 
                     xmax = start, xmin = end, height = 1)) +
    geom_point(aes(x = as.numeric(substr(year, 2, 5)), y = M), size= 3 )+ 
    geom_point(aes(x = (start + end) / 2, y = M), size= 3) + 
    guides(size = "none",colour=guide_legend(ncol=1)) +
    scale_x_continuous (breaks = seq(1200, 1800, by = 200)) +
    theme(legend.position="none")
  )
ewp_legend <- get_legend(english_wellcome_plot)

# Nicht genutzter Code?
# Gompertz beta from historical and osteological data
#english_wellcome_beta <- rbind(english_mortality_beta_prep, wellcome_prep_beta)
#english_wellcome_beta$data <- factor(english_wellcome_beta$data, levels = unique(english_wellcome_beta$data))
#english_wellcome_beta$start <- as.numeric(english_wellcome_beta$start) 
#english_wellcome_beta$end <- as.numeric(english_wellcome_beta$end) 

#english_wellcome_plot_beta <- 
#  ggplot(english_wellcome_beta, aes(colour = data, shape = source) ) +  
#  ylab("Gompertz \u03B2")  + xlab("year") + #ylim(10, 70) +
#  geom_errorbar(aes(x = (start + end) / 2, y = beta, ymin = HDIlow, ymax=  HDIhigh), width=0, colour = "dark grey") +
#  geom_errorbarh(aes(x = (start + end) / 2, y = beta, xmax = start, xmin = end, height = 0), colour = "dark grey") +
#  geom_point(aes(x = as.numeric(substr(year, 2, 5)), y = beta), size= 3 )+ 
#  geom_point(aes(x = (start + end) / 2, y = beta), size= 3) + guides(size = "none")
