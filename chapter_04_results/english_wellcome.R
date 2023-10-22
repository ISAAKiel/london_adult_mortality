# get symbols & colors from palette alphabet (max n = 26), alt. glasbey (32), polychrome(36)
plotcolors<-palette.colors(palette = 'alphabet')
plotsymbols<-c(17,18,15)

# MOLA Welcome data without correction of population growth
english_wellcome <- rbind(english_mortality_prep, wellcome_prep)

# slight modifications
english_wellcome <- english_wellcome %>%
  mutate(data = factor(data, levels = unique(data))) %>%
  mutate(source = gsub('written','England & Wales written', source)) %>%
  mutate(source = gsub('osteological','London osteological', source)) %>%
  mutate(source = ifelse(data=="London 1728-1840","London written",source)) %>%
  mutate(source = factor(source, levels = c('England & Wales written', 'London written', 
                         'London osteological'))) %>%
  mutate(start = as.numeric(start)) %>%
  mutate(end = as.numeric(end)) %>%
  mutate(year = ifelse(is.na(year), (start + end)/2, substr(year, 2,5))) %>%
  mutate(year = as.numeric(year))

ggplot(english_wellcome, aes(x = year, y = M, colour = data, shape = source) ) + 
  ylab("modal age & HDI low - HDI high")  + 
  xlab("year (from - to)") + ylim(2, 75) +
  scale_color_manual(values=unname(plotcolors)) +
  scale_shape_manual(values=plotsymbols) +
  geom_smooth(color = "dark grey") +
  geom_errorbar(aes(ymin = HDIlow, ymax=  HDIhigh), width=0, colour = "dark grey") +
  geom_errorbarh(aes(xmax = start, xmin = end, height = 1)) +
  geom_point(size= 2 ) + 
  guides(size = "none",colour=guide_legend(ncol=1)) +
  scale_x_continuous (breaks = seq(1200, 1800, by = 200)) +
  theme(legend.position="none") -> english_wellcome_plot

# MOLA Welcome data with correction of population growth (_r)
english_wellcome_r <- rbind(english_mortality_prep_r, wellcome_prep_r)

# slight modifications
english_wellcome_r <- english_wellcome_r %>%
  mutate(data = factor(data, levels = unique(data))) %>%
  mutate(source = gsub('written','England & Wales written', source)) %>%
  mutate(source = gsub('osteological','London osteological', source)) %>%
  mutate(source = ifelse(data=="London 1728-1840","London written",source)) %>%
  mutate(source = factor(source, levels = c('England & Wales written', 'London written', 
                         'London osteological'))) %>%
  mutate(start = as.numeric(start)) %>%
  mutate(end = as.numeric(end)) %>%
  mutate(year = ifelse(is.na(year), (start + end)/2, substr(year, 2,5))) %>%
  mutate(year = as.numeric(year))

ggplot(english_wellcome_r, aes(x = year, y = M, colour = data, shape = source) ) + 
  ylab("modal age (corrected for population growth)")  + 
  xlab("year (from - to)") + ylim(2, 75) +
  scale_color_manual(values=unname(plotcolors)) +
  scale_shape_manual(values=plotsymbols) +
  geom_smooth(color = "dark grey") +
  geom_errorbar(aes(ymin = HDIlow, ymax=  HDIhigh), width=0, colour = "dark grey") +
  geom_errorbarh(aes(xmax = start, xmin = end, height = 1)) +
  geom_point(size= 2 )+ 
  guides(size = "none",colour=guide_legend(ncol=1)) +
  scale_x_continuous (breaks = seq(1200, 1800, by = 200)) -> english_wellcome_plot_r

# get the legend and remove it afterwards 
ewp_legend <- get_legend(english_wellcome_plot_r)
english_wellcome_plot_r <- english_wellcome_plot_r + theme(legend.position="none")

# build the image
grid::grid.newpage()
ewp<-plot_grid(english_wellcome_plot, english_wellcome_plot_r, ncol=1)
modal_ages_plot <- plot_grid(ewp, ewp_legend, ncol = 2, rel_widths = c(.75, .25))
#plot(modal_ages_plot)

# Save the finished map object
ggsave(
  filename = "fig06_modal_ages_plot.pdf",
  width = 11, height = 8.5,
  plot = modal_ages_plot, 
  device = "pdf",
  path = "documented"
)