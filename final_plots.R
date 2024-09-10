#deviation from mean, all US counties 
counties_deviation_df <- diff_means_df %>%
  select(c('worried','fips')) 

counties_deviation_df$z_worried <- counties_deviation_df$worried/sd(counties_deviation_df$worried)

library('colorspace')

plot_usmap(regions = 'counties', data = counties_deviation_df, values = 'z_worried')+
  scale_fill_continuous_divergingx(palette = 'BrBG', mid = 0, name = 'Z Score')
ggsave('US Worry Deviation.png')

#voting as a predictor 
#a) map of voting trends by county 
plot_usmap(data = joined_df_ungroup, values = 'candidate')+
  scale_fill_brewer(palette = 'Set1', name = 'Candidate Elected')
ggsave('US Votes County.png')

#b) bar chart 
ggplot(data = joined_df, mapping = aes(x = candidate, y = worried_av))+
  geom_col(width = 0.6,fill = '#003C30')+
  geom_errorbar(aes(ymin = worried_av - worried_sd, ymax = worried_av+worried_sd),width=.2,
                position=position_dodge(.9))+
  xlab('Candidate')+
  ylab('Mean worry (%)')+
  theme_bw()
ggsave('Worry by Candidate.png')

#only Republican counties map 
rep_counties_map_df <- red_county_means_df %>% 
  select(c('worried','fips'))

rep_counties_map_df$z_worried <- rep_counties_map_df$worried/sd(rep_counties_map_df$worried)

#this should be a Z score 
plot_usmap(regions = 'counties', data = rep_counties_map_df, values = 'z_worried')+
  scale_fill_continuous_divergingx(palette = 'BrBG', mid = 0, name = 'Z Score')
ggsave('Republican Outliers.png')

plot_usmap(regions = 'counties', include = "Texas", data = rep_counties_map_df, values = 'z_worried')+
  scale_fill_continuous_divergingx(palette = 'BrBG', mid = 0, name = 'Z Score')
ggsave('Texas Republican Outliers.png')

#visualisation: texas map 
plot_usmap(data = texas_race_df,regions = 'counties', include = 'Texas', values = 'PCT_HS')+
  scale_fill_distiller(type = 'seq', direction = 1, palette = 'Blues', 
                        name = 'Proportion of hispanic residents (%)')
ggsave('Texas Hispanics.png')

#scatterplot 
#linear regression for the line 
linreg <- lm(texas_race_df$worried~texas_race_df$PCT_HS)
linreg

ggplot(data = texas_race_df, mapping = aes(x = PCT_HS, y = worried))+
  geom_point(colour = '#4292C6')+
  geom_abline(intercept = linreg$coefficients[1], slope = linreg$coefficients[2], linetype = 2)+
  xlab('Proportion of Hispanic Residents (%)')+
  ylab('Proportion worried (%)')+
  theme_bw()
ggsave('HSworry scatter.png')
cor.test(texas_race_df$PCT_HS, texas_race_df$worried)
