#following my exploratory analysis, I would like to explore how the data tracks specifically in red states

president_df <- read.csv('president_county_candidate.csv')
fips_df <- read.csv('FIPS.csv')

#adding FIPS labels for easy mapping
#first, we need to drop the word "county" from the county names, as otherwise the join will not work 
president_df$county <- sub(' County', '', president_df$county)

#we are only interested in counties where Donald Trump won and want to join the fips column
trump_df <- president_df %>%
  filter(candidate == 'Donald Trump' & won == "True") %>%
  left_join(fips_df)


#filtering the county_diff_means df to include only counties where Donald Trump won
red_county_means_df <- diff_means_df %>%
  filter(fips %in% trump_df$fips)

red_county_df <- county_df_no_oppose %>%
  filter(fips %in% trump_df$fips)

plot_usmap(regions = 'counties', data = red_county_means_df, values = 'worried')+
  scale_fill_distiller(type = 'div', palette = 'BrBG')

plot_usmap(regions = 'counties', data = red_county_df, values = 'worried')+
  scale_fill_distiller(type = 'seq', palette = 'PuBuGn')

#now, I want to join the FULL presidential df with the county non-mean data to run a logistic regression.
joined_df <- president_df %>%
  left_join(fips_df) %>%
  right_join(county_df_no_oppose)%>%
  select(-c('GeoType','GeoName')) %>% #filter out the duplicate columns 
  filter((candidate == 'Donald Trump' & won == 'True') | candidate == 'Joe Biden' & won == 'True' ) %>%
  #now group by president and calculate the mean and sd for the worried group
  group_by(candidate) %>%
  summarise(worried_av = mean(worried), worried_sd = sd(worried), worried_count = n())

#Logistic regression: Want to predict %worried
#Visualisation: Bar chart showing % worried in states where Trump won vs where Biden won 
ggplot(data = joined_df, mapping = aes(x = candidate, y = worried_av))+
  geom_col()+
  geom_errorbar(aes(ymin = worried_av - worried_sd, ymax = worried_av+worried_sd),width=.2,
                position=position_dodge(.9))
#Statistics: Test this difference of means with a t-test, check assumptions 
#histogram of worried data
joined_df_ungroup <- president_df %>%
  left_join(fips_df) %>%
  right_join(county_df_no_oppose)%>%
  select(-c('GeoType','GeoName')) %>% #filter out the duplicate columns 
  filter((candidate == 'Donald Trump' & won == 'True') | candidate == 'Joe Biden' & won == 'True' )

ggplot(data = joined_df_ungroup, mapping = aes(x = worried, fill = candidate))+
  geom_histogram(aes(y = ..density..), alpha = 0.5)+
  scale_fill_manual(values = c('#fb6a4a','#4292C6'), name = 'Candidate')+
  geom_density(alpha = 0.0)+
  geom_vline(xintercept = joined_df$worried_av[1], linetype = 'dashed', 
            colour = 'red', name = 'Mean Worry Trump Counties')+
  geom_vline(xintercept =joined_df$worried_av[2], linetype = 'dashed', 
             colour = 'blue', name = 'Mean Worry Biden Counties')+
  xlab('Proportion worried (%)')+
  ylab('Density')+
  theme_bw()
ggsave('histogram.png')
#t test: 
trump <- joined_df_ungroup %>%
  filter(candidate == 'Donald Trump') %>%
  select(c('worried'))
joe <- joined_df_ungroup %>%
  filter(candidate == 'Joe Biden')%>%
  select(c('worried'))

t.test(x = trump, y = joe) #statistically significant result. 

#calculating the effect size <- ich weiß nicht warum dies funktioniert nicht 
library('effectsize')
cohens_d(as.numeric(trump), as.numeric(joe))

#map only of Republican counties 

