#the texas race df shows the percentage of each county that is non-white. 
texas_race_df <- read.csv('cc-est2019-alldata-48.csv') %>%
  #we only care about the 2019 estimate for all ages
  filter(YEAR == 12, AGEGRP == 0)
  
#now we want to create a new column, PCT_NWA, which is the percentage of people who are not white 
texas_race_df$PCT_NWA <- (texas_race_df$TOT_POP - texas_race_df$WA_MALE - texas_race_df$WA_FEMALE)/texas_race_df$TOT_POP
texas_race_df$PCT_HS <- (texas_race_df$H_MALE + texas_race_df$H_FEMALE)/texas_race_df$TOT_POP

#selecting only county and PCT_NWA, joining to climate change awareness data
#first need to remove " County" so that we can do the join along the county names columns 
texas_race_df$county <- sub(' County', '', texas_race_df$CTYNAME)
texas_race_df %>%
  rename('state' = 'STNAME')

#performing the joins and selecting only relevant columns
texas_race_df <- texas_race_df %>%
  left_join(filter(fips_df, state == 'Texas')) %>% #adding fips so we can join with fips codes
  left_join(county_df_no_oppose) %>% #joining in the climate change perceptions data on the fips codes
  select(c('fips', 'county','worried','PCT_NWA', 'PCT_HS'))#selecting only columns of interest
  
#visualisation: scatter plot showing PCT_NWA and worried 
ggplot(data = texas_race_df, mapping = aes(x = PCT_NWA, y = worried))+
  geom_point()

