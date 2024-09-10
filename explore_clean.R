#loading in libraries 
library('dplyr')
library('ggplot2')
library('tidyverse')

df <- read.csv('howe-2016-data.csv')
head(df)

summary(df)
#the data here is surprisingly clean! some NA values in the mediaweekly and mediaweeklyOppose columns, but that's all

#start by breaking the data up into state, county and CBSA level 
state_df <- df %>% filter(GeoType == 'State')
county_df <- df %>% filter(GeoType == 'County')
CBSA_df <- df %>% filter(GeoType == 'CBSA')

#as the county df is the largest (3142 observations), I'll continue the exploration with just that one 

#a corrgram is a useful way to see correlations between variables.
#we need only consider the non-Oppose columns, and the mediaweekly column as there is a lot of NA
oppose_col_vec <- c('discussOppose','CO2limitsOppose','trustclimsciSSTOppose','regulateOppose',
                    'supportRPSOppose','fundrenewablesOppose','mediaweeklyOppose','happeningOppose',
                    'humanOppose','consensusOppose','worriedOppose','personalOppose','harmUSOppose',
                    'devharmOppose','futuregenOppose','harmplantsOppose','timingOppose','mediaweekly')
county_df_no_oppose <- county_df %>%
  select(-oppose_col_vec)

#producing a corrgram 
library(corrplot)
M <- cor(select(county_df_no_oppose, -c('GeoType','GEOID','GeoName')),
         use="pairwise.complete.obs")

corrplot(M, method = "color",type = "upper", diag = TRUE)

#there may also be some geographic trends. Plot each variable on a map of US counties. 
library('usmap')

values_vector <- c('discuss','CO2limits','trustclimsciSST','regulate',
                   'supportRPS','fundrenewables','happening',
                   'human','consensus','worried','personal','harmUS',
                   'devharm','futuregen','harmplants','timing')
#plot_usmap requires FIPS codes and a fips column. Conveniently, the FIPS Code is in the GEOID column 
county_df_no_oppose <- county_df_no_oppose %>%
  rename('fips' = 'GEOID')

#plotting maps for each variable 
for (value in values_vector){
 p <-  plot_usmap(regions = 'counties', data = county_df_no_oppose, values = value)+
    scale_fill_continuous(low = 'white', high = 'blue', name = value)
 print(p)
}


#another interesting plot would be to see deviation from the US mean.

#only using numeric columns
county_df_no_oppose_numeric <- select(county_df_no_oppose, -c('GeoType','fips','GeoName'))
#calculating the means of each column
means <- colMeans(county_df_no_oppose_numeric)
#computing the difference between each value and the mean
diff_means_df <- county_df_no_oppose_numeric[]-means[col(county_df_no_oppose_numeric[])]
#adding back in the fips column
diff_means_df$fips <- county_df_no_oppose$fips

#making our maps again for each variable, this time plotting the difference from the mean rather than the pct 
for (value in values_vector){
  p <-  plot_usmap(regions = 'counties', data = diff_means_df, values = value)+
    scale_fill_distiller(type = 'div', palette = 'BrBG')
    #scale_fill_continuous(low = 'red', mid = 'white', high = 'blue', name = value)
  print(p)
}

#a corrgram of the means data could also be informative 
N <- cor(select(diff_means_df, -c('fips')),
         use="pairwise.complete.obs")
corrplot(N, method = "color",type = "upper", diag = TRUE)
#ok, turns out it's not 

#now let's make the difference from mean plot, but for the state level data. 
state_df_no_oppose <- state_df %>%
  #keeping only the non-oppose columns
  select(-oppose_col_vec) %>%
  #selecting only numeric columns
  rename('fips' = 'GEOID')

#plotting the state level map
for (value in values_vector){
  q <-  plot_usmap(regions = 'states', data = state_df_no_oppose, values = value)+
    scale_fill_continuous(low = 'white', high = 'blue', name = value)
  print(q)
}

#same workflow as for counties. 
#need to work only with numeric columns for computing means
state_df_no_oppose_numeric <- select(state_df_no_oppose, -c('GeoType','fips','GeoName'))
#computing column means 
means <- colMeans(state_df_no_oppose_numeric)
#computing the difference between each value and the mean
state_diff_means_df <- state_df_no_oppose_numeric[]-means[col(state_df_no_oppose_numeric[])]
#adding back in the fips column
state_diff_means_df$fips <- state_df_no_oppose$fips

#plotting the state level differences of means map 
for (value in values_vector){
  p <-  plot_usmap(regions = 'states', data = state_diff_means_df, values = value)+
    scale_fill_distiller(type = 'div', palette = 'BrBG')
  #scale_fill_continuous(low = 'red', mid = 'white', high = 'blue', name = value)
  print(p)
}
  
  
  
  
