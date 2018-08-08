library(tidyverse)
library(tidycensus)
library(sf)



options(tigris_class = "sf", tigris_use_cache = TRUE)
Sys.getenv("CENSUS_API_KEY")


### Hispanic or Latino by Origin

## Central.America = "B03001_008E", South.America = "B03001_016E"

vars <- c(Mexico = "B03001_004E", Puerto.Rico = "B03001_005E",
Cuba = "B03001_006E", Dominican.Rebublic = "B03001_007E",
Costa.Rica = "B03001_009E", Guatemala = "B03001_010E",
Honduras = "B03001_011E", Nicaragua = "B03001_012E", Panama = "B03001_013E",
El.Salvador = "B03001_014E", Argentina = "B03001_017E", Bolivia = "B03001_018E",
Chile = "B03001_019E", Colombia = "B03001_020E", Ecuador = "B03001_021E",
Paraguay = "B03001_022E", Peru = "B03001_023E", Uruguay = "B03001_024E",
Venezuela = "B03001_025E")

#### People reporting single ancestry - European

vars2 <- c(British = "B04004_023E", Celtic = "B04004_028E", German = "B04004_042E",
Greek = "B04004_044E", Irish = "B04004_049E", Italian = "B04004_051E",
Polish = "B04004_061E")


#### People by race/ethnicity

vars3 <- c(White = "B02001_002E",
          Black = "B02001_003E",
          AmericanIndian.NativeAlaskan = "B02001_004E",
          Asian = "B02001_005E",
          NativeHawaiian.PacificIslander = "B02001_006E",
          TwoRaces = "B02001_008E")

############

states <- get_acs(geography = "state", variables = "B03001_016E", geometry = TRUE,
                  shift_geo = TRUE)

##########


HispOrig <- get_acs(geography = "county", variables = vars, output = "wide",
                    geometry = TRUE,shift_geo = TRUE)  %>%
  select(Mexico, Puerto.Rico,Cuba, Dominican.Rebublic,Costa.Rica, Guatemala,
         Honduras, Nicaragua , Panama ,El.Salvador, Argentina , Bolivia, 
         Chile, Colombia,Ecuador,Paraguay , Peru , Uruguay ,Venezuela)
  
  
###### reference  
## https://www.cultureofinsight.com/blog/2018/05/02/2018-04-08-multivariate-dot-density-maps-in-r-with-sf-ggplot2/


##### generate dots 
random_round <- function(x) {
  v=as.integer(x)
  r=x-v
  test=runif(length(r), 0.0, 1.0)
  add=rep(as.integer(0),length(r))
  add[r>test] <- as.integer(1)
  value=v+add
  ifelse(is.na(value) | value<0,0,value)
  return(value)
}

# data frame of number of dots to plot 
num_dots <- as.data.frame(HispOrig) %>% 
  select(Mexico:Venezuela) %>% 
  mutate_all(funs(. / 50)) %>% 
  mutate_all(random_round)

# generates data frame with coordinates for each point 
sf_dots <- map_df(names(num_dots), 
                  ~ st_sample(HispOrig, size = num_dots[,.x], type = "random") %>% # generate the points in each polygon
                    st_cast("POINT") %>%                                          # cast the geom set as 'POINT' data
                    st_coordinates() %>%                                          # pull out coordinates into a matrix
                    as_tibble() %>%                                               # convert to tibble
                    setNames(c("lon","lat")) %>%                                  # set column names
                    mutate(Origin = .x)                                            # add categorical party variable
) %>% 
  slice(sample(1:n())) # once map_df binds rows randomise order to avoid bias in plotting order

head(sf_dots)
  
write_excel_csv(sf_dots,"origin_dots.csv")

#######

ggplot(sf_dots, aes(lon, lat, color = Origin))  +
  geom_point(size = .5, alpha = .01) +
  theme_minimal() +  theme(axis.text = element_blank(), legend.position = "bottom") +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Hispanic/Latino Population by Country of Origin", subtitle = " 1 dot = 50 people") +
  labs(caption = "Data: US Census - 2016 5-year ACS estimates")
  

##################

EurAncest <- get_acs(geography = "county", variables = vars2, output = "wide",
                     geometry = TRUE,shift_geo = TRUE) %>%
  select(British, Celtic,German,Greek, Irish , Italian,Polish)


# data frame of number of dots to plot 
num_dots <- as.data.frame(EurAncest) %>% 
  select(British:Polish) %>% 
  mutate_all(funs(. / 100)) %>% 
  mutate_all(random_round)

# generates data frame with coordinates for each point 
sf_dots2 <- map_df(names(num_dots), 
                  ~ st_sample(HispOrig, size = num_dots[,.x], type = "random") %>% # generate the points in each polygon
                    st_cast("POINT") %>%                                          # cast the geom set as 'POINT' data
                    st_coordinates() %>%                                          # pull out coordinates into a matrix
                    as_tibble() %>%                                               # convert to tibble
                    setNames(c("lon","lat")) %>%                                  # set column names
                    mutate(Origin = .x)                                            # add categorical party variable
) %>% 
  slice(sample(1:n())) 


write_excel_csv(sf_dots2,"origin_dotsEUR.csv")

#######

race <- get_acs(geography = "county", variables = vars3, output = "wide",
                geometry = TRUE,shift_geo = TRUE) %>%
  select(White,Black, AmericanIndian.NativeAlaskan,
         Asian,NativeHawaiian.PacificIslander, TwoRaces)

random_round <- function(x) {
  v=as.integer(x)
  r=x-v
  test=runif(length(r), 0.0, 1.0)
  add=rep(as.integer(0),length(r))
  add[r>test] <- as.integer(1)
  value=v+add
  ifelse(is.na(value) | value<0,0,value)
  return(value)
}

# data frame of number of dots to plot 
num_dots <- as.data.frame(race) %>% 
  select(White:TwoRaces) %>% 
  mutate_all(funs(. / 300)) %>% 
  mutate_all(random_round)

# generates data frame with coordinates for each point 
sf_dots3 <- map_df(names(num_dots), 
                  ~ st_sample(race, size = num_dots[,.x], type = "random") %>% # generate the points in each polygon
                    st_cast("POINT") %>%                                          # cast the geom set as 'POINT' data
                    st_coordinates() %>%                                          # pull out coordinates into a matrix
                    as_tibble() %>%                                               # convert to tibble
                    setNames(c("lon","lat")) %>%                                  # set column names
                    mutate(Race = .x)                                            # add categorical party variable
) %>% 
  slice(sample(1:n())) # once map_df binds rows randomise order to avoid bias in plotting order


## write_excel_csv(sf_dots3,"origin_dots3.csv")


