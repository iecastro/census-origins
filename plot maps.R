

sf_dots <- read_csv("~/Desktop/DataProjects/Census Origins/Data/origin_dots.csv")
sf_dots2 <- read_csv("~/Desktop/DataProjects/Census Origins/Data/origin_dotsEUR.csv")
sf_dots3 <- read_csv("~/Desktop/DataProjects/Census Origins/Data/origin_dots3.csv")

sf_dots %>% group_by(Origin) %>% summarise(count = n()) %>% arrange(count)

LA10 <- sf_dots %>% filter(Origin %in% c("Mexico", "Peru", "Puerto.Rico","Ecuador", "Honduras",
                                         "Colombia","Cuba","El.Salvador", "Guatemala"))

LA10$ordered <- factor(LA10$Origin, levels = c("Mexico", "Peru", "Puerto.Rico","Ecuador", "Honduras",
                                              "Colombia","Cuba","El.Salvador", "Guatemala"))  

ggplot()  + geom_sf(data = states, fill = NA, color = "#7f7f7f") + 
  geom_point(data = LA10, aes(lon, lat, color = ordered), size = .5) +
  theme_minimal() +  theme(axis.text = element_blank(), legend.position = "bottom") +
  scale_color_viridis_d() + guides(colour = guide_legend(override.aes = list(size=3))) +
  ggtitle("US Hispanic/Latino Population by Country of Origin", subtitle = " 1 dot = 50 people") +
  labs(x = "", y = "",color = "", caption = "Data: US Census - 2016 5-year ACS estimates")


sf_dots2 %>% group_by(Origin) %>% summarise(count = n()) %>% arrange(count)
sf_dots2$Origins <- factor(sf_dots2$Origin, levels = c("German", "Celtic", "Irish",
                                                      "Greek", "Italian","British", "Polish"))
ggplot()  + geom_sf(data = states, fill = NA, color = "#7f7f7f") + 
  geom_point(data = sf_dots2, aes(lon, lat, color = Origins), size = .5) +
  theme_minimal() +  theme(axis.text = element_blank(), legend.position = "bottom") +
  scale_color_viridis_d() + guides(colour = guide_legend(override.aes = list(size=3))) +
  ggtitle("US Population by Single Ancestry", subtitle = " 1 dot = 100 people") +
  labs(x = "", y = "",color = "", caption = "Data: US Census - 2016 5-year ACS estimates")


ggplot()  + geom_sf(data = states, fill = NA, color = "#7f7f7f") + 
  geom_point(data = sf_dots3, aes(lon, lat, color = Race), size = .25) +
  theme_minimal() +  theme(axis.text = element_blank(), legend.position = "bottom") +
  scale_color_viridis_d(direction = -1) + guides(colour = guide_legend(override.aes = list(size=3))) +
  ggtitle("US Population by Race", subtitle = " 1 dot = 300 people") +
  labs(x = "", y = "", color = "", caption = "Data: US Census - 2016 5-year ACS estimates")

NW <- sf_dots3 %>% filter(Race != "White") 
  
ggplot()  + geom_point(data = NW, aes(lon, lat, color = Race), size = .5) +
  geom_sf(data = states, fill = NA, color = "#7f7f7f") + 
  theme_minimal() +  theme(axis.text = element_blank(), legend.position = "bottom") +
  scale_color_viridis_d() + guides(colour = guide_legend(override.aes = list(size=3))) +
  ggtitle("US Population by Non-White Race ", subtitle = "1 dot = 300 people") +
  labs(x = "", y = "", color = "", caption = "Data: US Census - 2016 5-year ACS estimates")





