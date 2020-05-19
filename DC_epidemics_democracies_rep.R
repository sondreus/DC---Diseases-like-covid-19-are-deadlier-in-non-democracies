
# Daily Chart - Epidemics in Democracies and Non-Democracies

pdat <- read.csv("DC_epidemics_democracies_rep.csv")

# Country-year dataset merged with data on all epidemics with at least one casualty.
# Updated comparable variables at the yearly level were not available for all countries after 2015, 
# so values from 2015 were then used. It is also possible to drop all observations after 2015, which
# results in the same conclusions.

# Columns:
# pdat$pop : Population in Thousands, from Maddison
# pdat$rgdpnapc : Real GDP pc in constant US dollars, from Maddison
# pdat$Total.deaths : Deaths, from EM-DAT
# pdat$boix_democracy : Democracy indicator, from Boix, Miller and Rosato (2015)

pdat$pop[pdat$country == "China" & pdat$year == 2020] <- 1408526 

# UPDATE NUMBER OF THOSE KILLED IN CURRENT OUTBREAK IN CHINA HERE IF DESIRED:
# UPDATE NUMBER OF THOSE KILLED IN CURRENT OUTBREAK IN CHINA HERE IF DESIRED:
# UPDATE NUMBER OF THOSE KILLED IN CURRENT OUTBREAK IN CHINA HERE IF DESIRED:

pdat$Total.deaths[pdat$country == "China" & pdat$year == 2020] <- 1869 # UPDATED: 17/02/20

# UPDATE NUMBER OF THOSE KILLED IN CURRENT OUTBREAK IN CHINA aboveIF DESIRED
# UPDATE NUMBER OF THOSE KILLED IN CURRENT OUTBREAK IN CHINA above IF DESIRED
# UPDATE NUMBER OF THOSE KILLED IN CURRENT OUTBREAK IN CHINA above IF DESIRED

# This updates the outcome variable once China has updated
pdat$dead_per_100k <- pdat$Total.deaths*100/pdat$pop 

# This adjust maddison project's estimates for inflation:
pdat$rgdpnapc_in_2020_usd_ppp <- pdat$rgdpnapc * 1.15
pdat$rgdpnapc <- pdat$rgdpnapc_in_2020_usd_ppp


# This creates the plot:
library(ggplot2)
library(scales)
ggplot(pdat[pdat$dead_per_100k > 0 & pdat$year <= 2015, ], aes(x=rgdpnapc, y=dead_per_100k, col = boix_democracy, group = boix_democracy))+
  geom_point(alpha = 0.2)+
  theme_minimal()+
  theme(legend.title = element_blank(), legend.position = "bottom")+
  geom_smooth(method = "loess")+
  xlab("GDP per capita, PPP-adjusted 2020 USD\n")+
  ggtitle("Epidemics in Democracies and Non-Democracies (1960-2020)")+ylab("Dead per 100 000 population")+
  scale_y_continuous(trans = "log10", labels = function(x) format(x, scientific = FALSE))+
  scale_x_continuous(trans = "log10")+
  geom_point(data = pdat[pdat$country == "China" & pdat$year == 2020, ], col = "black", size = 5, shape = "o")

ggsave("DC_viral_politics.pdf", width = 6.5, height = 6)

ggplot(pdat, aes(x=rgdpnapc, y=dead_per_100k, col = boix_democracy, group = boix_democracy))+
  geom_point(alpha = 0.2)+
  theme_minimal()+
  theme(legend.title = element_blank(), legend.position = "bottom")+
  geom_smooth(method = "lm")+
  xlab("GDP per capita, PPP-adjusted 2020 USD\n")+
  ggtitle("Epidemics in Democracies and Non-Democracies (1960-2020)")+ylab("Dead per 100 000 population")+
  scale_y_continuous(trans = "log10", labels = function(x) format(x, scientific = FALSE))+
  scale_x_continuous(trans = "log10")+
  geom_point(data = pdat[pdat$country == "China" & pdat$year == 2020, ], col = "black", size = 5, shape = "o")

ggsave("DC_viral_politics_lm.pdf", width = 6.5, height = 6)
ggsave("DC_viral_politics_lm.png", width = 6.5, height = 6)

# The black circle is the break-out in China at present. 

### Models of deaths per 100k population - Chart has slightly different slopes
# Main model:
summary(lm(log(dead_per_100k) ~ log(rgdpnapc) + boix_democracy, data =pdat))
# (dropping post 2015)
summary(lm(log(dead_per_100k) ~ log(rgdpnapc) + boix_democracy, data =pdat[pdat$year <= 2015, ]))

# Alternative model 1:
summary(lm(log(dead_per_100k) ~ log(rgdpnapc)+boix_democracy+year, data =pdat))

# Alternative model 2:
library(splines)
summary(lm(log(dead_per_100k) ~ bs(log(rgdpnapc))+boix_democracy+bs(year), data =pdat))

# Alternative model 3:
summary(lm(log(dead_per_100k) ~ log(rgdpnapc)+boix_democracy, data =pdat[pdat$Total.deaths > 50, ]))

### Models with death count instead of deaths per capita (using negative binomial):

# Alternative model 4:
summary(glm.nb(Total.deaths ~ log(rgdpnapc)+boix_democracy, data =pdat))
# (Dropping post 2015:)
summary(glm.nb(Total.deaths ~ log(rgdpnapc)+boix_democracy, data =pdat[pdat$year <= 2015, ]))

# Alternative model 5:
summary(glm.nb(Total.deaths ~ log(rgdpnapc)+boix_democracy+year, data =pdat))

# Alternative model 6:
summary(glm.nb(Total.deaths ~ bs(log(rgdpnapc))+boix_democracy+bs(year), data =pdat))

# Alternative model 7:
summary(glm.nb(Total.deaths ~ bs(log(rgdpnapc))+boix_democracy+bs(year)+bs(log(pop)), data =pdat))

# Alternative model 8:
summary(glm.nb(Total.deaths ~ log(rgdpnapc)+boix_democracy, data =pdat[pdat$Total.deaths > 50, ]))

