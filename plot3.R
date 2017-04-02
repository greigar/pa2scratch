# 3. Of the four types of sources indicated by the 𝚝𝚢𝚙𝚎 (point, nonpoint, onroad, nonroad) variable,
#
#    which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?
#
#    Which have seen increases in emissions from 1999–2008?
#
#    Use the ggplot2 plotting system to make a plot answer this question.

library(dplyr)
library(ggplot2)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

emissions_baltimore <- NEI %>% filter(fips == "24510")

yearly_emissions_baltimore <- emissions_baltimore %>%
                              group_by(year, type) %>%
                              summarise(total_emission = sum(Emissions))

ggplot(yearly_emissions_baltimore, aes(x = year, y = total_emission, col = factor(type) )) + geom_line()

ggplot(emissions_baltimore, aes(x = year, y = factor(type) )) + stat_sum(aes(group=1))

