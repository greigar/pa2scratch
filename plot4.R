# Coursera EDA - Project 2
#
# Question 4. Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?
#
# For a more interesting (!) plot, the total coal emissions have been broken down by EI.Sector
#

library(dplyr)
library(ggplot2)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#
# Extract all the SCC ids where coal is contained in the EI.Sector text
# e.g. "Fuel Comb - *** - Coal"
#
scc_coal <- SCC %>%
            filter( grepl("coal", EI.Sector, ignore.case = TRUE) ) %>%
            select(SCC, EI.Sector)


# Join between the Coal SCC ids and the NEI data
nei_coal <- inner_join(NEI, scc_coal, by = "SCC")


# Sum the emissions by year and EI.Sector
yearly_emissions_coal <- nei_coal %>%
                         group_by(year, EI.Sector) %>%
                         summarise(total_emissions = sum(Emissions)) %>%
                         mutate(EI.Sector = sub(" - Coal", "", EI.Sector) ) # remove coal text to make plot titles fit

#
# Plot emssions split by EI.Sector using facet_grid
#
ggplot(yearly_emissions_coal, aes(x = year, y = total_emissions )) +
  geom_line() +
  facet_grid(.~EI.Sector) +
  labs(x = "Year", y = expression(PM[2.5]~"Emissions (tons)")) +
  ggtitle(expression(PM[2.5]~"Emissions from Coal "))

ggsave("plot4.png")


