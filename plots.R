# 1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
#    Using the base plotting system, make a plot showing the total PM2.5 emission from all
#    sources for each of the years 1999, 2002, 2005, and 2008.


library(dplyr)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


yearly_emissions <- NEI %>% group_by(year) %>% summarise(total_emission = sum(Emissions))


png(filename="plot1.png", width = 480, height = 480)

with(yearly_emissions, {
        plot(  year, total_emission, pch=19, col="blue", xaxt = "n", ylab = "Total Emissions", xlab = "Year")
        points(year, total_emission,         col="red",  xaxt = "n", type="l")
    }
)

# axis function specifies the labelling etc of the axis
#   1st argument is position - a value of 1 is bottom
axis(1, yearly_emissions$year, yearly_emissions$year)

dev.off()




# 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (𝚏𝚒𝚙𝚜 == "𝟸𝟺𝟻𝟷𝟶") from 1999 to 2008?
# Use the base plotting system to make a plot answering this question.

library(dplyr)

## This first line will likely take a few seconds. Be patient!
#NEI <- readRDS("summarySCC_PM25.rds")
#SCC <- readRDS("Source_Classification_Code.rds")

yearly_emissions_baltimore <- NEI %>%
                              filter(fips == "24510") %>%
                              group_by(year) %>%
                              summarise(total_emission = sum(Emissions))

png(filename="plot2.png", width = 480, height = 480)

with(yearly_emissions_baltimore, {
        plot(  year, total_emission, pch=19, col="blue", xaxt = "n", ylab = "Total Emissions for Baltimore", xlab = "Year")
        points(year, total_emission,         col="red",  xaxt = "n", type="l")
    }
)

# axis function specifies the labelling etc of the axis
#   1st argument is position - a value of 1 is bottom
axis(1, yearly_emissions_baltimore$year, yearly_emissions_baltimore$year)

dev.off()


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
#NEI <- readRDS("summarySCC_PM25.rds")
#SCC <- readRDS("Source_Classification_Code.rds")

emissions_baltimore <- NEI %>% filter(fips == "24510")

yearly_emissions_baltimore <- emissions_baltimore %>%
                              group_by(year, type) %>%
                              summarise(total_emission = sum(Emissions))

ggplot(yearly_emissions_baltimore, aes(x = year, y = total_emission, col = factor(type) )) + geom_line()

ggsave("plot3.png")

#ggplot(emissions_baltimore, aes(x = year, y = factor(type) )) + stat_sum(aes(group=1))



# 4. Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

# NEI <- readRDS("summarySCC_PM25.rds")
# SCC <- readRDS("Source_Classification_Code.rds")

scc_coal     <- SCC[ grep("coal", SCC$EI.Sector, ignore.case = TRUE), 1 ]
nei_scc_coal <- NEI$SCC %in% scc_coal
nei_coal     <- NEI[nei_scc_coal,]

yearly_emissions_coal <- nei_coal %>% group_by(year) %>% summarise(total_emission = sum(Emissions))

ggplot(yearly_emissions_coal, aes(x = year, y = total_emission )) + geom_line()
ggsave("plot4.png")




# 5. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

# NEI <- readRDS("summarySCC_PM25.rds")
# SCC <- readRDS("Source_Classification_Code.rds")

scc_mobile_vehicles <- SCC[ grep("Mobile.*Vehicles.*", SCC$EI.Sector, ignore.case = TRUE), 1 ]



# 6. Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle
#    sources in Los Angeles County, California (𝚏𝚒𝚙𝚜 == "𝟶𝟼𝟶𝟹𝟽").
#    Which city has seen greater changes over time in motor vehicle emissions?


