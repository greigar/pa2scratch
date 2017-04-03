# 1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
#    Using the base plotting system, make a plot showing the total PM2.5 emission from all
#    sources for each of the years 1999, 2002, 2005, and 2008.


library(dplyr)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


yearly_emissions <- NEI %>% group_by(year) %>% summarise(total_emission = sum(Emissions))

with(yearly_emissions, {
        plot(  year, total_emission, pch=19, col="blue", xaxt = "n", ylab = "Total Emissions", xlab = "Year")
        points(year, total_emission,         col="red",  xaxt = "n", type="l")
    }
)

# axis function specifies the labelling etc of the axis
#   1st argument is position - a value of 1 is bottom
axis(1, yearly_emissions$year, yearly_emissions$year)



