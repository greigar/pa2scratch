# 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (𝚏𝚒𝚙𝚜 == "𝟸𝟺𝟻𝟷𝟶") from 1999 to 2008?
# Use the base plotting system to make a plot answering this question.

library(dplyr)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

yearly_emissions_baltimore <- NEI %>%
                              filter(fips == "24510") %>%
                              group_by(year) %>%
                              summarise(total_emission = sum(Emissions))

with(yearly_emissions_baltimore, {
        plot(  year, total_emission, pch=19, col="blue", xaxt = "n", ylab = "Total Emissions for Baltimore", xlab = "Year")
        points(year, total_emission,         col="red",  xaxt = "n", type="l")
    }
)

# axis function specifies the labelling etc of the axis
#   1st argument is position - a value of 1 is bottom
axis(1, yearly_emissions_baltimore$year, yearly_emissions_baltimore$year)

