# Coursera EDA - Project 2
#
# Question 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008?
# Use the base plotting system to make a plot answering this question.

library(dplyr)

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Sum the emissions for Baltimore per year
yearly_emissions_baltimore <- NEI %>%
                              filter(fips == "24510") %>%
                              group_by(year) %>%
                              summarise(total_emissions = sum(Emissions))

png(filename="plot2.png", width = 480, height = 480)

with(yearly_emissions_baltimore, {

        # plot blue points
        plot(year, total_emissions, pch  = 19,
                                    col  = "blue",
                                    xaxt = "n",
                                    ylab = expression(PM[2.5]~"Emissions (tons)"),
                                    xlab = "Year")

        # plot red line
        points(year, total_emissions, col="red", xaxt = "n", type="l")

        # axis label to get the right years - a value of 1 is bottom
        axis(1, year, year)

        title(expression("Total"~PM[2.5]~"Emissions From All Sources for Baltimore"))
    }
)

dev.off()

