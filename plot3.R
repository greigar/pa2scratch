# Coursera EDA - Project 2
#
# Quesion 3. Of the four types of sources indicated by the 𝚝𝚢𝚙𝚎 (point, nonpoint, onroad, nonroad) variable,
#
#    which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?
#
#    Which have seen increases in emissions from 1999–2008?
#
#    Use the ggplot2 plotting system to make a plot answer this question.

library(dplyr)
library(ggplot2)

if ( !all(c("summarySCC_PM25.rds", "Source_Classification_Code.rds") %in% dir()) ){
  stop("Erorr: missing files summarySCC_PM25.rds and Source_Classification_Code.rds")
}

## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Filter out the emissions for Baltimore only
emissions_baltimore <- NEI %>%
                       filter(fips == "24510")


# Sum emissions over the year and source type
yearly_emissions_baltimore <- emissions_baltimore %>%
                              group_by(year, type) %>%
                              summarise(total_emissions = sum(Emissions))

ggplot(yearly_emissions_baltimore, aes(x = year, y = total_emissions, colour = factor(type) )) +
    geom_line() +
    labs(x = "Year", y = expression(PM[2.5]~"Emissions (tons)")) +
    guides(colour = guide_legend(title = "Source Type")) +
    ggtitle(expression(PM[2.5]~"Emissions for Baltimore by Source Type")) +
    theme(text=element_text(size = 7) ) # small font for png

ggsave("plot3.png", width=5, height=5)

