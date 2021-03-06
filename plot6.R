# Coursera EDA - Project 2
#
# 6. Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle
#    sources in Los Angeles County, California (𝚏𝚒𝚙𝚜 == "𝟶𝟼𝟶𝟹𝟽").
#    Which city has seen greater changes over time in motor vehicle emissions?

#
# This creates a bar chart which is overlayed with values indicating the decrease/increase in emssions from 1999
# To make the comparison with 1999 clearer, the plot shows a horizontal line for the 1999 level for each city.
#

library(dplyr)
library(ggplot2)

if ( !all(c("summarySCC_PM25.rds", "Source_Classification_Code.rds") %in% dir()) ){
  stop("Erorr: missing files summarySCC_PM25.rds and Source_Classification_Code.rds")
}

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

# Find all the motor vehicles by examining the EI.Sector
scc_vehicle <- SCC %>%
               filter( grepl("Mobile.*Vehicles.*", EI.Sector, ignore.case = TRUE) ) %>%
               select(SCC)

# Find the emissions for Baltimore and LA
emissions   <- NEI %>% filter(fips == "24510" | fips == "06037")

# Change fips number to City name
cities         <- c("24510" = "Baltimore", "06037" = "Los Angeles")
emissions$City <- cities[emissions$fips]

# Join vehicle and City data sets by the SCC id
nei_vehicle <- inner_join(emissions, scc_vehicle, by = "SCC" )

# Sum emissions over year and EI.Sector
yearly_emissions_vehicle <- nei_vehicle %>%
                            group_by(City, year) %>%
                            summarise(total_emissions = sum(Emissions))


# Function to calculate the change in emissions from 1999 for Baltimore and LA
change_from_1999 <- function(x) { round(x$total_emissions - x[x$year == 1999,]$total_emissions, 1) }

# Store the changes from 1999
yearly_emissions_vehicle$cf1999 <- c(yearly_emissions_vehicle %>% filter(City == "Baltimore")   %>% change_from_1999,
                                     yearly_emissions_vehicle %>% filter(City == "Los Angeles") %>% change_from_1999)

# Convert the changes from 1999 to a more readable text label
cf1999_to_text <- function(x) {
  ifelse( x == 0, "", ifelse(x < 0, paste("down ", x), paste("up ", x)) )
}
yearly_emissions_vehicle$cf1999_text <- cf1999_to_text(yearly_emissions_vehicle$cf1999)


# Extract the 1999 value for plotting horizontal line
bav_1999 <- (yearly_emissions_vehicle %>% filter(City == "Baltimore"   & year == 1999) %>% select(total_emissions))$total_emissions
lav_1999 <- (yearly_emissions_vehicle %>% filter(City == "Los Angeles" & year == 1999) %>% select(total_emissions))$total_emissions
baseline <- data.frame(year_1999 = c(bav_1999,lav_1999), City = c("Baltimore","Los Angeles"))

#
# Creates bar chart with increases/decreases
#
ggplot(yearly_emissions_vehicle, aes(x = year, y = total_emissions, label = cf1999_text, fill = factor(year))) +
    geom_bar( stat = "identity" )  +
    scale_fill_brewer(palette = "Set1") +                                                     # set bar colors
    labs(x = "Year", y = expression(PM[2.5]~"Emissions from Motor Vehicles (tons)")) +
    guides(fill = guide_legend(title = "Year")) +                                             # set legend title
    facet_grid(.~City) +                                                                      # create plot per city
    geom_hline(data = baseline, aes(yintercept = year_1999), color = "#E41A1C") +             # draw line showing 1999, first colour from Set1
    scale_x_discrete( limits = c(1999,2002,2005,2008) )  +                                    # Set x axis years manually
    geom_text(size = 1.5, vjust = 0, show.legend = FALSE, nudge_x = -0.3)  +                    # adjust labels on bars
    ggtitle(expression(PM[2.5]~"Yearly Vehicle Emissions - Change against 1999 Levels")) +
    theme(text=element_text(size = 6) ) # small font for png

ggsave("plot6.png", width=5, height=5)

