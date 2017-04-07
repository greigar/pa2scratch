# 1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
#    Using the base plotting system, make a plot showing the total PM2.5 emission from all
#    sources for each of the years 1999, 2002, 2005, and 2008.


library(dplyr)

## This first line will likely take a few seconds. Be patient!
#NEI <- readRDS("summarySCC_PM25.rds")
#SCC <- readRDS("Source_Classification_Code.rds")


yearly_emissions <- NEI %>% group_by(year) %>% summarise(total_emissions = sum(Emissions))


png(filename="plot1.png", width = 480, height = 480)

with(yearly_emissions, {

        plot(  year, total_emissions, pch=19, col="blue", xaxt = "n", ylab = expression(PM[2.5]~"Emissions (tons)"), xlab = "Year")
        points(year, total_emissions,         col="red",  xaxt = "n", type="l")

        # axis function specifies the labelling etc of the axis
        #   1st argument is position - a value of 1 is bottom
        axis(1, year, year)

        title(expression("Total"~PM[2.5]~"Emissions From All Sources"))
    }
)

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
                              summarise(total_emissions = sum(Emissions))

png(filename="plot2.png", width = 480, height = 480)

with(yearly_emissions_baltimore, {

        plot(  year, total_emissions, pch=19, col="blue", xaxt = "n", ylab = expression(PM[2.5]~"Emissions (tons)"), xlab = "Year")
        points(year, total_emissions,         col="red",  xaxt = "n", type="l")

        # axis function specifies the labelling etc of the axis
        #   1st argument is position - a value of 1 is bottom
        axis(1, year, year)

        title(expression("Total"~PM[2.5]~"Emissions From All Sources for Baltimore"))
    }
)

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
                              summarise(total_emissions = sum(Emissions))

ggplot(yearly_emissions_baltimore, aes(x = year, y = total_emissions, colour = factor(type) )) +
  geom_line() +
  labs(x = "Year", y = expression(PM[2.5]~"Emissions (tons)")) +
  guides(colour = guide_legend(title = "Source Type")) +
  ggtitle(expression(PM[2.5]~"Emissions for Baltimore by Source Type"))

ggsave("plot3.png")

#ggplot(emissions_baltimore, aes(x = year, y = factor(type) )) + stat_sum(aes(group=1))



# 4. Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

# NEI <- readRDS("summarySCC_PM25.rds")
# SCC <- readRDS("Source_Classification_Code.rds")

scc_coal <- SCC %>% filter( grepl("coal", EI.Sector, ignore.case = TRUE) ) %>% select(SCC, EI.Sector)
nei_coal <- inner_join(NEI, scc_coal, by = "SCC")

yearly_emissions_coal <- nei_coal %>%
                         group_by(year, EI.Sector) %>%
                         summarise(total_emissions = sum(Emissions)) %>%
                         mutate(EI.Sector = sub(" - Coal", "", EI.Sector) ) # remove coal text to make plot titles fit

ggplot(yearly_emissions_coal, aes(x = year, y = total_emissions )) +
  geom_line() +
  facet_grid(.~EI.Sector) +
  labs(x = "Year", y = expression(PM[2.5]~"Emissions (tons)")) +
  ggtitle(expression(PM[2.5]~"Emissions from Coal "))

ggsave("plot4.png")




# 5. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

# NEI <- readRDS("summarySCC_PM25.rds")
# SCC <- readRDS("Source_Classification_Code.rds")

scc_vehicle           <- SCC %>% filter( grepl("Mobile.*Vehicles.*", EI.Sector, ignore.case = TRUE) )  %>% select(SCC, EI.Sector)
emissions_baltimore   <- NEI %>% filter(fips == "24510")
baltimore_nei_vehicle <- inner_join(emissions_baltimore, scc_vehicle, by = "SCC" )

yearly_baltimore_emissions_vehicle <- baltimore_nei_vehicle %>%
                                      group_by(year, EI.Sector) %>%
                                      summarise(total_emissions = sum(Emissions)) %>%
                                      mutate(EI.Sector = sub("Mobile - On-Road ", "", EI.Sector) ) # make plot titles fit

ggplot(yearly_baltimore_emissions_vehicle, aes(x = year, y = total_emissions )) +
  geom_line() +
  facet_grid(.~EI.Sector) +
  labs(x = "Year", y = expression(PM[2.5]~"Emissions (tons)")) +
  ggtitle(expression(PM[2.5]~"Emissions from Motor Vehicles "))

ggsave("plot5.png")


# --------------------------------------------------------------------------------

# 6. Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle
#    sources in Los Angeles County, California (𝚏𝚒𝚙𝚜 == "𝟶𝟼𝟶𝟹𝟽").
#    Which city has seen greater changes over time in motor vehicle emissions?


# NEI <- readRDS("summarySCC_PM25.rds")
# SCC <- readRDS("Source_Classification_Code.rds")
#

scc_vehicle <- SCC %>% filter( grepl("Mobile.*Vehicles.*", EI.Sector, ignore.case = TRUE) ) %>% select(SCC)
emissions   <- NEI %>% filter(fips == "24510" | fips == "06037")

cities <- c("24510" = "Baltimore", "06037" = "LA")
emissions$City <- cities[emissions$fips]
nei_vehicle <- inner_join(emissions, scc_vehicle, by = "SCC" )
yearly_emissions_vehicle <- nei_vehicle %>% group_by(year, City) %>% summarise(total_emissions = sum(Emissions))


#
# LINE
#
ggplot(yearly_emissions_vehicle, aes(x = year, y = total_emissions, colour = City)) +
  geom_line() +
  labs(x = "Year", y = expression(Total~PM[2.5]~Emissions))

ggsave("plot6-line-1.png")

ggplot(yearly_emissions_vehicle, aes(x = year, y = total_emissions) ) + facet_grid(.~City) + geom_line()
ggsave("plot6-line-2.png")


#
# LAG
#

yearly_emissions_vehicle %>% filter(City == "Baltimore") -> baltimore
yearly_emissions_vehicle %>% filter(City == "LA")        -> la

y <- baltimore$total_emissions
bm_lag <- na.exclude(y - lag(y))

y <- la$total_emissions
la_lag <- na.exclude(y - lag(y))

lagged <- as.data.frame( cbind(bm_lag, la_lag) )

ggplot(lagged, aes(x = c(2002,2005,2008), y = bm_lag, color = "red") ) + geom_line() + geom_line(aes(y = la_lag), color = "blue")
ggsave("plot6-lag.png")

#
# BAR
#

nei_vehicle %>% filter(City == "Baltimore") -> bav
nei_vehicle %>% filter(City == "LA")        -> lav

ggplot(lav, aes(x = year, y = Emissions)) + geom_bar( stat = "identity" )
ggsave("plot6-bar-1.png")

# head( ggplot_build(g)$data[[1]] ) # get colors from ggplot, g is the ggplot

bav_1999 <- bav %>% filter(year == 1999) %>% select(Emissions) %>% sum
lav_1999 <- lav %>% filter(year == 1999) %>% select(Emissions) %>% sum

baseline <- data.frame(year_1999 = c(bav_1999,lav_1999), City = c("Baltimore","LA"))

ggplot(nei_vehicle, aes(x = year, y = Emissions, label = y, fill = factor(year))) +
  geom_bar( stat = "identity" )  +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Year", y = "Emissions") +
  guides(fill = guide_legend(title = "Year")) +
  facet_grid(.~City) +
  geom_hline(data = baseline, aes(yintercept = year_1999), color = "#E41A1C") + # first colour from Set1
  scale_x_discrete( limits = c(1999,2002,2005,2008) )
ggsave("plot6-bar-2.png")

# use yearly_emissions
ggplot(nei_vehicle, aes(x = year, y = Emissions, label = Emissions, fill = factor(year))) +
  geom_bar( stat = "identity" )  +
  geom_text(position = position_dodge(0.9))
ggsave("plot6-bar-3.png")


yearly_emissions_vehicle <- nei_vehicle %>% group_by(City,year) %>% summarise(total_emissions = sum(Emissions))

change_from_1999 <- function(x) { round(x$total_emissions - x[x$year == 1999,]$total_emissions, 2) }

yearly_emissions_vehicle$cf1999 <- c( yearly_emissions_vehicle %>% filter(City == "Baltimore")  %>% change_from_1999,
                                     yearly_emissions_vehicle %>% filter(City == "LA")         %>% change_from_1999 )

ggplot(yearly_emissions_vehicle, aes(x = year, y = total_emissions, label = cf1999, fill = factor(year))) +
  geom_bar( stat = "identity" )  +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Year", y = "Emissions") +
  guides(fill = guide_legend(title = "Year")) +
  facet_grid(.~City) +
  geom_hline(data = baseline, aes(yintercept = year_1999), color = "#E41A1C") + # first colour from Set1
  scale_x_discrete( limits = c(1999,2002,2005,2008) )  +
  geom_text(size = 3, vjust = 0,  aes(colour = cf1999 <= 0 ) , nudge_x = -0.1 )  +
  ggtitle("PM[2.5] Yearly Emissions - Showing change against 1999 Levels")

ggsave("plot6-bar-4.png")



