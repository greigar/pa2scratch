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

scc_coal     <- SCC %>% filter( grepl("coal", EI.Sector, ignore.case = TRUE) ) %>% select(SCC)

nei_scc_coal <- NEI$SCC %in% scc_coal$SCC
nei_coal     <- NEI[nei_scc_coal,]

yearly_emissions_coal <- nei_coal %>% group_by(year) %>% summarise(total_emission = sum(Emissions))

ggplot(yearly_emissions_coal, aes(x = year, y = total_emission )) + geom_line()
ggsave("plot4.png")




# 5. How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

# NEI <- readRDS("summarySCC_PM25.rds")
# SCC <- readRDS("Source_Classification_Code.rds")

scc_vehicle  <- SCC %>% filter( grepl("Mobile.*Vehicles.*", EI.Sector, ignore.case = TRUE) ) %>% select(SCC)

emissions_baltimore <- NEI %>% filter(fips == "24510")

baltimore_nei_scc_vehicle <- emissions_baltimore$SCC %in% scc_vehicle$SCC
baltimore_nei_vehicle     <- emissions_baltimore[baltimore_nei_scc_vehicle,]

yearly_baltimore_emissions_vehicle <- baltimore_nei_vehicle %>% group_by(year) %>% summarise(total_emission = sum(Emissions))

ggplot(yearly_baltimore_emissions_vehicle, aes(x = year, y = total_emission )) + geom_line()
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

#
# LINE
#

yearly_emissions_vehicle <- nei_vehicle %>% group_by(year, City) %>% summarise(total_emissions = sum(Emissions))

ggplot(yearly_emissions_vehicle, aes(x = year, y = total_emissions, colour = City)) +
  geom_line() +
  labs(x = "Year", y = expression(Total~PM[2.5]~Emissions))

ggsave("plot6a.png")

ggplot(yearly_emissions_vehicle, aes(x = year, y = total_emissions) ) + facet_grid(.~City) + geom_line()
ggsave("plot6b.png")


#
# LAG
#

yearly_emissions_vehicle %>% filter(City == "Baltimore") -> baltimore
yearly_emissions_vehicle %>% filter(City == "LA")        -> la

y <- baltimore$total_emissions
y - lag(y)

y1 <- c( 342.9102,   327.3847 , -500.0939 )
y2 <- c(  -212.511179 ,  -3.878441 , -42.154922 )

y3 <- as.data.frame( cbind(y1,y2) )

ggplot(y3, aes(x = c(2002,2005,2008), y = y1, color = "red") ) + geom_line() + geom_line(aes(y = y2), color = "blue")

# BOX -> too squished
ggplot(bav, aes(x = year, y = Emissions)) + geom_boxplot(aes(group=year)) + ylim(0,5)

# BAR
nei_vehicle %>% filter(City == "Baltimore") -> bav
nei_vehicle %>% filter(City == "LA") -> lav
ggplot(bav, aes(x = year, y = Emissions)) + geom_bar( stat = "identity" ) + geom_line(aes(mean(Emmissions)))
ggplot(lav, aes(x = year, y = Emissions)) + geom_bar( stat = "identity" )

head( ggplot_build(g)$data[[1]] ) # get colors from ggplot, g is the ggplot

ggplot(bav, aes(x = year, y = Emissions, fill = factor(year))) + geom_bar( stat = "identity" )  +
  scale_fill_brewer(palette = "Set1") +
  geom_hline(yintercept=bav_1999, color = "#E41A1C") # first colour from Set1


