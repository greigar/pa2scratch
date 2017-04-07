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

