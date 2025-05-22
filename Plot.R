# Installer nødvendige pakker (hvis du ikke har dem)
if (!require("fredr")) install.packages("fredr")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")

library(fredr)
library(ggplot2)
library(dplyr)

# Sæt din FRED API-nøgle (fås gratis på https://fred.stlouisfed.org/)
fredr_set_key("ea90d424181505bd4afbd1f54bece638")

# Hent 1-årig amerikansk rente (GS1) og forbrug (PCEC96)
rate_data <- fredr(series_id = "GS1", observation_start = as.Date("2000-01-01"))
consumption_data <- fredr(series_id = "PCEC96", observation_start = as.Date("2000-01-01"))

# Forbered data
data <- inner_join(rate_data %>% select(date, rate = value),
									 consumption_data %>% select(date, consumption = value),
									 by = "date")

# Fjern NA'er
data <- na.omit(data)

# Estimer lineær model
model <- lm(consumption ~ rate, data = data)
summary(model)

# Visualiser
ggplot(data, aes(x = rate, y = consumption)) +
	geom_point(alpha = 0.5) +
	geom_smooth(method = "lm", se = TRUE, color = "blue") +
	labs(title = "Sammenhæng mellem rente og forbrug i USA",
			 x = "1-årig rente (%)",
			 y = "Reelt personligt forbrug (milliarder USD)") +
	theme_minimal()
