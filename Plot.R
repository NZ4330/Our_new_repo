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
data <- inner_join(data ,ecb_rate %>% select(date, ecb_rate = value), by = "date" )

# Fjern NA'er
data <- na.omit(data) %>% 
	mutate(log_consumption = log(data$consumption)) %>% 
	mutate(diff_consumption = c(NA, diff(log_consumption))) %>% 
	mutate(diff_rate = c(NA, diff(rate)))



# Estimer lineær model
model <- lm(diff_consumption ~ diff_rate, data = data)
summary(model)

# Visualiser
ggplot(data, aes(x = diff_rate, y = diff_consumption)) +
	geom_point(alpha = 0.5) +
	geom_smooth(method = "lm", se = TRUE, color = "blue") +
	labs(title = "Sammenhæng mellem rente og forbrug i USA",
			 x = "1-årig rente (%)",
			 y = "Reelt personligt forbrug (milliarder USD)") +
	theme_minimal()

# Hent ECB's hovedrefinansieringsrente
ecb_rate <- fredr(series_id = "IRLTLT01EZM156N", observation_start = as.Date("2000-01-01"))

# Beregn ændringer i renten (første differens)
ecb_rate <- ecb_rate %>%
	arrange(date) %>%
	select(date, diff_ecb_rate)

# Forbered data
data <- data %>%
	# Fjern rækker med NA først
	na.omit() %>%
	mutate(log_consumption = log(consumption)) %>%

	mutate(
		diff_consumption = c(NA, diff(log_consumption)),  # log-differens ≈ vækstrate
		diff_rate = c(NA, diff(rate)) * 100,              # ændring i procentpoint
		diff_euro_rate = c(NA, diff(ecb_rate)) * 100  # instrument i pct.point
	) %>%
	
	na.omit()

# Fjern NA'er der opstår pga. lag()
data <- na.omit(data)


if (!require("AER")) install.packages("AER")
library(AER)

first_stage <- lm(diff_rate ~ diff_euro_rate, data = data)
summary(first_stage)

data <- data %>% mutate(diff_rate_lag1 = lag(diff_rate))


# IV-regression med instrument
iv_model <- ivreg(diff_consumption ~ diff_rate | diff_euro_rate, data = data)

summary(iv_model, vcov = sandwich::vcovHC)


# Opsummer resultater
summary(iv_model)

# Hent forudsagte værdier fra IV-modellen
data$predicted_consumption <- predict(iv_model)

# Indlæs ggplot2, hvis du ikke har det
if (!require("ggplot2")) install.packages("ggplot2")
library(ggplot2)

# Plot
ggplot(data, aes(x = predicted_consumption, y = diff_consumption)) +
	geom_point(alpha = 0.6) +
	geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 1) +
	labs(
		title = "Faktisk vs. Forudsagt forbrugsvækst",
		subtitle = "Forbrugsvækst som funktion af IV-modelens forudsigelser",
		x = "Forudsagt forbrugsvækst (log-differens)",
		y = "Faktisk forbrugsvækst (log-differens)"
	) +
	theme_minimal()

library(tidyr)

ggplot(data, aes(x = date)) +
	geom_line(aes(y = diff_consumption, color = "Faktisk")) +
	geom_line(aes(y = predicted_consumption, color = "Forudsagt")) +
	labs(title = "Faktisk vs. Forudsagt forbrugsvækst over tid",
			 x = "Dato", y = "Log-differens (forbrugsvækst)",
			 color = "") +
	theme_minimal()

data$residuals <- residuals(iv_model)

ggplot(data, aes(x = residuals)) +
	geom_histogram(bins = 30, fill = "gray", color = "black") +
	labs(title = "Fordeling af residualer fra IV-modellen",
			 x = "Residual", y = "Antal") +
	theme_minimal()



