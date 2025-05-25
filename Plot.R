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

# Hent ECB's hovedrefinansieringsrente
ecb_rate <- fredr(series_id = "IRLTLT01EZM156N", observation_start = as.Date("2000-01-01"))

# Beregn ændringer i renten (første differens)
ecb_rate <- ecb_rate %>%
	arrange(date) %>%
	select(date, rate = value)

# Forbered data
data <- inner_join(rate_data %>% select(date, rate = value),
									 consumption_data %>% select(date, consumption = value),
									 
									 by = "date")
data <- inner_join(data ,ecb_rate %>% select(date, ecb_rate = rate), by = "date" ) %>% 
	mutate(log_consumption = log(consumption))

# Fjern NA'er
data <- na.omit(data) %>% 
	mutate(diff_consumption = c(NA, diff(log_consumption))) %>% 
	mutate(diff_rate = c(NA, diff(rate))) %>% 
	mutate(diff_rate_euro = c(NA, diff(ecb_rate)))



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



# Forbered data
data <- data %>%
	# Fjern rækker med NA først
	na.omit() %>%

	mutate(
		diff_consumption = c(NA, diff(log_consumption)),  # log-differens ≈ vækstrate
		diff_rate = c(NA, diff(rate)) * 100,              # ændring i procentpoint
		diff_euro_rate = diff_rate_euro * 100  # instrument i pct.point
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

# 1. Hent kontrolvariable fra FRED
cpi <- fredr(series_id = "CPIAUCSL", observation_start = as.Date("2000-01-01"))
unemp <- fredr(series_id = "UNRATE", observation_start = as.Date("2000-01-01"))
income <- fredr(series_id = "DSPIC96", observation_start = as.Date("2000-01-01"))
oil <- fredr(series_id = "DCOILBRENTEU", observation_start = as.Date("2000-01-01"))

# 2. Klargør hver variabel
cpi <- cpi %>% select(date, cpi = value)
unemp <- unemp %>% select(date, unemployment = value)
income <- income %>% select(date, income = value)
oil <- oil %>% select(date, oil_price = value)

# 3. Saml det hele med dit eksisterende datasæt
data <- data %>%
	left_join(cpi, by = "date") %>%
	left_join(unemp, by = "date") %>%
	left_join(income, by = "date") %>%
	left_join(oil, by = "date")

# 4. Beregn differenser (hvor relevant) og rens datasættet
data <- data %>%
	mutate(
		log_consumption = log(consumption),
		diff_consumption = c(NA, diff(log_consumption)),
		diff_rate = c(NA, diff(rate)) * 100,
		diff_euro_rate = c(NA, diff(ecb_rate)) * 100,
		diff_cpi = c(NA, diff(log(cpi))) * 100,
		diff_unemployment = c(NA, diff(unemployment)),
		diff_income = c(NA, diff(log(income))) * 100,
		diff_oil = c(NA, diff(log(oil_price))) * 100
	) %>%
	na.omit()

library(AER)
iv_model_controls <- ivreg(
	diff_consumption ~ diff_rate + diff_cpi + diff_unemployment + diff_oil + diff_income |
		diff_euro_rate + diff_cpi + diff_unemployment + diff_oil + diff_income,
	data = data
)

summary(iv_model_controls, vcov = sandwich::vcovHC)

data <- data %>%
	mutate(
		L1_diff_rate = lag(diff_rate, 1),
		L2_diff_rate = lag(diff_rate, 2),
		L1_diff_euro_rate = lag(diff_euro_rate, 1),
		L2_diff_euro_rate = lag(diff_euro_rate, 2),
		
		L1_diff_cpi = lag(diff_cpi, 1),
		L1_diff_unemployment = lag(diff_unemployment, 1),
		L1_diff_oil = lag(diff_oil, 1),
		L1_diff_income = lag(diff_income, 1)
	) %>%
	na.omit()

iv_model_lags <- ivreg(
	diff_consumption ~ L1_diff_rate + L2_diff_rate + L1_diff_cpi + L1_diff_unemployment + L1_diff_oil + L1_diff_income |
		L1_diff_euro_rate + L2_diff_euro_rate + L1_diff_cpi + L1_diff_unemployment + L1_diff_oil + L1_diff_income,
	data = data
)

summary(iv_model_lags, vcov = sandwich::vcovHC)

data <- data %>%
	mutate(
		L1_diff_rate = lag(diff_rate, 1),
		L1_diff_cpi = lag(diff_cpi, 1),
		L1_diff_income = lag(diff_income, 1)
	) %>%
	na.omit()

iv_model_with_lags <- ivreg(
	diff_consumption ~ diff_rate + L1_diff_rate + diff_cpi + L1_diff_cpi +
		diff_unemployment + diff_oil +
		diff_income + L1_diff_income |
		diff_euro_rate + lag(diff_euro_rate, 1) +
		diff_cpi + L1_diff_cpi +
		diff_unemployment + diff_oil +
		diff_income + L1_diff_income,
	data = data
)

summary(iv_model_with_lags, vcov = sandwich::vcovHC)

coefs <- coef(iv_model_with_lags)
total_effect <- coefs["diff_rate"] + coefs["L1_diff_rate"]
print(total_effect)

data$after_2020 <- as.numeric(data$date >= as.Date("2020-01-01"))

iv_model_regime <- ivreg(
	diff_consumption ~ diff_rate * after_2020 + diff_cpi + diff_unemployment + diff_oil + diff_income |
		diff_euro_rate * after_2020 + diff_cpi + diff_unemployment + diff_oil + diff_income,
	data = data
)

summary(iv_model_regime, vcov = sandwich::vcovHC)


