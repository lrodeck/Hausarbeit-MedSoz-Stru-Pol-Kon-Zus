
# Load Libraries ----------------------------------------------------------


library(tidyverse)
library(readxl)
library(zoo)
library(forecast)

# load existing data sets -------------------------------------------------


data_fin <- readRDS("table_spiegel_data.rds")
data_sent_raw <- readRDS("table_sentiments_raw.rds")
data_flucht_sent <- readRDS("table_data_flucht_sent_sentiments_raw.rds")
data_flucht <- readRDS("table_data_flucht_sentiments_raw.rds")


# load and transoform polit bar data -----------------------------------------------------


polit_bar_prob <- read_excel("C:/Users/lrodeck/Downloads/9_Probleme_1.xlsx", 
                            skip = 6)
polit_bar_prob <- polit_bar_prob%>%
  filter(date >= '2015-07-01' & date < '2016-04-01')


dates <- data.frame(date = seq.Date(as.Date(min(polit_bar_prob$date))
                  , as.Date(max(polit_bar_prob$date))
                  , by = "day"))

polit_bar_prob_full_dates <- left_join(dates, polit_bar_prob, by = join_by(date))%>%
  mutate(migration = na.locf(migration)
         , climate = na.locf(climate)
         , pensions = na.locf(pensions)
         , social_down_mobility = na.locf(social_down_mobility))%>%
  select(-covid)

data_flucht_sent_means <- data_flucht_sent%>%
  select(upload_date, sentiment_ai, outlet)%>%
  group_by(upload_date, outlet)%>%
  mutate(sentiment_ai = median(sentiment_ai)
         , upload_date = as.Date(upload_date))%>%
  unique()%>%
  ungroup()%>%
  left_join(polit_bar_prob_full_dates, by = join_by(upload_date == date))


library(ggplot2)
library(ggpubr)
data_flucht_sent_means%>%
  spread(outlet, sentiment_ai)%>%
  ggplot() +
   aes(x = BILD, y = SPON, colour = migration) +
   geom_point(shape = "circle", size = 1.5 , add = "reg.line") +
    stat_smooth(method = 'lm', se = F, color = "black", linetype = "dashed")+
    stat_cor(label.x = 0.3, label.y = 0.6) +
    stat_regline_equation(label.x = 0.5, label.y = 0.6) +
   scale_color_gradient() +
   labs(x = "Sent. BILD", y = "Sent. Spiegel", title = "Sentiment Korrelation", 
   subtitle = "Korrelation von Spiegel Und Bild Sentiment", caption = "Jul 2015 - Mär 2016", color = "öffentl. Relevanz Migration") +
   ggthemes::theme_solarized()




library(dplyr)
library(ggplot2)

data_flucht_sent_means %>%
 filter(upload_date >= "2015-06-29" & upload_date <= "2016-03-28" & !is.na(upload_date)) %>%
 ggplot() +
 aes(x = migration, y = sentiment_ai) +
 geom_point(shape = "circle", 
 size = 1.5, colour = "#112446") +
 ggthemes::theme_solarized() +
 facet_wrap(vars(outlet))



# some random data
x <- ts(data_flucht_sent_means$upload_date[data_flucht_sent_means$outlet == 'SPON']
        , data_flucht_sent_means$sentiment_ai[data_flucht_sent_means$outlet == 'SPON'])
public_opinion = data_flucht_sent_means$migration[data_flucht_sent_means$outlet == 'SPON']

# build the model (check ?auto.arima)
model = auto.arima(x, xreg = matrix(public_opinion))

# some random predictors
mig.reg = data.frame(public_opinion = data_flucht_sent_means$migration[data_flucht_sent_means$outlet == 'SPON'])

# forecasting
forec = forecast(model, xreg = matrix(public_opinion))

# quick way to visualize things
plot(forec)

# model diagnosis
tsdiag(model)

summary(forec)


library(stargazer)
stargazer(model)
