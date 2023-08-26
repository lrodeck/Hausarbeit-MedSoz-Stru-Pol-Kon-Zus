# load packages
library(tidyverse)
library(udpipe)
library(flextable)
library(here)
library(progress)
# activate klippy for copy-to-clipboard button
klippy::klippy()

# load text
text_1 <- readRDS("data_full.rds")%>%select(content)%>%sample_n(1)
# clean data
text_1 <- text_1 %>%
  str_squish()

# download language model
# m_ger   <- udpipe::udpipe_download_model(language = "german-gsd")

# load language model from your computer after you have downloaded it once
m_ger <- udpipe_load_model(file = "C:/Users/lrodeck/OneDrive - M365 Universität Hamburg/Desktop/Zur medialen und sozialen Strukturierung Politischen Konflikts und Zusammenhalts/german-gsd-ud-2.5-191206.udpipe")

# tokenise, tag, dependency parsing
text_anndf <- udpipe::udpipe_annotate(m_ger, x = text_1) %>%
  as.data.frame() %>%
  dplyr::select(-sentence)

POS_ANN_DF <- text_anndf[0,]
# load text
text <- readRDS("data_full.rds")
n <- nrow(text)
pb <- progress_bar$new(total = n)
for (i in text$urls) {
  print(i)

  text_selected <- text %>%
    filter(urls == i)%>%
    select(content)%>%
    str_squish()
  
  # tokenise, tag, dependency parsing
  text_anndf <- udpipe::udpipe_annotate(m_ger
                                        , x = text_selected
                                        , doc_id = paste(i)) %>%
    as.data.frame() %>%
    dplyr::select(-sentence)
  POS_ANN_DF <- rbind(POS_ANN_DF,text_anndf)
  
  pb$tick()
  Sys.sleep(1 / n)
}

POS_ANN_DF%>%
  select(doc_id)%>%
  unique()%>%
  count()

saveRDS(POS_ANN_DF, "POS_DATA.rds")

