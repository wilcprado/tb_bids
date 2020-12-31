library(tidyverse)
library(rvest)
library(ggplot2)
library(devtools)
Sys.setlocale("LC_TIME", "English")


# Extracao Paginas --------------------------------------------------------

char_bid <- list()
char_stats <- list()
char_ref <- list()

url <- 'https://www.tibia.com/charactertrade/?subtopic=pastcharactertrades&currentpage='

for (i in 1:10) {
  webpage <- read_html(paste0(url, i))

  aux_info_char <- html_nodes(webpage, '.AuctionHeader')
  aux_info_bid <- html_nodes(webpage, '.AuctionBody')
  aux_ref <- html_nodes(webpage, '.AuctionCharacterName')

  char_bid <-
    c(char_bid, str_split(html_text(aux_info_bid), 'Auction'))

  char_stats <-
    c(char_stats, str_split(str_split(gsub(
      "char_level", "", html_text(aux_info_char)
    ), ':'), '"'))

  char_ref <-
    c(char_ref,
      aux_ref %>%
        html_children() %>%
        html_attr('href'))

}


# Breve correcao ---------------------------------------------------------

dt_link <-
  data.frame(matrix(
    unlist(char_ref),
    ncol = max(lengths(char_ref)),
    byrow = TRUE
  ))

dt_bid <-
  data.frame(matrix(
    unlist(char_bid),
    ncol = max(lengths(char_bid)),
    byrow = TRUE
  ))

dt_char <-
  data.frame(matrix(
    unlist(char_stats),
    ncol = max(lengths(char_stats)),
    byrow = TRUE
  ))


names(dt_link) <- c('ref')
names(dt_bid)  <- c('spl', 'start', 'end')
names(dt_char) <- c('_1', 'nickname', '_3','char_level','_5','vocation','_7','world','_9')


# Saida -------------------------------------------------------------------

dt_char_bid <-
  cbind(dt_char, dt_bid, dt_link) %>%
  mutate(
    char_level = as.numeric(str_replace_all(char_level, '\\| Vocation', "")),
    i_date = as.Date(str_sub(start, 8, 18), format = '%b %d %Y'),
    e_date = as.Date(str_sub(end, 6, 16), format = '%b %d %Y'),
    bid = sub(" .*", "", gsub(',','',str_sub(end, 40, 55))),

    gender = case_when(
      str_detect(vocation, "Male") ~ "Male",
      str_detect(vocation, "Female") ~ "Female",
    ),

    vocation = case_when(
      str_detect(vocation, "Paladin") ~ "Paladin",
      str_detect(vocation, "Druid") ~ "Druid",
      str_detect(vocation, "Sorcerer") ~ "Sorcerer",
      str_detect(vocation, "Knight") ~ "Knight",
      TRUE ~ "Rooker",
    ),

    situation = case_when(
      str_detect(end, "finished") ~ "Finished",
      str_detect(end, "processed") ~ "Currently processed",
      str_detect(end, "transferred") ~ "Will be transferred at the next server save",
    ),

    sold = case_when(
      str_detect(end, "Minimum") ~ "0",
      str_detect(end, "Winning") ~ "1",
    ),

    range = case_when(
      char_level >= 0   & char_level <= 100 ~ '8 - 100',
      char_level >= 101 & char_level <= 200 ~ '101 - 200',
      char_level >= 201 & char_level <= 300 ~ '201 - 300',
      char_level >= 301 & char_level <= 400 ~ '301 - 400',
      char_level >= 401 & char_level <= 500 ~ '401 - 500',
      char_level >= 501 & char_level <= 600 ~ '501 - 600',
      char_level >= 601 & char_level <= 700 ~ '601 - 700',
      char_level >= 701 & char_level <= 800 ~ '701 - 800',
      char_level >= 801 & char_level <= 900 ~ '801 - 900',
      char_level >= 901 & char_level <= 1000 ~ '901 - 1000',
      char_level >= 1001 ~ '1001+',
    ),

    ref= sub(".*auctionid=", "",str_replace(dt_link$ref,'&source=overview',''))

  ) %>%

  select(nickname,vocation,gender,char_level,
         range,world,bid,situation,sold,i_date,
         e_date,ref
  ) %>%
  write.csv2('Data/data_tibia.csv')

