#! /usr/bin/Rscript

# Употреба:
# Вчитај ја оваа скипта во R за да ја користиш функцијата `sumiraj_trosoci`  

# Закачи ги ти пакетите кои се користат подолу 
library(dplyr)
library(readr)

# Доколку не се достапни, инсталирај со:
# install.packages("dplyr")
# install.packages("readr")

# Функција за групирање и сумирање трошоци
# Аргументот `trosoci` е патека до табелата што треба да се трансформира

sumiraj_trosoci <- function(trosoci_tabela) {
  
  # Вчитај ги податоците
  trosoci <- read_csv(trosoci_tabela)
  
  # Групирај и пресметај суми
  trosoci_sumirani <- trosoci %>%
    group_by(vraboten, tip_na_trosok) %>%
    summarise_at("cena", "sum") %>% 
  arrange(vraboten, tip_na_trosok)
  
  # Направи патека за дестинација
  folder_name <- dirname(trosoci_tabela)
  base_name <- tools::file_path_sans_ext(basename(trosoci_tabela))
  new_name <- paste(base_name, "sumirani.csv", sep="-")
  destinacija <- file.path(folder_name, new_name)
  
  # Зачувај
  write_csv(trosoci_sumirani, path = destinacija)
}

# Земи го првиот аргумент
dadeni_trosoci <- commandArgs(trailingOnly=TRUE)[[1]]

# Изврши ја функцијата
sumiraj_trosoci(trosoci = dadeni_trosoci)
