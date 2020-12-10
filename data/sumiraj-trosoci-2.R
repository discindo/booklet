# (data/sumiraj-trosoci-2.R)

'Сумирај трошоци групирани по вработен и тип на трошок. 
 Табелаta со трошоци мора да ги содржи колоните: `vraboten`, `tip_na_trosok`, и `cena`.
 
 Usage:
    sumiraj-trosoci-2.R <tabela_so_trosoci>
    sumiraj-trosoci-2.R --help
    sumiraj-trosoci-2.R --version

 Options:
    --help      Прикажи помош
    --version   Прикажи верзија
    
' -> doc

# Логика за аргументи
library(docopt)
arguments <- docopt(doc, version = "Сумирај трошоци 2.0\n")

# Провери дали табелата е csv формат
assertthat::assert_that(assertthat::has_extension(arguments$tabela_so_trosoci, ext = "csv"))

# Закачи ги ти пакетите кои се користат подолу
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(assertthat)
})

# Доколку не се достапни, инсталирај со:
# install.packages("dplyr")
# install.packages("readr")
# install.packages(assertthat)

# Функција за групирање и сумирање трошоци
# Аргументот `trosoci_tabela` е патека до табелата што треба да се трансформира

sumiraj_trosoci <- function(trosoci_tabela) {

  # Вчитај ги податоците
  trosoci <- read_csv(trosoci_tabela, col_types = "ccn")
  
  assertthat::assert_that(
    inherits(trosoci, "data.frame"),
    msg = "Табелата не беше вчитана како `data.frame`.")

  assertthat::assert_that(all(c("vraboten", "tip_na_trosok", "cena") %in% names(trosoci)), 
    msg = "Табелата мора да содржи колони со имињата: 'vraboten', 'tip_na_trosok', 'cena'.")
  
  assertthat::assert_that(is.numeric(trosoci$cena), 
    msg = "Колоната `cena` мора да биде нумеричка.")
  
  
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

# Земи го првиот аргумент (табелата)
dadeni_trosoci <- arguments$tabela_so_trosoci

# Изврши ја функцијата
sumiraj_trosoci(trosoci = dadeni_trosoci)
