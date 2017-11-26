

library("rvest")
url <- "https://physics.nist.gov/PhysRefData/Handbook/Tables/heliumtable2.htm"
population <- url %>%
  html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/table[1]') %>%
  html_table()