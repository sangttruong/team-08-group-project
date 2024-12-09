rsdb <- xml2::read_html("http://www.rsdb.org/full") %>%
  html_node("table") %>%
  html_table()

save(rsdb, file = "data/rsdb.rda")
