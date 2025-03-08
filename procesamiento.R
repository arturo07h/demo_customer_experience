library(googlesheets4)

# Leer datos desde Google Sheets con un enlace público
url <- "https://docs.google.com/spreadsheets/d/1QJbqyLT45yE1C0zIDx_xQFQtbyMrEHej1ISM3Merb0A/edit?gid=1133498161#gid=1133498161"
df <- read_sheet(url)

