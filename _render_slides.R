# Setup HTML slides deployment -------------------------------------------------

## Load packages ----
library(xaringan)


## Render slides to HTML ----
rmarkdown::render(input = "index.Rmd")


## Render slides to PDF ----
pagedown::chrome_print(input = "index.html", output = "devNutrition.pdf")


## Create directory for slide outputs ----
output_dir <- "docs"
dir.create(output_dir, showWarnings = FALSE)


## Copy needed directories and files to output directory ----
from <- c(
  "data", "images", "xaringan-themer.css", "libs",
  "index_files", "index.html", "devNutrition.pdf"
)

file.copy(
  from = from,
  to = output_dir,
  recursive = TRUE
)


## Clean up
files_to_remove <- c(
  "index.html", "devNutrition.pdf",
  "libs", "index_files"
  # list.files("index_files", full.names = TRUE, recursive = TRUE),
  # list.files("libs", full.names = TRUE, recursive = TRUE)
)

unlink(files_to_remove, recursive = TRUE)
