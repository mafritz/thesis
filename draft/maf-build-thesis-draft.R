library(pdftools)
library(here)

bookdown::render_book("index.Rmd")

thesis_bookdown_file <- here("_book/thesis.pdf")

if (file.exists(thesis_bookdown_file)) {
  total_pages <- pdf_length(thesis_bookdown_file)

  pdf_subset(
    thesis_bookdown_file,
    pages = 2:total_pages,
    output = here("removed_pages.pdf")
  )

  pdf_combine(
    c(here("draft/maf-cover-page-draft.pdf"), "removed_pages.pdf"),
    output = here("draft/maf-thesis-draft.pdf")
  )

  if (file.exists("removed_pages.pdf")) {
    file.remove("removed_pages.pdf")
  }
}
