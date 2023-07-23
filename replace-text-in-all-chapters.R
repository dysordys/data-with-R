library(tidyverse)

changePipe <- \(file) {
  read_lines(file) |>
    str_replace_all("%>%", "|>") |>
    write_lines(file)
}

changePlaceholder <- \(file) {
  read_lines(file) |>
    str_replace_all("= \\.", "= _") |>
    write_lines(file)
}

tibble(file = Sys.glob("*.qmd")) |>
  mutate(content = map(file, read_lines)) |>
  mutate(dot = map(content, \(x) x |> str_c(collapse = "\n") |> str_detect("dot"))) |>
  filter(dot == TRUE)
  #walk(changePipe)
