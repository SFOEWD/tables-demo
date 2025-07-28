library(gt)
library(gtExtras)
library(gtsummary)
library(tidyverse)

# Film Locations in San Francisco
# Cf. https://data.sfgov.org/Culture-and-Recreation/Film-Locations-in-San-Francisco/yitu-d5am/about_data
films <- read_csv("https://data.sfgov.org/resource/yitu-d5am.csv?$limit=999999")

set.seed(2)
# Get a sample of 12 films by four production companies
# with three actors listed
my_films <- films |> 
  select(1:11, -fun_facts, -locations, -distributor) |>
  filter(if_all(starts_with("actor"), \(x) !is.na(x))) |> 
  distinct() |> 
  mutate(gross = sample(100000:999999, size = n())) |> 
  group_by(production_company) |> 
  filter(n() >= 3) |> 
  slice_sample(n = 3) |> 
  ungroup() |> 
  arrange(production_company, release_year) |> 
  slice_tail(n = 12) |> 
  mutate(title_year = glue::glue("{title} ({release_year})")) |> 
  select(-release_year, -title)

glimpse(my_films)

my_films |> 
  gt()
# ?gt

# Add groups and rows to 'visual hierarchy'
films_tbl <- my_films |> 
  group_by(production_company) |> 
  gt(rowname_col = "title_year")
films_tbl

# Add header and source note
films_tbl <- films_tbl |> 
  tab_header(
    md("**A Sample of San Francisco Cinema**"),
    "Three Films from Three Production Companies"
  ) |> 
  tab_source_note(md("Source: ['Film Locations in San Francisco', DataSF | Open Data Portal](https://data.sfgov.org/Culture-and-Recreation/Film-Locations-in-San-Francisco/yitu-d5am/about_data)"))
films_tbl

# Add 'spanners'
films_tbl <- films_tbl |> 
  tab_spanner(
    "Staff",
    columns = c(director, writer)
  ) |> 
  tab_spanner(
    "Cast",
    columns = starts_with("actor")
  )
films_tbl

# Formatters
films_tbl <- films_tbl |> 
  fmt_currency(
    columns = gross,
    decimals = 0
  )
films_tbl

# Rename columns
films_tbl <- films_tbl |> 
  cols_label(
    starts_with("actor") ~ "",
    director = "Director",
    writer = "Writer",
    gross = "Gross"
  )
films_tbl

# Add style
films_tbl <- films_tbl |> 
  tab_style(
    style = list(
      cell_text(
        style = "italic",
        align = "right"
      )
    ),
    locations = cells_stub()
  ) |> 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_row_groups()
  )
films_tbl
# ?tab_style

# Conditional coloring
films_tbl |> 
  data_color(
    columns = gross,
    colors = "Greens"
  )
# ?data_color

# Add summary rows
films_tbl <- films_tbl |> 
  summary_rows(
    columns = gross,
    fns = list(Total = ~sum(.)),
    formatter = fmt_currency,
    decimals = 0
  ) |> 
  grand_summary_rows(
    columns = gross,
    fns = list(`Grand Total` = ~sum(.)),
    formatter = fmt_currency,
    decimals = 0
  )
films_tbl


# gtExtras bars
films_tbl <- my_films |> 
  mutate(gross2 = gross, .after = gross) |> 
  group_by(production_company) |> 
  gt(rowname_col = "title_year") |> 
  tab_header(
    md("**A Sample of San Francisco Cinema**"),
    "Three Films from Three Production Companies"
  ) |> 
  tab_spanner(
    "Cast",
    columns = starts_with("actor")
  ) |> 
  tab_spanner(
    "Staff",
    columns = c(director, writer)
  ) |>
  fmt_currency(
    columns = gross,
    decimals = 0
  ) |> 
  cols_label(
    starts_with("actor") ~ "",
    director = "Director",
    writer = "Writer",
    gross = "Gross",
    gross2 = ""
  ) |> 
  tab_style(
    style = list(
      cell_text(
        style = "italic",
        align = "right"
      )
    ),
    locations = cells_stub()
  ) |> 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_row_groups()
  ) |> 
  summary_rows(
    columns = gross,
    fns = list(Total = ~sum(.)),
    fmt = ~fmt_number(., decimals = 0)
  ) |> 
  grand_summary_rows(
    columns = gross,
    fns = list(`Grand Total` = ~sum(.)),
    fmt = ~fmt_number(., decimals = 0)
  ) |> 
  gtExtras::gt_plt_bar_pct(
    column = gross2,
    fill = "darkgreen"
  )
films_tbl

# Additional style
films_tbl <- films_tbl |> 
  cols_merge(
    columns = starts_with("actor"),
    pattern = ("<ul><li>{1}</li><li>{2}</li><li>{3}</li></ul>")
  )
films_tbl
# ?cols_merge

# themes
films_tbl |> 
  gtExtras::gt_theme_538()

#images
my_films |> 
  head(3) |>
  mutate(film_img = c(
    "https://upload.wikimedia.org/wikipedia/commons/a/a7/All_About_Eve_%281950_poster_-_retouch%29.jpg",
    "https://upload.wikimedia.org/wikipedia/en/5/5e/The_House_on_Telegraph_Hill_Poster.jpg",
    "https://upload.wikimedia.org/wikipedia/en/e/ed/The-internship-poster.jpg"
    )
  ) |>
  relocate(film_img, .before = director) |>
  mutate(gross2 = gross) |> 
  group_by(production_company) |>
  gt(rowname_col = "title_year") |> # rowname_col = "title_year"
  tab_header(
    md("**A Sample of San Francisco Cinema**"),
    "Three Films from Three Production Companies"
  ) |> 
  tab_spanner(
    "Cast",
    columns = starts_with("actor")
  ) |> 
  tab_spanner(
    "Staff",
    columns = c(director, writer)
  ) |> 
  fmt_currency(
    columns = gross,
    decimals = 0
  ) |> 
  gtExtras::gt_plt_bar_pct(
    column = gross2,
    fill = "darkgreen"
  ) %>%
  cols_label(
    starts_with("actor") ~ "",
    director = "Director",
    writer = "Writer",
    gross = "Gross",
    gross2 = "",
    film_img = ""
  ) |> 
  cols_merge(
    columns = starts_with("actor"),
    pattern = ("<ul><li>{1}</li><li>{2}</li><li>{3}</li></ul>")
  ) |> 
  tab_style(
    style = list(
      cell_text(
        style = "italic",
        align = "right"
      )
    ),
    locations = cells_stub()
  ) |> 
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_row_groups()
  ) |> 
  summary_rows(
    columns = gross,
    fns = list(Total = ~sum(.)),
    formatter = fmt_currency,
    decimals = 0
  ) |> 
  grand_summary_rows(
    columns = gross,
    fns = list(`Grand Total` = ~sum(.)),
    formatter = fmt_currency,
    decimals = 0
  ) |> 
  gtExtras::gt_img_rows(
    columns = film_img,
    height = 200
  ) |>
  tab_source_note(md("Source: ['Film Locations in San Francisco', DataSF | Open Data Portal](https://data.sfgov.org/Culture-and-Recreation/Film-Locations-in-San-Francisco/yitu-d5am/about_data)")) |> 
  gtExtras::gt_theme_538()

#####################################
# Begruding ChatGPT inclusion below # 
#####################################

# Prepare and slightly modify the dataset
mtcars_tbl <- mtcars %>%
  rownames_to_column(var = "car") %>%
  mutate(
    cyl = factor(cyl),
    mpg_rating = case_when(
      mpg >= 25 ~ "Excellent",
      mpg >= 20 ~ "Good",
      TRUE ~ "Average"
    )
  )

# Create the fancy gt table
mtcars_tbl %>%
  select(car, mpg, mpg_rating, cyl, hp, wt, qsec) %>%
  gt(rowname_col = "car") %>%
  
  # Add title and subtitle
  tab_header(
    title = md("**Fuel Efficiency and Performance: mtcars Dataset**"),
    subtitle = md("A showcase of `gt` package features")
  ) %>%
  
  # Add source note
  tab_source_note(
    source_note = md("Data from R's built-in `mtcars` dataset.")
  ) %>%
  
  # Format numbers
  fmt_number(
    columns = c(mpg, hp, wt, qsec),
    decimals = 2
  ) %>%
  
  # Add color scale to mpg
  data_color(
    columns = mpg,
    colors = scales::col_numeric(
      palette = c("lightblue", "darkblue"),
      domain = NULL
    )
  ) %>%
  
  # Add bar plots inside columns
  gtExtras::gt_plt_bar_pct(
    column = hp,
    scaled = TRUE,
    fill = "darkgreen"
  ) %>%
  
  # Format categorical column
  text_transform(
    locations = cells_body(columns = mpg_rating),
    fn = function(x) {
      dplyr::case_when(
        x == "Excellent" ~ glue::glue("<span style='color:green;'>{x}</span>"),
        x == "Good" ~ glue::glue("<span style='color:orange;'>{x}</span>"),
        TRUE ~ glue::glue("<span style='color:red;'>{x}</span>")
      )
    }
  ) %>%
  
  # Rename column labels
  cols_label(
    mpg = "Miles/Gallon",
    mpg_rating = "MPG Rating",
    cyl = "Cylinders",
    hp = "Horsepower",
    wt = "Weight",
    qsec = "1/4 mile time"
  ) %>%
  
  # Add column spanner
  tab_spanner(
    label = "Performance Metrics",
    columns = c(hp, qsec)
  ) %>%
  
  # Highlight specific rows
  tab_style(
    style = cell_fill(color = "lightyellow"),
    locations = cells_body(rows = mpg > 30)
  ) %>%
  
  # Align text
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  
  # Style the table
  # opt_table_font(font = list(
  #   google_font(name = "Roboto Mono"),
  #   default_fonts()
  # )) %>%
  opt_row_striping() %>%
  opt_stylize(style = 3) # Apply a built-in GT theme


# gtsummary for regression tables
m <- lm(mpg ~ wt + hp + factor(cyl), data = mtcars)
tbl_reg <- tbl_regression(m)
tbl_reg
tbl_reg |> 
  gtsummary::as_gt() |> 
  tab_header(
    "Modeling MPG w/mtcars"
  ) |> 
  gtExtras::gt_theme_538()
