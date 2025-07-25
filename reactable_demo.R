library(janitor)
library(tidyverse)
library(reactable)

# A reactable within a reactable?
grants <- c("CDBG/General Fund", "WIOA Adult", "WIOA Youth", "WIOA Dislocated Worker")

client_data <- charlatan::ch_generate(n = 50) |> 
  mutate(
    grant = sample(grants, size = 50, replace = TRUE),
    id = row_number()
  )

summary_data <- client_data |> 
  tabyl(grant) |> 
  adorn_totals() |> 
  select(grant, clients = n)

row_details <- function(index) {
  
  if (index != nrow(summary_data)) {
    
    gc <- summary_data$grant[index]
    df <- client_data |> 
      filter(grant == gc) |> 
      select(-grant) |> 
      relocate(id, .before = 1)
    
    reactable(
      df,
      filterable = TRUE,
      sortable = TRUE,
      pagination = FALSE,
      highlight = TRUE,
      compact = TRUE,
      wrap = FALSE
    )
  }
}

reactable(
  summary_data,
  sortable = TRUE,
  pagination = FALSE,
  highlight = TRUE,
  compact = TRUE,
  details = row_details,
)

