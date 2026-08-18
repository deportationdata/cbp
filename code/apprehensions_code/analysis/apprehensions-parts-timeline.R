#packages 
library(tidyverse)
library(arrow)
library(lubridate)

parts_metadata <- read_parquet(
  "data/apprehensions/metadata/parts_metadata.parquet"
)

# data info / cleaning 
glimpse(parts_metadata)

parts_metadata |>
  select(
    part_file,
    source_file,
    min_date,
    max_date,
    n_rows
  ) |>
  print(n = Inf)

class(parts_metadata$min_date)
class(parts_metadata$max_date)

# extract timeline data, create contained flag
timeline_data <- parts_metadata |>
  filter(
    !is.na(min_date),
    !is.na(max_date)
  ) |>
  mutate(
    contained = pmap_lgl(
      list(
        part_file,
        min_date,
        max_date
      ),
      \(current_file, current_min, current_max) {
        any(
          part_file != current_file &
            min_date <= current_min &
            max_date >= current_max
        )
      }
    ),
    part_label = part_file |>
      str_remove("\\.parquet$") |>
      str_remove("^\\d{4}_"),
    part_label = fct_reorder(
      part_label,
      min_date,
      .desc = TRUE
    )
  )


timeline_data |>
  select(
    part_label,
    min_date,
    max_date,
    n_rows
  ) |>
  print(n = Inf)

# plot timeline 
apprehensions_timeline <- ggplot(
  timeline_data,
  aes(y = part_label)
) +
  geom_segment(
    aes(
      x = min_date,
      xend = max_date,
      yend = part_label,
      color = contained
    ),
    linewidth = 0.5
  ) +
  geom_point(
    aes(x = min_date,
        color = contained),
    shape = 124,
    size = 0.75
  ) + 
  geom_point(
    aes(x = max_date,
        color = contained),
    shape = 124,
    size = 0.75
  ) + 
  scale_color_manual(
    values = c(
      "FALSE" = "black",
      "TRUE" = "red"
    ),
    labels = c(
      "FALSE" = "Not contained",
      "TRUE" = "Contained in a larger date range"
    ),
    name = NULL
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    date_minor_breaks = "1 month",
    expand = expansion(
      mult = c(0.01, 0.02)
    )
  ) +
  labs(
    title = "Date Coverage of Apprehension Parts",
    x = "Date Coverage",
    y = NULL,
    caption = "Source: U.S. Customs and Border Protection"
  ) + 
  theme_minimal() + 
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(
      color = "grey75",
      linewidth = 0.4
    ),
    panel.grid.minor.x = element_line(
      color = "grey90",
      linewidth = 0.2
    )
  )

apprehensions_timeline


# save plot
ggsave(
  "data/apprehensions/metadata/apprehensions-parts-timeline.pdf",
  plot = apprehensions_timeline,
  width = 24,
  height = 14,
  units = "in",
  limitsize = FALSE
)


