
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fitness

<!-- badges: start -->
<!-- badges: end -->

A dumping ground for some of my fitness data.

## intervals

-   Run some distance as fast as you can (time it), stop and recover
    until heartrate is 140 bpm.
-   Do this for the following distances (m): 800, 800, 600, 600, 400,
    400, 200.

``` r
library(tidyverse)

convert_min_sec_to_sec <- function(xs) {
  # ye olde splitten applyen combinen
  xs |> 
    strsplit(split = ":", fixed = TRUE) |> 
    lapply(function(x) {
      x <- as.numeric(x)
      60 * x[1] + x[2]
    }) |> 
    unlist(use.names = FALSE)
}

d <- "data/intervals.csv" |> 
  read_csv(
    col_types = cols(
      interval_number = "i",
      time = "c",
      distance = "c",
      date = col_date("%m/%d/%Y")
    )
  ) |> 
  mutate(
    time = convert_min_sec_to_sec(time)
  )

d |> 
  # Include all distances aggregated together as "[all]"
  bind_rows(d |> mutate(distance = "[all]")) |> 
  filter(interval_type == "run") |> 
  group_by(date, distance) |> 
  summarise(
    intervals = n(),
    total_run_time = sum(time),
    .groups = "drop"
  )
#> # A tibble: 10 × 4
#>    date       distance intervals total_run_time
#>    <date>     <chr>        <int>          <dbl>
#>  1 2022-07-28 [all]            7           1232
#>  2 2022-07-28 200              1             57
#>  3 2022-07-28 400              2            263
#>  4 2022-07-28 600              2            411
#>  5 2022-07-28 800              2            501
#>  6 2022-08-16 [all]            7           1181
#>  7 2022-08-16 200              1             50
#>  8 2022-08-16 400              2            257
#>  9 2022-08-16 600              2            399
#> 10 2022-08-16 800              2            475

d |> 
  group_by(date, interval_type) |> 
  summarise(
    intervals = n(),
    total_time = sum(time),
    .groups = "drop"
  )
#> # A tibble: 4 × 4
#>   date       interval_type intervals total_time
#>   <date>     <chr>             <int>      <dbl>
#> 1 2022-07-28 recover               7        778
#> 2 2022-07-28 run                   7       1232
#> 3 2022-08-16 recover               7       1231
#> 4 2022-08-16 run                   7       1181

d |> 
  filter(interval_type == "run") |> 
  ggplot() + 
    aes(
      x = (interval_number + 1) / 2, 
      y = (time / 60) / (as.numeric(distance) / 1000)
    ) +
    geom_line(aes(group = date, color = factor(date))) +
    geom_point(aes(color = factor(date))) +
    geom_label(
      aes(
        label = paste0(distance, " m"), 
        y = 3.5
      ), 
      hjust = .5,
      stat = "unique"
    ) +
    labs(x = "running interval number", y = "pace (min per km)", color = "date")
```

![](README_files/figure-gfm/intervals-1.png)<!-- -->

## bodyweight

In April, I took up a new hobby: Weekly boxing classes. This new
activity motivated me to be a little more active (closing my watch’s
daily exercise goal more often or trying exercises that help for boxing
conditioning). I started measuring my bodyweight around this time. I
haven’t change my diet or anything like that, and I don’t have any
weight goals besides getting back to my pre-pandemic bodyweight (250
lbs).

``` r
"data/bodyweight.csv" |> 
  read_csv(col_types = cols(date = col_date("%m/%d/%Y"))) |> 
  ggplot() + 
  aes(x = date, y = weight) + 
  ylim(250, 270) + 
  stat_smooth() +
  geom_point() +
  theme_grey(base_size = 16) +
  labs(y = "bodyweight [lb]")
#> `geom_smooth()` using method = 'loess' and formula 'y ~ x'
```

![](README_files/figure-gfm/bodyweight-1.png)<!-- -->
