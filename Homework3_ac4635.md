Homework 3
================
JR Chansakul
2020-10-05

## Problem 1

``` r
data("instacart")
```

This dataset contains 1384617 rows and 15columns.

Observations are the level of items in orders by user. There are user /
order variables – user ID, order ID, order day, and order hour. There
are also item variables – name, aisle, department, and some numeric
codes.

How many aisles, and which are most items from?

``` r
instacart %>% 
    count(aisle) %>% 
  arrange(desc(n))
```

    ## # A tibble: 134 x 2
    ##    aisle                              n
    ##    <chr>                          <int>
    ##  1 fresh vegetables              150609
    ##  2 fresh fruits                  150473
    ##  3 packaged vegetables fruits     78493
    ##  4 yogurt                         55240
    ##  5 packaged cheese                41699
    ##  6 water seltzer sparkling water  36617
    ##  7 milk                           32644
    ##  8 chips pretzels                 31269
    ##  9 soy lactosefree                26240
    ## 10 bread                          23635
    ## # … with 124 more rows

Let’s make a plot.

``` r
instacart %>% 
    count(aisle) %>% 
    filter(n > 10000) %>% 
  mutate(
        aisle = factor(aisle),
        aisle = fct_reorder(aisle, n)
    ) %>% 
    ggplot(aes(x = aisle, y = n)) + 
    geom_point() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
```

<img src="Homework3_ac4635_files/figure-gfm/unnamed-chunk-3-1.png" width="90%" />

Let’s make a table\!

``` r
instacart %>% 
    filter(aisle %in% c("baking ingredients", "dog food care", "packaged vegetables fruits")) %>%
  group_by(aisle) %>% 
    count(product_name) %>% 
    mutate(rank = min_rank(desc(n))) %>% 
  filter(rank < 4) %>% 
  arrange(aisle, rank) %>% 
    knitr::kable()
```

| aisle                      | product\_name                                 |    n | rank |
| :------------------------- | :-------------------------------------------- | ---: | ---: |
| baking ingredients         | Light Brown Sugar                             |  499 |    1 |
| baking ingredients         | Pure Baking Soda                              |  387 |    2 |
| baking ingredients         | Cane Sugar                                    |  336 |    3 |
| dog food care              | Snack Sticks Chicken & Rice Recipe Dog Treats |   30 |    1 |
| dog food care              | Organix Chicken & Brown Rice Recipe           |   28 |    2 |
| dog food care              | Small Dog Biscuits                            |   26 |    3 |
| packaged vegetables fruits | Organic Baby Spinach                          | 9784 |    1 |
| packaged vegetables fruits | Organic Raspberries                           | 5546 |    2 |
| packaged vegetables fruits | Organic Blueberries                           | 4966 |    3 |

Apples vs ice cream..

``` r
instacart %>% 
    filter(product_name %in% c("Pink Lady Apples", "Coffee Ice Cream")) %>%
  group_by(product_name, order_dow) %>% 
    summarize(mean_hour = mean(order_hour_of_day)) %>% 
    pivot_wider(
        names_from = order_dow,
        values_from = mean_hour
    )
```

    ## `summarise()` regrouping output by 'product_name' (override with `.groups` argument)

    ## # A tibble: 2 x 8
    ## # Groups:   product_name [2]
    ##   product_name       `0`   `1`   `2`   `3`   `4`   `5`   `6`
    ##   <chr>            <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
    ## 1 Coffee Ice Cream  13.8  14.3  15.4  15.3  15.2  12.3  13.8
    ## 2 Pink Lady Apples  13.4  11.4  11.7  14.2  11.6  12.8  11.9

## Problem 2

## Problem 2 part 1

Load, tidy, and otherwise wrangle the data. Your final dataset should
include all originally observed variables and values; have useful
variable names; include a weekday vs weekend variable; and encode data
with reasonable variable classes. Describe the resulting dataset
(e.g. what variables exist, how many observations, etc).

``` r
##Tidy and Wrangle Dataset
tidy_accel_df=
  read.csv("./data/accel_data.csv") %>%
  janitor::clean_names() %>%
  pivot_longer(
          activity_1:activity_1440,
          names_to = "activity_minute", 
          names_prefix = "activity_",
          values_to = "activity_count") %>% 
  mutate(
    Weekdays = 
      case_when(day %in% c("Saturday", "Sunday") ~ "weekend",
                day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday") ~  "weekday")) %>%
  mutate(
      activity_minute = as.numeric(activity_minute),
      day = factor(day),
      day = fct_relevel(day, c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))) %>%
  view ()

# View variable types of final dataset

view(tidy_accel_df) %>% view ()
sapply(tidy_accel_df, class) %>% view ()
```

The accelerometer dataset contains daily information of “activity
counts” in one-minute intervals of a 63 year-old male with BMI 25, who
was admitted to Columbia University Medical Center and diagnosed with
congestive heart failure (CHF). The tidied dataset contains 6 variables
and 50400 rows or observations.The variables of included in the dataset
are week, day\_id, day, activity\_minute, activity\_count, Weekdays and
their variable types are integer, integer, factor, numeric, numeric,
character, respectively.

We have 35 days of activity count data collected from the 65 year-old
male by minute. We have the week variable that denotes the week that
that accelerometer data was collected and the newly generated factor
variable of weekday, which denotes whether the data collected was on a
weekend or a weekday.

## Problem 2 part 2

Traditional analyses of accelerometer data focus on the total activity
over the day. Using your tidied dataset, aggregate accross minutes to
create a total activity variable for each day, and create a table
showing these totals. Are any trends apparent?

``` r
# Double check group_by 
Total_activity_day = 
  tidy_accel_df %>% 
  group_by(day_id, day) %>%
  mutate(
    sum_activity_day = sum(activity_count)) %>% 
  select(week, day_id, day, sum_activity_day) %>% 
  distinct() %>%
  knitr::kable() %>%
  view ()
```

summarize(total\_activity = sum(activity\_count)) view
(Total\_activity\_day)

## Problem 2 part 3

Accelerometer data allows the inspection activity over the course of the
day. Make a single-panel plot that shows the 24-hour activity time
courses for each day and use color to indicate day of the week. Describe
in words any patterns or conclusions you can make based on this graph.

``` r
tidy_accel_df %>% 
  ggplot(aes(x = activity_minute, y = activity_count, color = day)) + 
  geom_line() + 
  labs(
        title = "Activity Level Per Minute",
        x = "Time in minutes",
        y = "Level of Activity") + 
  scale_x_continuous(
    breaks = c(0, 360, 720, 1080, 1440), 
    labels = c("12AM", "6AM", "12PM", "6PM", "11:59PM"),
    limits = c(0, 1440)
    ) +
    viridis::scale_color_viridis(
      name = "Day",
      discrete = TRUE) + 
  theme(legend.position = "bottom")
```

<img src="Homework3_ac4635_files/figure-gfm/unnamed-chunk-7-1.png" width="90%" />

``` r
noaa_data <- p8105.datasets::ny_noaa %>% 
  janitor::clean_names() 
```

y format to long format group by and summarize problem geomline to
connect the dots x minutes on x-axissand activity count on y-axis only 1
plot

## Problem 3

data manipulation steps and then plotting
