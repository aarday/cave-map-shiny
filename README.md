# Cave Map Shiny App

📐 This is a Shiny application for visualizing 2D cave survey data from SQL files. It reads survey shots data, calculates station coordinates, and plots cave passages as lines with station labels.

------------------------------------------------------------------------

## Features

-   Upload `.sql` survey files containing cave shots data.
-   Calculate 2D positions of survey stations based on shot length and azimuth.
-   Visualize cave passages as line segments.
-   Display station IDs at their respective positions.
-   Rotate the entire cave map 90° to the left or right using buttons.

------------------------------------------------------------------------

## Requirements

-   R (version 4.0 or later recommended)
-   R packages:
    -   shiny
    -   dplyr
    -   ggplot2
    -   stringr

You can install required packages using:

``` r
install.packages(c("shiny", "dplyr", "ggplot2", "stringr"))
```

Author 
Arda Yapıcı
