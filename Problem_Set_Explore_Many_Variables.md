Lesson 8: Explore Many Variables
================
Josh Goldberg
October 29, 2017

``` r
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(data.table)
```

``` r
# Create a histogram of diamond prices.
# Facet the histogram by diamond color
# and use cut to color the histogram bars.

# The plot should look something like this.
# http://i.imgur.com/b5xyrOu.jpg

# Note: In the link, a color palette of type
# 'qual' was used to color the histogram using
# scale_fill_brewer(type = 'qual')

# load dataset
data(diamonds)

# name variables
names(diamonds)
```

    ##  [1] "carat"   "cut"     "color"   "clarity" "depth"   "table"   "price"  
    ##  [8] "x"       "y"       "z"

``` r
# structure of dataset
glimpse(diamonds)
```

    ## Observations: 53,940
    ## Variables: 10
    ## $ carat   <dbl> 0.23, 0.21, 0.23, 0.29, 0.31, 0.24, 0.24, 0.26, 0.22, ...
    ## $ cut     <ord> Ideal, Premium, Good, Premium, Good, Very Good, Very G...
    ## $ color   <ord> E, E, E, I, J, J, I, H, E, H, J, J, F, J, E, E, I, J, ...
    ## $ clarity <ord> SI2, SI1, VS1, VS2, SI2, VVS2, VVS1, SI1, VS2, VS1, SI...
    ## $ depth   <dbl> 61.5, 59.8, 56.9, 62.4, 63.3, 62.8, 62.3, 61.9, 65.1, ...
    ## $ table   <dbl> 55, 61, 65, 58, 58, 57, 57, 55, 61, 61, 55, 56, 61, 54...
    ## $ price   <int> 326, 326, 327, 334, 335, 336, 336, 337, 337, 338, 339,...
    ## $ x       <dbl> 3.95, 3.89, 4.05, 4.20, 4.34, 3.94, 3.95, 4.07, 3.87, ...
    ## $ y       <dbl> 3.98, 3.84, 4.07, 4.23, 4.35, 3.96, 3.98, 4.11, 3.78, ...
    ## $ z       <dbl> 2.43, 2.31, 2.31, 2.63, 2.75, 2.48, 2.47, 2.53, 2.49, ...

``` r
levels(diamonds$clarity)
```

    ## [1] "I1"   "SI2"  "SI1"  "VS2"  "VS1"  "VVS2" "VVS1" "IF"

``` r
# reorganize levels of ordered factors in ascending order
levels(diamonds$color) = c('J', 'I', 'H', 'G', 'F', 'E', 'D')
levels(diamonds$clarity) = c('I1', 'SI1', 'SI2', 'VS1', 'VS2', 'VVS1', 'VVS2', 'IF')

# plot
ggplot(aes(x = log10(price), fill = cut), data = diamonds) +
  labs(x = 'Price', y = 'Frequency',
       title = 'Price of Diamonds by Color and Cut') +
  geom_histogram(bidwith = .01) +
  facet_wrap( ~ color)
```

![](Figs/Price%20Histograms%20with%20Facet%20and%20Color-1.png)

``` r
# Create a scatterplot of diamond price vs.
# table and color the points by the cut of
# the diamond.

# The plot should look something like this.
# http://i.imgur.com/rQF9jQr.jpg

# Note: In the link, a color palette of type
# 'qual' was used to color the scatterplot using
# scale_color_brewer(type = 'qual')

ggplot(aes(x = table, y = price), data = diamonds) +
  labs(x = 'Table: width of top of diamond relative to widest point (43–95)',
       y = 'Price', title = 'Price vs. Table Colored by Cut') +
  geom_point(aes(color = cut)) +
  scale_color_brewer(type = 'qual')
```

![](Figs/Price%20vs.%20Table%20Colored%20by%20Cut-1.png)

What is the typical table range for the majority of diamonds of ideal cut? 53-57. What is the typical table range for the majority of diamonds of premium cut? 58-62.

``` r
# Create a scatterplot of diamond price vs.
# volume (x * y * z) and color the points by
# the clarity of diamonds. Use scale on the y-axis
# to take the log10 of price. You should also
# omit the top 1% of diamond volumes from the plot.

# Note: Volume is a very rough approximation of
# a diamond's actual volume.

# The plot should look something like this.
# http://i.imgur.com/excUpea.jpg

# Note: In the link, a color palette of type
# 'div' was used to color the scatterplot using
# scale_color_brewer(type = 'div')

diamonds$volume <- diamonds$x * diamonds$y * diamonds$z

ggplot(aes(x = volume, y = price), 
       data = subset(diamonds, volume <= quantile(volume, 0.99) & volume > 0)) +
  labs(x = 'Volume', y = 'Price',
       title = 'Price vs. Volume and Diamond Clarity') +
  geom_point(aes(color = clarity)) +
  coord_trans(y = 'log10') +
  scale_color_brewer(type = 'div')
```

![](Figs/Price%20vs.%20Volume%20and%20Diamond%20Clarity-1.png)

``` r
# Many interesting variables are derived from two or more others.
# For example, we might wonder how much of a person's network on
# a service like Facebook the user actively initiated. Two users
# with the same degree (or number of friends) might be very
# different if one initiated most of those connections on the
# service, while the other initiated very few. So it could be
# useful to consider this proportion of existing friendships that
# the user initiated. This might be a good predictor of how active
# a user is compared with their peers, or other traits, such as
# personality (i.e., is this person an extrovert?).

# Your task is to create a new variable called 'prop_initiated'
# in the Pseudo-Facebook data set. The variable should contain
# the proportion of friendships that the user initiated.

pf <- read_tsv('pseudo_facebook.tsv')

pf$prop_initiated <- pf$friendships_initiated / pf$friend_count
```

``` r
# Create a line graph of the median proportion of
# friendships initiated ('prop_initiated') vs.
# tenure and color the line segment by
# year_joined.bucket.

# Recall, we created year_joined.bucket in Lesson 5
# by first creating year_joined from the variable tenure.
# Then, we used the cut function on year_joined to create
# four bins or cohorts of users.

# (2004, 2009]
# (2009, 2011]
# (2011, 2012]
# (2012, 2014]

# The plot should look something like this.
# http://i.imgur.com/vNjPtDh.jpg
# OR this
# http://i.imgur.com/IBN1ufQ.jpg

pf$year_joined <- floor(2014 - (pf$tenure / 365))
pf$year_joined.bucket <- cut(pf$year_joined, breaks =
                             c(2004, 2009, 2011, 2012, 2014))

ggplot(aes(x = tenure, y = prop_initiated),
       data = pf) +
  labs(x = 'Tenure', 
       y = 'Median Proportion of Friends Initiated',
       title = 'Proportion Initiated vs. Tenure') +
  geom_line(aes(color = year_joined.bucket), stat = 'summary', 
            fun.y = median) +
  scale_color_brewer('Year Joined', palette = 'Set2')
```

![](Figs/Proportion%20Initiated%20vs.%20Tenure-1.png)

``` r
# Smooth the last plot you created of
# of prop_initiated vs tenure colored by
# year_joined.bucket. You can bin together ranges
# of tenure or add a smoother to the plot.

# You will answer some questions about your plot in
# the next two exercises.

ggplot(aes(x = tenure, y = prop_initiated),
       data = pf) +
  labs(x = 'Tenure', 
       y = 'Median Proportion of Friends Initiated',
       title = 'Proportion Initiated vs. Tenure') +
  geom_line(aes(color = year_joined.bucket), stat = 'summary', 
            fun.y = median) +
  geom_smooth() +
  scale_colour_brewer('Year Joined', palette = 'Set2')
```

![](Figs/SmoothingProportion%20Initiated%20vs.%20Tenure-1.png)

On average, which group initiated the greastest proportion of its Facebook friendship? People who joined after 2012.

For the group with the largest proportion of friendships initiated, what is the group's average (mean) proportion of friendships initiated? The mean is 0.6653892

Why do you think this group's proportion of friendships initiated is higher than others? They have been around for a shorter period of time. This relationship holds true for the most part across cohorts. As buckets of facebook users tenure increases, friendships initiated declines.

``` r
# Create a scatter plot of the price/carat ratio
# of diamonds. The variable x should be
# assigned to cut. The points should be colored
# by diamond color, and the plot should be
# faceted by clarity.

# The plot should look something like this.
# http://i.imgur.com/YzbWkHT.jpg.

# Note: In the link, a color palette of type
# 'div' was used to color the histogram using
# scale_color_brewer(type = 'div')

ggplot(aes(x = cut, y = price/carat),
       data = diamonds) +
  labs(x = 'Cut', y = 'Price/Carat Ratio',
       title = 'Price/Carat Binned, Faceted, & Colored') +
  geom_point(aes(color = color), position = 'jitter',
             alpha = 0.5) +
  scale_color_brewer(type = 'div') +
  facet_wrap( ~ clarity, ncol = 2)
```

![](Figs/Price/Carat%20Binned,%20Faceted,%20&%20Colored-1.png)

``` r
# The Gapminder website contains over 500 data sets with information about
# the world's population. Your task is to continue the investigation you did at the
# end of Problem Set 4 or you can start fresh and choose a different
# data set from Gapminder.

# If you’re feeling adventurous or want to try some data munging see if you can
# find a data set or scrape one from the web.

# In your investigation, examine 3 or more variables and create 2-5 plots that make
# use of the techniques from Lesson 5.

# You can find a link to the Gapminder website in the Instructor Notes.

# Once you've completed your investigation, create a post in the discussions that includes:
#       1. the variable(s) you investigated, your observations, and any summary statistics
#       2. snippets of code that created the plots
#       3. links to the images of your plots
```
