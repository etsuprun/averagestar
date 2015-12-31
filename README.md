# Average Star

www.averagestar.com lets you explore mean Yelp ratings by business category (such as "Food", "Barber Shops", or "Post Offices").

The data comes from [Yelp Dataset Challenge](http://www.yelp.com/dataset_challenge). Among other things, this dataset contains attributes of about 61,000 businesses from 10 metropolitan areas in the US, Canada, UK, and Germany, inlcluding the businesses' categories and Yelp star ratings.

This repository contains the code for processing the data (`process_categories.R`), the resulting dataset in the R object format (`categories.Rdata`), and the [reactive Shiny app](http://shiny.rstudio.com/) that's featured on averagestar.com (`ui.R` and `server.R`).

## Processing the Data

We process using `process_categories.R`. Each category with at least 10 businesses in the dataset gets a row in the dataframe `categories`, and we calculate the following:

1) Number of businesses with each star rating (1-5, rounded to the nearest half star)
2) Number of reviews with each star rating (1-5, whole numbers only)
3) Funniness, usefulness, and coolness of reviews by category

(Note that for averagestar.com, we're only using #1. #2 and #3 can be used in other projects, such as Yelp Galaxy.)

We also create a dataframe called `category.association` indicating how often any two categories are associated with the same business. Each row in this dataframe contains the two categories as well as the frequency of association. For instance, two categories that often co-exist are "Bars" and "Nightlife". There are 3,628 businesses (out of 61,000) that are classified with both of these categories.

We save abbreviated versions of the two dataframes, containing only the columns relevant to averagestar, to the R object `categories.app`.

## Visualizing the Data

Averagestar follows the typical Shiny structure, with the UI and inputs located in `ui.R` and the logic/visualization located in `server.R`.

We produce a ggvis plot that can react to one of three inputs:

* Selecting a category from a list of all categories
* Selecting a category from a list of categories related to the last selected categories. (The related categories are based on the `category.association` dataframe.)
* Selecting a mean Yelp rating from an input slider.

## Your feedback

Your feedback is welcome! Feel free to e-mail me at eugene@tsuprun.com.

© averagestar.com. Code and analysis by Eugene Tsuprun. The data is from the Yelp Challenge. 