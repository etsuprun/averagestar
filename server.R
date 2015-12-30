require(shiny)
require(DT)
require(shinythemes)
require(dplyr)
require(ggvis)
load("categories.app.Rdata")

# Loading two objects here.
# categories.app: contains the categories along with the corresponding distribution of star ratings (min. # of businesses = 10)
# category.association.app: contains the frequency of a business belong to any two categories (with the min. frequency of 5)
# The data is processed by process_categories.R.


shinyServer(function(input, output, session) {
  
  
  getCategoryByMeanRating <- function (rating) {
    categories.app$category[order((categories.app$mean.stars.businesses - rating) %>% abs)][1]
  }
  
  # Create the drop-down of categories
  
  output$cat <- renderUI({
    selectInput(inputId = "category",
                label = h4("Choose the category of businesses:"),
                choices = sort(categories.app$category),
                selected = "Restaurants",
                multiple = F
    )   
  })
  
  # Display the average score.
  
  output$heading <- renderUI({
    h4(paste0("The mean Yelp rating for ",input$category," is ", 
              round(categories.app$mean.stars.businesses[categories.app$category==input$category],2),"."
    ),p(),paste0("(",
                 categories.app$business.count[categories.app$category==input$category],
                 " ",input$category, " businesses in the dataset)"
    ))
    
  })
  
  output$category <- renderUI ({h4(input$category)})
  
  # Update the data to be plotted 
  
  histogram.data <- reactive({
    
    
    
    
    # If the input controls haven't loaded yet, we're going to set the category to "Restaurants".
    
    if (is.null(input$category)) {
      category <- "Restaurants"
    }
    else {
      category <- input$category
    }
    
    # If the slider input doesn't correspond to the selected category mean, it means it was changed. Let's react.
    # We will make the category input box equal to the category whose mean score is closest to the slider score.
    
    
    
    myData <-categories.app[categories.app$category==category, 
                            c("businesses.1.0stars",
                              "businesses.1.5stars",
                              "businesses.2.0stars",
                              "businesses.2.5stars",
                              "businesses.3.0stars",
                              "businesses.3.5stars",
                              "businesses.4.0stars",
                              "businesses.4.5stars",
                              "businesses.5.0stars")] %>%
      as.numeric %>%
      cbind(seq(1,5,.5)) %>%
      as.data.frame %>%
      setNames(c("number","stars"))
    
    myData$percent <- round(myData$number/ sum(myData$number) * 100)
    myData$percent.label <- paste0(round(myData$number/ sum(myData$number) * 100),"%")
    
    myData
    
    
  })
  
  # Plot the histogram
  
  histogram.data %>%
    ggvis(~stars, ~percent) %>%
    layer_text(text := ~percent.label, dy := -10, dx :=-5) %>%
    layer_bars(width = .3) %>%
    
    set_options(width="100%") %>%
    add_tooltip(function (data) {
      paste0(data$stack_upr_,
             "% of ",
             input$category,
             " businesses have the Yelp rating of ",
             (data$xmin_ + data$xmax_) / 2,
             "."
      )
    }) %>%
    add_axis(type = "x",
             title="Yelp Star Rating",
             title_offset = 30,
             grid = F
    ) %>%
    add_axis(type = "y",
             title="% of businesses",
             title_offset = 30,
             grid = F,
             ticks =5
    ) %>%
    
    bind_shiny("categoryvis")
  
  # Get the list of related categories sorted by how frequently categories are associated together (limit to 10 rows)
  
  output$relatedcategories <- renderUI ({
    
    if (!is.null(input$category)) {
      category <- input$category
    }
    else {
      category <- "Restaurants"
    }
    
    related.categories <- 
      category.association.app %>%
      filter(category1==category) %>%
      select(category2,count) %>%
      setNames(c("category1","count")) %>%
      rbind(
        category.association.app %>%
          filter(category2==category) %>%
          select(category1,count)
      ) %>%
      arrange (desc(count)) %>%
      top_n (10) %>%
      select(category1) %>%
      t %>%
      as.vector
    
    
    related.categories <- related.categories[related.categories %in% categories.app$category]
    
    
    
    
    # If there's a related a category selected, that overrides the main category.
    
    if (!is.null(input$related.category) && input$related.category !="(Select a related category)") {
      updateSelectInput(session = session,
                        inputId = "category",
                        selected = input$related.category)
    }
    
    selectInput("related.category",h4(paste0("Choose from categories similar to ",
                                             input$category,":")),
                c("(Select a related category)", related.categories),
                multiple = F)
    
  })
  
  
  
  
  output$avgSlider <- renderUI({
    
    if (!is.null(input$category)) {
      category <- input$category
    }
    
    else {
      category <- "Restaurants"
    }
    
    sliderInput("avgrating", label = h4("Find a business category by average score:"),
                min = round(min(categories.app$mean.stars.businesses),3),
                max = round(max(categories.app$mean.stars.businesses),3),
                step = 0.001,
                ticks = F,
                value = categories.app$mean.stars.businesses[categories.app$category==category],
                width = "100%")
    
  })
  
  # Update the category from the slider
  
  observeEvent(input$avgrating, {   
    if (!is.null (input$avgrating) &
        input$viewType=="Mean Yelp star rating"
    ) {
      category <-  getCategoryByMeanRating(input$avgrating)
      
      updateSelectInput(session, "category", selected = category)
    }
  })
  
})

