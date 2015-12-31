require(shiny)
library("ggvis")
require(ndtv)
require(shinythemes)

shinyUI(
  fluidPage(
    theme = shinytheme("spacelab"),
    fluidRow(
      column(3,
             br(),
             img(src="logo.png", height = "30")
      ),
      column(9,
             titlePanel(windowTitle="averagestar.com - mean Yelp ratings by category","See how different types of businesses are rated on Yelp.")
      )
    ),
    p(),
    fluidRow(
      column(3,
             wellPanel(
               radioButtons(inputId = "viewType",
                            label = h4("Search for Yelp categories by:"),
                            choices = c("Category name",
                                        "Mean Yelp star rating")
               )
             ),
             conditionalPanel(
               condition = "input.viewType=='Category name'"
               ,
               wellPanel(
                 uiOutput("cat")
               ),
               wellPanel(
                 uiOutput("relatedcategories")
               )
             ),
             conditionalPanel(
               condition = "input.viewType=='Mean Yelp star rating'",
               
               wellPanel(
                 uiOutput("avgSlider"),
                 uiOutput("category")
               )
             )
      ),
      column(9,
             style = "background-color: #fff",
             wellPanel(uiOutput("heading"), style = "background-color: #fff"),
             ggvisOutput("categoryvis"),
             helpText("© averagestar.com. Code and analysis by Eugene Tsuprun. The data is from the ",
                a(href = "http://www.yelp.com/dataset_challenge","Yelp Challenge"),"."),
             a(href = "https://github.com/etsuprun/averagestar",img(src = "github.png")),
             a(href = "https://www.linkedin.com/pulse/why-internet-service-providers-airlines-get-bad-rap-eugene-tsuprun",
               img(src = "linkedin.png"))

      )
    )
  )
)