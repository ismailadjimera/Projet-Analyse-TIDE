library(dplyr)
library(tidyr)
library(rvest)
library(rcdimple)
demo(dimple)

# Get the table from the Census database with rvest

url <- "http://www.census.gov/population/international/data/idb/region.php?N=%20Results%20&T=10&A=separate&RT=0&Y=2015,2020,2025,2030,2035,2040,2045,2050&R=-1&C=IN"

df <- url %>%
  html() %>%
  html_nodes("table") %>%
  html_table() %>%
  data.frame()

names(df) <- c("Year", "Age", "total", "Male", "Female", "percent", "pctMale", "pctFemale", "sexratio") 

cols <- c(1, 3:9)

df[,cols] <- apply(df[,cols], 2, function(x) as.numeric(as.character(gsub(",", "", x))))

# Format the table with dplyr and tidyr

df1 <- df %>%
  mutate(Order = 1:nrow(df), 
         Male = -1 * Male) %>%
  filter(Age != "Total") %>%
  select(Year, Age, Male, Female, Order) %>%
  gather(Gender, Population, -Age, -Order, -Year)

max_x <- plyr::round_any(max(df1$Population), 10000, f = ceiling)
min_x <- plyr::round_any(min(df1$Population), 10000, f = floor)

# Build the chart with rcdimple

df1 %>%
  dimple(x = "Population", y = "Age", group = "Gender", type = 'bar', storyboard = "Year") %>%
  yAxis(type = "addCategoryAxis", orderRule = "Order") %>%
  xAxis(type = "addMeasureAxis", overrideMax = max_x, overrideMin = min_x) %>%
  default_colors(c("blue", "red")) %>%
  add_legend() %>%
  add_title(html = "<h3 style='font-family:Helvetica; text-align: center;'>India's population, 2015-2050</h3>") %>%
  
  # Here, I'll pass in some JS code to make all the values on the X-axis and in the tooltip absolute values
  tack(., options = list(
    chart = htmlwidgets::JS("
                            function(){
                            var self = this;
                            // x axis should be first or [0] but filter to make sure
                            self.axes.filter(function(ax){
                            return ax.position == 'x'
                            })[0] // now we have our x axis set _getFormat as before
                            ._getFormat = function () {
                            return function(d) {
                            return d3.format(',.0f')(Math.abs(d) / 1000000) + 'm';
                            };
                            };
                            // return self to return our chart
                            return self;
                            }
                            "))
    )

