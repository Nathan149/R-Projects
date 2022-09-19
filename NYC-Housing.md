### About
This is an analysis done on a record of every building or building unit (apartment etc) sold in the New York property market over a 12 month period between 2016 and 2017.
The goal of this project is to highlight any trends in this data set, and maybe predict sale value for some of these properties for the future.

This dataset is a concatenated and slightly cleaned-up version of the New York City Department of Finance's made available by [City of New York](www.kaggle.com/datasets/new-york-city/nyc-property-sales)

#### Data cleaning and manipulation
installing packages and adding data set
```{r}
install.packages("tidyverse")
library(tidyverse)

```
```{r}
nyc_sales <- read.csv('nyc-rolling-sales.csv')
View(nyc_sales)

```
Delete the Easement column since there's no data, then renaming all the other columns.
```{r}
nyc_sales <- subset(nyc_sales, select = -c(EASE.MENT))

#convert all column names to lower case.
names(nyc_sales) <- tolower(names(nyc_sales))

#rename columns
nyc_sales <- rename(nyc_sales, c(building_class_category = building.class.category,
tax_class_present = tax.class.at.present,
building_class_present = building.class.at.present,
apartment_number = apartment.number,
zipcode = zip.code,
residential_units = residential.units,
commercial_units = commercial.units,
total_units = total.units,
land_square_feet = land.square.feet,
gross_square_feet = gross.square.feet,
year_built = year.built,
tax_class_time_of_sale = tax.class.at.time.of.sale,
building_class_time_of_sale = building.class.at.time.of.sale,
sale_price = sale.price,
sale_date = sale.date))

```
Tax classes consist of values 1,2,3 and 4
checking out unique values for the tax classes.
 
We replace the values with alphabet with just the preceding numbers to represent the tax class.

```{r}
unique(nyc_sales$tax_class_present)

```
```{r}
nyc_sales['tax_class_present'][nyc_sales['tax_class_present'] == '2A'] <- 2
nyc_sales['tax_class_present'][nyc_sales['tax_class_present'] == '2B'] <- 2
nyc_sales['tax_class_present'][nyc_sales['tax_class_present'] == '2C'] <- 2
nyc_sales['tax_class_present'][nyc_sales['tax_class_present'] == '1C'] <- 1
nyc_sales['tax_class_present'][nyc_sales['tax_class_present'] == '1A'] <- 1
nyc_sales['tax_class_present'][nyc_sales['tax_class_present'] == '1B'] <- 1
nyc_sales['tax_class_present'][nyc_sales['tax_class_present'] == ' '] <- '-'

```
 Now we check for duplicates and create another file without duplicate values.
```{r}
nyc_housing <- nyc_sales[!duplicated(nyc_sales[c('address', 'building_class_category','sale_price', 'sale_date')]),]

```
Change data format for Date, sale price, land square feet and gross square feet columns 
```{r}
nyc_housing$sale_date <- as.POSIXct(nyc_housing$sale_date)

```
convert sale price, land square feet, gross square feet to numeric data type
```{r}
nyc_housing <- nyc_housing %>% mutate_at('sale_price', as.numeric)

nyc_housing <- nyc_housing %>% mutate_at(c('land_square_feet', 'gross_square_feet'), as.numeric)

```

In a bid to stream line our analysis we'll consider only buildings built no later than the year 1800.
```{r}
nyc_housing <- filter(nyc_housing, year_built >= 1800)

```
Most of the sales in the data set occurred with really small dollar amount or no amount at all, from this data set this indicates that these sales are actually transfer of deeds between parties, for example parents transferring deeds of ownership to their children.
To better come to an understanding of the sale trends and prices, we'll filter sales where the sale price is not available.
```{r}
nyc_housing <- nyc_housing %>% filter(!is.na(sale_price))

```

To better understand this data set we'll classify the year built into centuries to understand the number of buildings built in the various centuries from the 19th century to this 21st century.
```{r}
nyc_housing <- nyc_housing %>%
mutate(period = case_when(
year_built >= 1800 & year_built < 1900 ~ "19th Century",
year_built >= 1900 & year_built < 2000 ~ "20th Century",
year_built >= 2000 ~ "21st Century",
TRUE ~ "other"
))

```

#### Visualization
```{r}
nyc_housing %>%
  count(period) %>%
    mutate(Percentage = n/sum(n)) %>%
    ggplot(aes(x = period, y = Percentage)) +
    geom_col(aes(fill = period), position = "dodge") +
    geom_text(aes(label = scales::percent(Percentage),
    y = Percentage, group = period),
    position = position_dodge(width = 0.9),vjust = -1) +
    labs(title = "Houses sold per period", x = "Period")

```

 This clearly states that 82 percent of the houses sold were built in the 20th century.
 
 some other visuals shows the [most expensive neighborhood](https://public.tableau.com/views/NYChousingsalesdata/Mostexpensiveneigborhood?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link) to live in, [Average sale price per year](https://public.tableau.com/views/NYChousingsalesdata/Yearbuiltversusaveragesaleprice?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link), and the [average sale price per century](https://public.tableau.com/views/NYChousingsalesdata/Averagesalepricespercentury?:language=en-US&publish=yes&:display_count=n&:origin=viz_share_link) clearly stating that the more recent the building the more expensive it is.
 
 Other areas of analysis could include buildings that were resold, dividing the areas into areas that are primarily industrial and areas that are primarily residential.
