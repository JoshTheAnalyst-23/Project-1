# Video games analysis
#  1. What are the top 5 best-selling games globally?
#  2. How do the global sales compare across genres?
#  3. Which platform has the highest sales in North America?
#  4. What are the top 3 genres in Europe based on sales?
#  5. Are there any noticeable differences in genre popularity between Japan and other regions?
#  6. Which platform has shown the highest average global sales?
#  7. What is the most popular genre in terms of global sales?
#  8. Are there any genres that are consistently underperforming in all regions?
#  9. How have global sales trended over the years?
# 10. Are there any notable years with a significant increase or decrease in sales?
# 11. Which publisher has the highest total global sales?

# Solution 
# Firstly, activate the packages for data manipulation and data visualization
library("dplyr")
library("ggplot2")
library("forcats")

# Secondly, import the dataset from the source. Mine is from Microsoft Excel. Also, the dataset has properly 
# analyzed and cleaned on the spreadsheet before importation.
getwd()
VgSales <- readxl::read_xlsx("Video games.xlsx")
View(VgSales)

# Question 1
Top5_Games <- select(VgSales,Name,Global_Sales )
View(Top5_Games)
Top5_Games <- Top5_Games %>% arrange(desc(Global_Sales)) %>% head(5)

# Question 2 
# Firstly, select the required columns
GenreSales <- select(VgSales,Genre,Global_Sales)
# Secondly, perform the analysis on the chosen column
GenreSales <- GenreSales %>% group_by(GenreSales$Genre) %>% summarise(Total_Sales = sum(Global_Sales)) %>%
  arrange(desc(Total_Sales))
# View the output of the above query.
View(GenreSales)

# Question 3
NA_Platform <- select(VgSales,Platform,NA_Sales )
NA_Platform <- NA_Platform %>% group_by(Platforms = NA_Platform$Platform) %>% summarise(Total = sum(NA_Sales)) %>% 
  arrange(desc(Total)) %>% head(1)
View(NA_Platform)

# Question 4 
Europe_Genre <- select(VgSales,Genre,EU_Sales)
View(Europe_Genre)
Europe_Genre <- Europe_Genre %>% group_by(Genre = Genre) %>% reframe(Total =sum(EU_Sales)) %>%
  arrange(desc(Total)) %>% head(3)

# Question 5
# Step 1 (Organize and summarize the data)
Games <- VgSales %>% group_by(Genre = VgSales$Genre) %>% summarise("North America" = round(sum(NA_Sales),1),
                                                            European = round(sum(EU_Sales),1),
                                                           Japan = round(sum(JP_Sales),1),
                                                           "Other Regions" = round(sum(Other_Sales),1),
                                                           Globally = round(sum(Global_Sales),1))
View(Games) 

# Step 2 (Select the region and organize the data using the fct_reorder function from the "FORCATS" package.
InJapan <- Games %>% mutate(Genre = fct_reorder(Genre,Japan))

# Step 3 (Visualize the findings)
In_Japan <- ggplot(data = InJapan, mapping = aes(x = Japan, y = Genre)) + geom_bar(stat = "identity",
                                                                                   fill = "black",
                                                                                   colour = "black") +
  labs(title = "Popularity in Japan", x = "Values", y = "Genre") + theme_minimal()

# Region 2
In_North <- Games %>% mutate(Genre = fct_reorder(Genre, `North America`))
In_north <- ggplot(data = In_North, mapping = aes(x = `North America`, y = Genre)) + geom_bar(stat = "identity",
                                                                                              fill = "red",
                                                                                              colour = "blue") +
  labs(title = "Popularity in Noth America", x = "Values", y = "Genre") + theme_minimal()

# Region 3
InEurope <- Games %>% mutate(Genre = fct_reorder(Genre, European))
In_Europe <- ggplot(data = InEurope, mapping = aes(x = European, y = Genre)) + geom_bar(stat = "identity",
                                                                                              fill = "navy",
                                                                                              colour = "white") +
  labs(title = "Popularity in Europe", x = "Values", y = "Genre") + theme_minimal()

# Region 4
Other_Region <- Games %>% mutate(Genre = fct_reorder(Genre, `Other Regions`))
OtherRegion <- ggplot(data = Other_Region, mapping = aes(x = `Other Regions`, y = Genre)) + geom_bar(stat = "identity",
                                                                                        fill = "white",
                                                                                        colour = "black") +
  labs(title = "Popularity in other regions", x = "Values", y = "Genre") + theme_minimal()

# Region 5
Globe <- Games %>% mutate(Genre = fct_reorder(Genre, Globally))
Globally <- ggplot(data = Globe, mapping = aes(x = Globally, y = Genre)) + geom_bar(stat = "identity",
                                                                                                  fill = "green",
                                                                                                  colour = "black") +
  labs(title = "Global Popularity", x = "Values", y = "Genre") + theme_minimal()

# Organize the visuals in a grid format using the grid.arrange function from the "GRID EXTRA" package.
grid.arrange(In_Japan,In_Europe,OtherRegion,Globally,In_north, nrow = 2, ncol = 3)

# Question 6
Platform_Sales <- VgSales %>% group_by(Platform = VgSales$Platform) %>% summarise(Average_Sales = round(mean(Global_Sales),1)) %>%
  arrange(desc(Average_Sales)) %>% head(1)
View(Platform_Sales)

# Question 7
Globe <- Games %>% mutate(Genre = fct_reorder(Genre, Globally))
Global_Sales <- ggplot(data = Globe, mapping = aes(x = Globally, y = Genre)) + geom_bar(stat = "identity",
                                                                                    fill = "darkblue",
                                                                                    colour = "darkgreen") +
  labs(title = "Global Sales", x = "Sales", y = "Genre") + theme_minimal()
distinct

# Question 8
# Based on the analysis from Question 5, the visuals show that the strategy genre consistently underperforms in four out of the five regions

# Question 10 
# Step 1
trendZ <- select(VgSales, Year)
trendZ <- distinct(trendZ, Year) %>% arrange(Year)
View(trendZ)

# Step 2
trendZ <- trendZ %>% filter(Year != "N/A" ) 
Trendz_Overyears <- select(VgSales,Year,Global_Sales )
Trendz_Overyears <- Trendz_Overyears %>% group_by(Years = Trendz_Overyears$Year) %>% filter(Year != "N/A") %>%
  summarise(Total = sum(Global_Sales))
View(Trendz_Overyears)
Trendz_Overyears$Years <- as.numeric(Trendz_Overyears$Years)

# Step 3 (Visualization)
Trend_Overtime <- ggplot(data = Trendz_Overyears, mapping = aes(x = Years, y = Total)) +
  geom_line(color = "black", size = 1.2) +  
  labs(title = "Global Sales Overtime",
       x = "Years",
       y = "Sales") +
  theme_minimal() 

# Question 10
# Step one (Checking for NA values)
sum(is.na(VgSales$Year))
sum(is.na(VgSales$Global_Sales))

# Step two (Cleaning the data)
VgSales_clean <- VgSales %>%
  filter(!is.na( VgSales$Year) & !is.na( VgSales$Global_Sales))

# Step three (Converting the Year column to numeric)
VgSales$Year<- as.numeric(VgSales$Year)

# Step Four (Summarizing the total sum per year)
sales_by_year <- VgSales_clean %>%
  group_by(Year) %>%
  summarise(Total_Sales = sum(Global_Sales, na.rm = TRUE)) %>%
  arrange(Year)

# Step Five (Visualizing the data)
ggplot(sales_by_year, aes(x = Year, y = Total_Sales, group = 1)) +  
  geom_line(color = "black", size = 1) +
  geom_point(color = "white") +
  labs(title = "Global Video Game Sales Over the Years",
       x = "Year of Release",
       y = "Global Sales (Millions)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Conclusion: From the line chart above, the sales of the game were at their highest in 2008 and at their lowest from 2017 to 2020, consecutively.

# Question 11
Sales_By_Publisher <- select(VgSales,Publisher,Global_Sales)
Sales_By_Publisher <- Sales_By_Publisher  %>% group_by(Publisher) %>% summarise(Total = sum(Global_Sales)) %>%
  arrange(desc(Total)) %>% head(10)
View(Sales_By_Publisher)

# Step 2 (Arrange the data for visuals)
Viz_By_Publisher <- Sales_By_Publisher %>% mutate(Publisher = fct_reorder(Publisher,Total))
# Step 2 (Visualization)
Viz_By_Publisher <- ggplot(data = Viz_By_Publisher, mapping = aes(x = Total, y = Publisher )) + geom_bar(stat = "identity",
                                                                                       fill = "black",
                                                                                       colour = "black") +
  labs(title = "Sales by Publishers", x = "Sales", y = "Publisher") + theme_minimal()
