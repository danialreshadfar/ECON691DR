library(tidycensus)
library(tidyverse)

var <- load_variables(2021, "acs5", cache = TRUE)
temp <- var %>% 
  #filter(grepl("MEDIAN INCOME", concept))        #I found out that I need B06011_001 from this code
  #filter(grepl("MEDIAN CONTRACT RENT", concept)) #I found out that I need B25058_001 from this code
  filter(grepl("MEDIAN", concept))                #I have both B06011_001 and B25058_001 in temp with this line

vars <- c("B06011_001", "B25058_001")

acs <- get_acs(geography = "county",              #defines geography level of data
               variables = vars,                  #specifies the data we want
               state =c(05,22,28,01),                         #Arkansas=05, Louisiana=22, Mississippi=28, Alabama=01 refrence:https://www2.census.gov/geo/docs/reference/state.txt
               year = 2021,                       #denotes the year
               geometry = TRUE)

core <- acs %>% 
  select(-moe) %>%
  #modifying columns
  mutate(variable = case_when(variable == "B06011_001" ~ "Med_Inc",
                              variable == "B25058_001" ~ "Med_Rent",
                              TRUE ~ variable)) %>%
  pivot_wider(id_cols = c("GEOID", "NAME", "geometry"), #Our data is long and I want to convert it to a wide-form data
              names_from = "variable",
              values_from = "estimate") %>%
  mutate(med_inc2 = Med_Inc/12,
         Ratio = Med_Rent/med_inc2,
         Afford= 0.33-Ratio)
#after running last 10 lines , I type head(core) in console to check my data
ggplot(core) +
  #fill = Afford means that we gonna be visualize afford  
  geom_sf(aes(fill=Afford))+
  scale_fill_gradient2() +
  theme_bw()

ggplot(core) +
  geom_sf(aes(fill=med_inc2))

ggplot(core) +
  geom_sf(aes(fill=Med_Rent))


#I use to code the line 14 like: sate=c(05,22,28,01) but I cannot find out how to change the code for plots to have each states map separately? Is creating maps for each state separately needed?
#Moreover, in the HW1 PDF file part 3, you mentioned "In the second and third maps, remove the scale_fill_gradient2() line of code and fill by the median income and median rent respectively."
#what does it mean? I created three maps for Afford,Med_Rent, and med_inc2 (monthly income). Is anything else needed?

#See Readme