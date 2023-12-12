library(tidycensus)
library(tidyverse)
library(sf)
library(stargazer)

#Get a list of variables available to download
v17 <- load_variables(2021, "acs5", cache = TRUE)

#Filter the list of variables for what we are looking for.
data<-v17  %>%
  filter(grepl("ALLOCATION OF TRAVEL", concept))

#Access data from ACS 5-Year API
#data that we want: 1)Total Population: "B01001_001" 2)%Population White: "B02001_002" 3)%Population Black: "B02001_003" 4)%Population Asian: "B02001_005"
#%Population Other: Total-White-Black-Asian 6)Percent Population <18 7)Percent Population >64 8)Total Housing Units: "B25087_001"
#9)Housing with M:"B25087_002" 10)Median Owner Costs:"B25088_001" 11)Total Work Force:"B99084_001" 12)Workforce that works from Home:"B99084_005"
vars<-c("B01001_001", "B01001_003", "B01001_004", "B01001_005", "B01001_006",
        "B01001_020", "B01001_021", "B01001_022", "B01001_023", "B01001_024", "B01001_025",
        "B01001_027", "B01001_028", "B01001_029", "B01001_030",
        "B01001_044", "B01001_045", "B01001_046", "B01001_047", "B01001_048", "B01001_049",
        "B02001_002", "B02001_003", "B02001_005",
        "B25087_001", "B25087_002", "B25088_001",
        "B99084_001", "B99084_005",
        "B06011_001", "B25058_001")
  
#Command to pull data from ACS
years <- c(2018, 2019, 2020, 2021)

for(i in years){
  acs <- get_acs(geography = "county",	 #defines geography level of data 
                 variables = vars,	     #specifics the data we want 
                 state = c(05,22,28,01), #denotes the specific state 
                 year = i,	             #denotes the year
                 geometry = TRUE)     	 #downloads the TIGER shapefile data 
  core <- acs %>% 
    #modifying columns
    mutate(variable = case_when(variable == "B06011_001" ~ "Med_Inc",
                                variable == "B25058_001" ~ "Med_Rent",
                                variable == "B01001_001" ~ "Population",
                                variable == "B25088_001" ~ "Med_Cost",
                                TRUE ~ variable)) %>%
    select(-"moe") %>%
    pivot_wider(id_cols = c("GEOID", "NAME", "geometry"), names_from = "variable", values_from = "estimate") 
  core <- core %>%
    group_by(GEOID) %>%
    mutate(per_und17 = (sum(c_across(B01001_003:B01001_006))+sum(c_across(B01001_027:B01001_030)))/Population,
           per_ovr64 = (sum(c_across(B01001_020:B01001_025))+sum(c_across(B01001_044:B01001_049)))/Population,
           per_blk   = (B02001_003)/Population,
           per_wht   = (B02001_002)/Population,
           per_asn   = (B02001_005)/Population,
           per_oth   = 1 - per_wht - per_blk - per_asn,
           per_mort  = (B25087_002)/B25087_001,
           per_wfh   = (B99084_005)/B99084_001,
           m_monthly = Med_Inc/12,
           Rent_Share = Med_Rent/m_monthly,
           Affordable = .33 - Rent_Share,
           Population = Population/10000000,
           Med_Cost = Med_Cost/10000,
           Year = i) 
  ifelse(i==years[1], CORE <- core, CORE <- rbind(CORE, core))
}

#I did not add the percentage of white people owing to that I think we should not consider all 100% of categories as independent variables in our regression.
mod1 <- lm(Affordable ~ Population + per_und17 + per_ovr64 + per_blk + per_asn + per_oth + per_mort + per_wfh + Med_Cost,
           data = core)
mod2 <- lm(Rent_Share ~ Population + per_und17 + per_ovr64 + per_blk + per_asn + per_oth + per_mort + per_wfh + Med_Cost,
           data = core)
mod1a <- lm(Affordable ~ Population + per_und17 + per_ovr64 + per_blk + per_asn + per_oth + per_mort + per_wfh + Med_Cost +
              factor(Year),
            data = CORE)
mod2a <- lm(Rent_Share ~ Population + per_und17 + per_ovr64 + per_blk + per_asn + per_oth + per_mort + per_wfh + Med_Cost + 
              factor(Year),
            data = CORE)
mod1b <- lm(Affordable ~ Population + per_und17 + per_ovr64 + per_blk + per_asn + per_oth + per_mort + per_wfh + Med_Cost +
              factor(Year) + factor(GEOID),
            data = CORE)
mod2b <- lm(Rent_Share ~ Population + per_und17 + per_ovr64 + per_blk + per_asn + per_oth + per_mort + per_wfh + Med_Cost + 
              factor(Year) + factor(GEOID),
            data = CORE)
summary(mod1)
summary(mod2)
summary(mod1a)
summary(mod2a)
summary(mod1b)
summary(mod2b)

#Result Tables in Latex format
stargazer(mod1, type = "latex")
stargazer(mod2, type = "latex")
stargazer(mod1a, type = "latex")
stargazer(mod2a, type = "latex")
stargazer(mod1b, type = "latex")
stargazer(mod2b, type = "latex")

#Result Tables in HTML format  #This is all fine and good, but you did not save them so they just show as a coded mess on the console. Use the "out" option in stargazer to save.
stargazer(mod1, type = "html")
stargazer(mod2, type = "html")
stargazer(mod1a, type = "html")
stargazer(mod2a, type = "html")
stargazer(mod1b, type = "html")
stargazer(mod2b, type = "html", out="./mod2b.html")
