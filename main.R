library(tidyverse)
library(lubridate)
library(dbplyr)
library(openxlsx)
library(RPostgreSQL)

source("../misc_info.R")
source("../aux_functions.R")
source("excluded_legal_forms.R")
source("ou_indexing_functions.R")

#----------------------------------------------
# This script makes a number of breakdowns to 
# explore in which segments we 'over/under index'
# meaning where we have a relatively large/small
# number of signups compared to the overall 
# number of companies.
# The breakdowns are written to 
# (multiple sheets of) an Excel file.

conn <- dbConnect(drv = RPostgres::Postgres(),
                  dbname=db, 
                  host=host_db, 
                  port=db_port, 
                  user=db_user, 
                  password=db_password)

output_file <- "results/over_under_indexing_fc_de.xlsx"
curr_date <- Sys.Date()
two_years_ago <- ymd(curr_date) - years(2)
extra_time <- ymd("2019-10-01") - curr_date
two_years_ago_and_a_bit <- two_years_ago - extra_time
wb = createWorkbook()



#---
# Subquery selecting eligible prospects
elig_prosp <- dplyr::tbl(conn, in_schema("direct_mail", "prospects")) %>% 
  
  filter(country_id == 1, 
         foundation_date < two_years_ago,
         !(legal_form %in% excluded_legal_forms)) %>%
  
  left_join(tbl(conn, in_schema("quelle", "prospect_industries")) %>% 
               filter(is.null(known_industry_end_date)), 
             by = "prospect_id") %>% 
  left_join(tbl(conn, in_schema("quelle", "industries")) %>% 
               filter(!is_excluded),
             by = c("industry_id")) 

#---
# Breakdown by industry
ind_break <- elig_prosp %>% collect() %>% 
  get_perc_of_univ(c("industry_description"))

addWorksheet(wb, sheetName = "Industry")
writeData(wb, ind_break, sheet = "Industry", 
           startCol = 1)

#---
# Breakdown by company age
comp_age_break <- elig_prosp %>% collect() %>% 
  
                  mutate(comp_age = (curr_date - foundation_date)/365,
                        age_bucket = case_when(comp_age > 2 & comp_age <= 7  ~  '> 2, <= 5',
                                               comp_age > 5 & comp_age <= 7  ~ '> 5, <=7',
                                               comp_age > 7 & comp_age <= 10  ~ '> 7, <=10',
                                               comp_age > 10 & comp_age <= 20  ~ '> 10, <=20',
                                               comp_age > 20 ~ '> 20')) %>% 
  
                  get_perc_of_univ(c("age_bucket"))

comp_age_break <- comp_age_break[match(comp_age_break$age_bucket, 
                                  c("> 2, <= 5", "> 7, <=10", "> 10, <=20", "> 20")),]

addWorksheet(wb, sheetName = "Company Age")
writeData(wb, comp_age_break, sheet = "Company Age", 
          startCol = 1)


#---
# Breakdown based on contacst:
# Gender & Age

contact_data <- elig_prosp  %>% 
  
  left_join(tbl(conn, in_schema("direct_mail", "contacts")) %>% 
               filter(is_selected_contact == 1),
             by = c("prospect_id", "country_id")) %>% 
  
  dplyr::mutate(date_of_birth = substr(as.character(date_of_birth), 1, 10)) %>% 
  
  select(prospect_id, first_name, gender, funding_circle_identifier,
         date_of_birth) %>% 

  collect() 

# Gender: 
# first name is used to fill in some of the missing values
# based on some common German first names
c_first_names <- read.csv2("data/firstnames.csv", stringsAsFactors = F,
                           encoding="latin1") %>% 
  filter((!is.na(Germany)),
         !str_detect(name,"¼")) %>% 
  mutate(name = str_to_lower(name))

c_de_male_names <- c_first_names$name[c_first_names$gender == "M"]
c_de_fem_names <- c_first_names$name[c_first_names$gender == "F"]


gender_break <- contact_data %>%  
  
                mutate(first_name = str_to_lower(first_name), 
                       gender = ifelse((is.na(gender) & first_name %in% c_de_male_names),
                                       'M', gender),
                       gender = ifelse((is.na(gender) & first_name %in% c_de_fem_names),
                                       'F', gender)) %>%  
                
                filter(gender %in% c('M', 'F')) %>% 
                
                get_perc_of_univ(c("gender"))


addWorksheet(wb, sheetName = "Contacts")
writeData(wb, gender_break, sheet = "Contacts", 
          startCol = 1)


# Age
age_break <- contact_data %>% 
  
  dplyr::mutate(cont_age = (ymd(curr_date) - ymd(date_of_birth))/365,
                age_bucket = case_when(cont_age <= 30 ~  '<= 30',
                                       cont_age <= 40 & cont_age > 30 ~  '> 30, <= 40',
                                       cont_age > 40 & cont_age <= 50 ~  '> 40, <= 50',
                                       cont_age > 50 ~ '> 50')) %>% 
  
  get_perc_of_univ(c("age_bucket")) %>% 
  
  mutate(age_bucket = replace_na(age_bucket, "unknown"))


writeData(wb, age_break, sheet = "Contacts", 
          startRow = 8)

saveWorkbook(wb, file = output_file, 
             overwrite = T)

#---
# Breakdown based on location
# City (yes/no)
address_data <- elig_prosp  %>% 
  
  left_join(tbl(conn, in_schema("direct_mail", "addresses")) %>% 
              filter(is_current_address == 1),
            by = c("prospect_id", "country_id")) %>% 
  select(prospect_id, province, city, zipcode,
         funding_circle_identifier) %>% 
  collect()

#---
# Bundesland, East/West
zipcodes <- read.csv2("data/German-Zip-Codes.csv", stringsAsFactors = F,
                      encoding = "UTF-8") %>% 
  select(zipcode = Plz,
         bundesland = Bundesland) %>% 
  mutate(bundesland = str_replace(bundesland, "ü", "ue"),
         east_west = get_province_cat(bundesland),
         # dwh zipcodes have 0 padded before if zipcode is 4 digits
         # long
         zipcode = substr(paste0("0", zipcode), 2, 6))

address_data <- address_data %>% left_join(zipcodes, by = "zipcode") %>% 
                mutate(bundesland = ifelse(is.na(bundesland), province, bundesland),
                       bundesland = str_replace(bundesland, "ü", "ue"), 
                       bundesland = str_replace(bundesland, "Schlewig-Holstein", "Schleswig-Holstein"), 
                       east_west = get_province_cat(bundesland),
                       bundesland = ifelse(is.na(bundesland), "unknown", bundesland),
                       bundesland = ifelse(bundesland == "", "unknown", bundesland),
                       east_west = ifelse(is.na(east_west), "unknown", east_west)
                       ) %>% unique()

bundesland <- address_data %>% get_perc_of_univ(c("east_west", "bundesland"))
east_west <- address_data %>% get_perc_of_univ("east_west")


# Match city names in dwh to cities in file 
addWorksheet(wb, sheetName = "Bundesland")
writeData(wb, east_west, sheet = "Bundesland", 
          startCol = 1)

writeData(wb, bundesland, sheet = "Bundesland", 
          startRow = 8)

saveWorkbook(wb, file = output_file, 
             overwrite = T)


#----
# City yes/no
city_data <- read.xlsx("data/05-staedte.xlsx", sheet = 2, 
                       startRow = 8, colNames = F) %>% 
  
             setNames(c("lfd_nr", "land", "rb", "kreis", "vb", "gem", "city",
                        "zipcode", "surface_area_km", "pop_tot", "pop_men", 
                        "pop_fem", "pop_per_km")) %>% 
  
             mutate(city = str_extract(paste0(city, ","),
                                       "[a-zA-züößä\\. ()]*,"),
                    city = substr(city, 1, nchar(city) - 1),
                    city = str_replace_all(city, "ü", "ue"),
                    city = str_replace_all(city, "ß", "ss"),
                    city = str_replace_all(city, "ö", "oe"),
                    match_city = city)


city_matches <- data.frame(city = na.omit(unique(address_data$city))) %>% 
  
  mutate(city_simp = city,
         city_simp = str_replace_all(city_simp, "ü", "ue"),
         city_simp = str_replace_all(city_simp, "ß", "ss"),
         city_simp = str_replace_all(city_simp, "ö", "oe"))

city_matches$match_city <- unlist(lapply(city_matches$city_simp, get_match_city,
                                         ref_cities = city_data$city))
city_matches <- city_matches %>% mutate(match_city = ifelse(city %in% c("Franfkurt Am Main", 
                                                                        "Frankfurt/Main", "Frankfurt"),
                                                            "Frankfurt am Main", match_city))

city_break <- address_data %>% merge(city_matches, by = "city")
city_break_0 <- city_break %>% mutate(in_city = ifelse(!(match_city == "no match"), 
                                                       "yes", "no")) %>% 
  get_perc_of_univ("in_city")

addWorksheet(wb, sheetName = "City")
writeData(wb, city_break_0, sheet = "City", 
          startCol = 1)

saveWorkbook(wb, file = output_file, 
             overwrite = T)


city_break <- city_break %>% merge(city_data, by = "match_city")


city_break <- city_break %>% 

                mutate(pop_dens_bucket = case_when(pop_per_km <= 500 ~  '<= 500',
                                                   pop_per_km > 500 & pop_per_km <= 1000 ~  '> 500, <= 1000',
                                                   pop_per_km > 1000 & pop_per_km <= 2000 ~  '> 1000, <= 2000',
                                                   pop_per_km > 2000 ~ '> 2000'))

city_break_1 <- city_break %>% get_perc_of_univ("pop_dens_bucket")

writeData(wb, city_break_1, sheet = "City", 
          startRow = 8)
saveWorkbook(wb, file = output_file, 
             overwrite = T)
#city_break <- address_data %>% merge(city_data, all.x = T, by = c("city"))




