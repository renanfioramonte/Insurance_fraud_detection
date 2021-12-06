# 1.0 Libraries -----------------------------------------------------------

library(corrplot)
library(tidyverse)
library(lubridate)
library(recipes)
library(skimr)
library(data.table)
library(forcats)

# 2.0 Uploading Dataset ---------------------------------------------------

dataassignment_tbl <- read_csv("assignment_data.csv")


# 3.0 Data Wrangling ------------------------------------------------------

df_toanalyse <- dataassignment_tbl %>%

    mutate(repaircost = as_factor(repaircost),
           passenger1 = as_factor(passenger1),
           passenger2 = as_factor(passenger2)) %>% 
    
    mutate(passenger1 = ifelse(is.na(passenger1), 0, 1),
           passenger2 = ifelse(is.na(passenger2), 0, 1)
    ) %>%
    
    separate(col = address, 
             into = c("addressdull", "address", "address_1"), 
             sep = " ",
             remove = TRUE) %>% 
    select(-addressdull, -address_1) %>%
    
    mutate(address = address %>% str_replace_all("[0-9[:punct:]!@£$%^&*()+_~=]", ""),
           repaircost = repaircost %>% str_replace_all("k", "000"),
           repaircost = repaircost %>% str_remove_all("[a-z[:punct:]*$~!]")) %>% 
    
    mutate(driver = driver %>% str_replace_all("[0-9[:punct:]!@£$%^&*()+_~=]", "")) %>% 
    
    mutate(address = case_when(
        address %like% "^CO" ~ "CORRIB",
        address %like% "^SH" ~ "SHANNON",
        address %like% "^LI" ~ "LIFFEY",
        address %like% "^LF" ~ "LIFFEY",
        address %like% "^BL" ~ "BLACKWATER",
        address %like% "^LEE" ~ "LEE",
        address %like% "^SL" ~ "SLANEY",
        address %like% "^BA" ~ "BARROW",
        address %like% "^SU" ~ "SUIR",
        address %like% "^CA" ~ "CAMAC",
        address %like% "^DOD" ~ "DODDER",
        address %like% "^SA" ~ "SAVERN",
        address %like% "^TH" ~ "THAMES",
        address %like% "^BO" ~ "BOYNE",
        address %like% "^BN" ~ "BOYNE",
        address %like% "^SE" ~ "SERN",
        address %like% "^TH" ~ "THAMES",
        address %like% "^TM" ~ "THAMES",
        TRUE ~ address
        )) %>% 
    mutate(address = as_factor(address)) %>% 
    
    drop_na() %>% 
    
    mutate(repaircost = case_when(
        repaircost == "5" ~ "500",
        repaircost == "0" ~ "500",
        repaircost == "2" ~ "500",
        TRUE ~ repaircost
    )) %>% 

    mutate(driver = case_when(
        driver %like% "^SHE" ~ "SHEEHAN",
        driver %like% "^OCONNE" ~ "OCONNEL",
        driver %like% "^HG" ~ "HIGGINS",
        TRUE ~ driver
    )) %>%     
    
    mutate(age_category =
               case_when(age <= 29 ~ "20 - 29", 
                         age <= 39 ~ "30 - 39",
                         age <= 49 ~ "40 - 49",
                         age <= 59 ~ "50 - 59",
                         age <= 69 ~ "60 - 69",
                         age <= 79 ~ "70 - 79",
                         age <= 90 ~ "80 - 89",)) %>%  
   

# 4.0 Feature Engineering -------------------------------------------------

    
    mutate(fraudFlag = case_when(
        fraudFlag == "TRUE" ~ 1,
        TRUE ~ 0)) %>% 
    
    mutate(driver_alone = case_when(
            passenger1 == 0 & passenger2 == 0 ~ 1,
            TRUE ~ 0)) 



# 5.0 EDA -----------------------------------------------------------------

#CORRELATION
test_corr <- df_toanalyse %>% select(fraudFlag, passenger1, passenger2, driver_alone)

firstcor <- cor(test_corr)

corrplot(firstcor, method = 'circle', type = 'lower', insig='blank',
         addCoef.col ='black', addgrid.col = "black", tl.srt = 45, 
         number.cex = 0.8, tl.col ="black", order = 'AOE', diag=FALSE)
    


# GRAPH SHOWING COUNT OF FRAUD FLAG PER AGE GROUP
ggplot(data = df_toanalyse, aes(x = age_category, y = fraudFlag)) +
    geom_col(aes(fill = age_category)) +
    theme_classic() +
    labs(title = "FRAUDS FLAGGED BY AGE GROUP", 
         subtitle = "Count of frauds by age group",
         fill = "AGE CATEGORY") +
    labs(x = "AGE CATEGORY", y = "NUMBER OF FRAUDS FLAGGED")

    


# GRAPH SHOWING AREAS CORRELATED WITH FRAUD FLAGS


ggplot(data = df_toanalyse, aes(x = address, y = fraudFlag)) +
    geom_col(aes(fill = address)) +
    scale_x_discrete(guide = guide_axis(angle = 90)) +
    labs(title = "FRAUDS FLAGGED BY AREA", 
         subtitle = "Number of insurance frauds grouped by area") +
    labs(x = "DRIVER'S AREA", y = "NUMBER OF FRAUDS FLAGGED", fill = "AREA") +
    theme_minimal()



#COUNT OF FRAUD PER REPAIR COST
plotting <- df_toanalyse %>% slice(-c(69, 346, 417))



ggplot(data = plotting, aes(x = reorder(repaircost, -fraudFlag), y = fraudFlag)) +
    geom_col(aes(fill = repaircost), width = 0.9) +
    theme_classic() +
    labs(title = "FRAUDS FLAGGED IN RELATION TO REPAIR COST", 
         subtitle = "Number of frauds flagged in relation to the repair cost",
         fill = "REPAIR COST") +
    labs(x = "REPAIR COST", y = "NUMBER OF FRAUDS FLAGGED")
