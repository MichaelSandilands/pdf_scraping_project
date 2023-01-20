library(pdftools)
library(tidyverse)
library(skimr)

pdf_files <- list.files("00_pdf/1_crude-oil-imports2019/") 

pdf_paths <- map2("00_pdf/1_crude-oil-imports2019/", pdf_files, str_c) %>% 
  unlist()

# I'm going to use the oldest and newest PDF files to develop the data extraction. 
# The idea here is that if it works for both the oldest and the newest it will (hopefully) generalize to all the others. 

oldest_pdf <- pdf_paths[1]
newest_pdf <- pdf_paths[length(pdf_paths)]

text_old <- pdftools::pdf_text(oldest_pdf)

# Each page is currently one long string. So as a first step I'll split the page by the new line regular expression, "\n".

# Inspect 1st page
text_old %>%
  str_split("\n") %>%
  pluck(1)

# Inspect intermediate page
text_old %>%
  str_split("\n") %>%
  pluck(5)

# Period changes from "01-12/2005" pattern to "Mon - 2005" pattern
# Conviently all the elements containing the data we're interested in end with "%"

text_new <- pdftools::pdf_text(newest_pdf)

# Inspect 1st page
text_new %>%
  str_split("\n") %>%
  pluck(1)

# Inspect intermediate page
text_new %>%
  str_split("\n") %>%
  pluck(5)

# Slight change in 1st period pattern. Went from "01-12/2005" to "1-12/2019". 2nd period pattern remains the same
# All data elements still end in "%"

text_old_split_newline <- str_split(text_old, "\n")
text_new_split_newline <- str_split(text_new, "\n")

### Extracting Period

# We need a way to id what period the data relates to. To do this I'm going to extract the period. There are two distinct patterns:

# 1. "1-12/year" or "01-12/year"
# 2. "Mon - year"

text_old_split_newline %>%
  map(
    ~ str_subset(.x,
                 # Use str_c and separate by "|" for better readability
                 str_c(
                   "\\d{1,2}-\\d{2}/\\d{4}$",   # "{one or two digits}-{exactly two digits}/{exactly four digits}{end of string}
                   "\\w{3}\\s-\\s\\d{4}$",      # "{exactly three word characters}{whitespace}-{whitespace}{exactly four digits}{end of string}
                   sep = "|"
                 )
    ) %>%
      # Remove everything from the element except the last ten characters.
      str_sub(-10, -1) # negative numbers count backwards from the end of the string
  )

# Works perfectly for the old pdf.

text_new_split_newline %>% 
  map(
    ~ str_subset(.x,
                 # Use str_c and separate by "|" for better readability
                 str_c(
                   "\\d{1,2}-\\d{2}/\\d{4}$",   # "{one or two digits}-{exactly two digits}/{exactly four digits}{end of string}
                   "\\w{3}\\s-\\s\\d{4}$",      # "{exactly three word characters}{whitespace}-{whitespace}{exactly four digits}{end of string}
                   sep = "|"
                 )
    ) %>%
      # Remove everything from the element except the last ten characters.
      str_sub(-10, -1) # negative numbers count backwards from the end of the string
  )

# Unfortunately the new pdf has some empty elements. `tidyr` has a function `fill` which can solve this problem.
# Notice the November period also has a typo. The same typo shows up for 2018.

period_old <- text_old_split_newline %>% 
  map(
    ~ str_subset(.x,
                 # Use str_c and separate by "|" for better readability
                 str_c(
                   "\\d{1,2}-\\d{2}/\\d{4}$", # "{one or two digits}-{exactly two digits}/{exactly four digits}{end of string}
                   "\\w{3}\\s-\\s\\d{3,4}$",  # "{exactly three word characters}{whitespace}-{whitespace}{three or four digits}{end of string}
                   sep = "|"
                 )
    ) %>%
      # Remove everything from the element except the last ten characters.
      str_sub(-10, -1) # negative numbers count backwards from the end of the string
  )

# Replace the empty elements with NA
period_old[lengths(period_old) == 0] <- NA_character_

period_old <- period_old %>%
  unlist() %>%
  # Convert to tibble so we can use `fill()` function
  as_tibble() %>%
  fill(value, .direction = "down") %>%
  # there is a typo in 2018 and 2019
  mutate(value = ifelse(str_detect(newest_pdf, "2018-01-12") & value == " Nov - 201", "Nov - 2018", value),
         value = ifelse(str_detect(newest_pdf, "2019-01-12") & value == " Nov - 201", "Nov - 2019", value)) %>% 
  pull(value) %>%
  as.list()

period_new <- text_new_split_newline %>% 
  map(
    ~ str_subset(.x,
                 # Use str_c and separate by "|" for better readability
                 str_c(
                   "\\d{1,2}-\\d{2}/\\d{4}$", # "{one or two digits}-{exactly two digits}/{exactly four digits}{end of string}
                   "\\w{3}\\s-\\s\\d{3,4}$",  # "{exactly three word characters}{whitespace}-{whitespace}{three or four digits}{end of string}
                   sep = "|"
                 )
    ) %>%
      # Remove everything from the element except the last ten characters.
      str_sub(-10, -1) # negative numbers count backwards from the end of the string
  )

# Replace the empty elements with NA
period_new[lengths(period_new) == 0] <- NA_character_

period_new <- period_new %>%
  unlist() %>%
  # Convert to tibble so we can use `fill()` function
  as_tibble() %>%
  fill(value, .direction = "down") %>%
  # there is a typo in 2018 and 2019
  mutate(value = ifelse(str_detect(newest_pdf, "2018-01-12") & value == " Nov - 201", "Nov - 2018", value),
         value = ifelse(str_detect(newest_pdf, "2019-01-12") & value == " Nov - 201", "Nov - 2019", value)) %>% 
  pull(value) %>%
  as.list()

period_new

# Inspecting the pages it seems that all the rows that have the data we're interested in end with "%".

text_old_data_subset <- map(text_old_split_newline, ~ str_subset(.x, "%$"))
text_new_data_subset <- map(text_new_split_newline, ~ str_subset(.x, "%$"))

text_old_data_subset %>% head(2)
text_new_data_subset %>% head(2)

# I think it will be easier to split right to left so I'm actually going to reverse the strings.

text_old_rev <- map(text_old_data_subset, ~ stringi::stri_reverse(.x))
text_new_rev <- map(text_new_data_subset, ~ stringi::stri_reverse(.x))

text_old_rev %>% head(2)
text_new_rev %>% head(2)

# The first split will occur at the first occurrence of a double white space, "\\s{2}". 

text_old_rev_split1 <- map(text_old_rev, ~ str_split_fixed(.x, "\\s{2}", n = 2))
text_new_rev_split1 <- map(text_new_rev, ~ str_split_fixed(.x, "\\s{2}", n = 2))

text_old_rev_split1 %>% head(2)
text_new_rev_split1 %>% head(2)

# Once we trim the left hand white space, the second split will also occur at the first occurrence of a double white space, "\\s{2}". 

text_old_rev_split2 <- map(
  text_old_rev_split1, 
  ~ cbind(
    .x[,1],     # We have to bind the split we already created or it will be dropped.
    .x[,2] %>%  
      str_trim(side = "left") %>% 
      str_split_fixed("\\s{2}", n = 2)
  )
)

text_new_rev_split2 <- map(
  text_new_rev_split1, 
  ~ cbind(
    .x[,1],     # We have to bind the split we already created or it will be dropped.
    .x[,2] %>%  
      str_trim(side = "left") %>% 
      str_split_fixed("\\s{2}", n = 2)
  )
)

text_old_rev_split2 %>% head(2)
text_new_rev_split2 %>% head(2)

# We have a problem now. Some of the elements only have a single white space between the next split.
# Again I'll trim the left hand side white space. 
# But now I'll explicitly enter the start and end of the elements to keep in this split and bind that to the remaining elements

text_old_rev_split3 <- map(
  text_old_rev_split2,
  ~ cbind(
    .x[,1], # 1st split
    .x[,2], # 2nd split
    # Elements for new split
    .x[,3] %>% 
      str_trim(side = "left") %>%
      str_sub(1, 11),
    # Remaining Elements
    .x[,3] %>%
      str_trim(side = "left") %>%
      str_sub(12, -1)
  )
)

text_new_rev_split3 <- map(
  text_new_rev_split2,
  ~ cbind(
    .x[,1], # 1st split
    .x[,2], # 2nd split
    # Elements for new split
    .x[,3] %>% 
      str_trim(side = "left") %>%
      str_sub(1, 11),
    # Remaining Elements
    .x[,3] %>%
      str_trim(side = "left") %>%
      str_sub(12, -1)
  )
)

text_old_rev_split3 %>% head(2)
text_new_rev_split3 %>% head(2)

# Once we trim the left hand white space, the fourth split will again occur at the first occurrence of a double white space, "\\s{2}". 

text_old_rev_split4 <- map(
  text_old_rev_split3,
  ~ cbind(
    .x[,1], # 1st split
    .x[,2], # 2nd split
    .x[,3], # 3rd split
    .x[,4] %>% 
      str_trim(side = "left") %>% 
      str_split_fixed("\\s{2}", n = 2)
  )
)

text_new_rev_split4 <- map(
  text_new_rev_split3,
  ~ cbind(
    .x[,1], # 1st split
    .x[,2], # 2nd split
    .x[,3], # 3rd split
    .x[,4] %>% 
      str_trim(side = "left") %>% 
      str_split_fixed("\\s{2}", n = 2)
  )
)

text_old_rev_split4 %>% head(2)
text_new_rev_split4 %>% head(2)

# I think the remaining splits will be easier in a tibble format and in correct order (not reverse order)

col_names <- c("pct_of_total_imports", "cif_price", "total_value", "volume", "region_country_type_of_crude")

text_old_tibble <- text_old_rev_split4 %>% 
  map2(
    .y = period_old,
    # Convert to tibble
    ~ as_tibble(.x, .name_repair = "minimal") %>% 
      # Rename Columns
      set_names(col_names) %>% 
      # Reverse back to correct order
      mutate(across(everything(), stringi::stri_reverse)) %>% 
      # Add period column
      mutate(period = .y)
  ) %>% 
  bind_rows() %>% 
  # Reverse Column Order
  select(ncol(.):1)

text_new_tibble <- text_new_rev_split4 %>% 
  map2(
    .y = period_new,
    # Convert to tibble
    ~ as_tibble(.x, .name_repair = "minimal") %>% 
      # Rename Columns
      set_names(col_names) %>% 
      # Reverse back to correct order
      mutate(across(everything(), stringi::stri_reverse)) %>% 
      # Add period column
      mutate(period = .y)
  ) %>% 
  bind_rows() %>% 
  # Reverse Column Order
  select(ncol(.):1)

# The first character in the region_country_type_of_crude column is always either white space or the regions. 
# Conveniently, other than the region the elements that start with a character have only whitespace.
# What's more is that if we can extract the region we can reuse the tidyr `fill()` function using the direction up to cleanly remove all the NA values
# My first step is to get all the regions.

regions_old <- text_old_tibble %>% 
  select(region_country_type_of_crude) %>% 
  # Filter to only the rows starting with a word character ("\\w")
  filter(str_detect(region_country_type_of_crude, "^\\w")) %>% # "^" is the regex for "starts with"
  # Trim whitespace
  mutate(region_country_type_of_crude = region_country_type_of_crude %>% str_trim()) %>% 
  # Get the unique regions
  distinct() %>% 
  # place in vector format
  pull()

regions_new <- text_new_tibble %>% 
  select(region_country_type_of_crude) %>% 
  # Filter to only the rows starting with a word character ("\\w")
  filter(str_detect(region_country_type_of_crude, "^\\w")) %>% # "^" is the regex for "starts with"
  # Trim whitespace
  mutate(region_country_type_of_crude = region_country_type_of_crude %>% str_trim()) %>% 
  # Get the unique regions
  distinct() %>% 
  # place in vector format
  pull()

regions_old
regions_new

# While not in the oldest and newest PDF, Asia and Other are also regions.
# We have a problem now where "Middle East" is not getting returned in the regions_new even though it's present in the pdf.

text_new_tibble %>% 
  filter(row_number() == 63)
# this row should have "Middle East" in it but it contains only white space instead.

# The solution here is to find the rows where there are only white space in the region_country_type_of_crude column 
# and replace the value of region_country_type_of_crude to "Middle East". 
# This solution will not work in the 2006 PDF and the 2012 PDF.
# For 2006 the solution is to fill the rows with only white space as "Other"
# For 2012 I can't think of a solution other than manually changing the error rows to "Other"

text_old_tibble_fix_middle_east <- text_old_tibble %>% 
  mutate(
    # Middle East fix
    region_country_type_of_crude = ifelse(
      !str_detect(region_country_type_of_crude, "\\w") & !str_detect(oldest_pdf, "2006"),
      "Middle East",
      region_country_type_of_crude
    ),
    # 2006 Other fix
    region_country_type_of_crude = ifelse(
      !str_detect(region_country_type_of_crude, "\\w") & str_detect(oldest_pdf, "2006"),
      "Other",
      region_country_type_of_crude
    ),
  )

text_new_tibble_fix_middle_east <- text_new_tibble %>% 
  mutate(
    # Middle East fix
    region_country_type_of_crude = ifelse(
      !str_detect(region_country_type_of_crude, "\\w") & !str_detect(newest_pdf, "2006"),
      "Middle East",
      region_country_type_of_crude
    ),
    # 2006 Other fix
    region_country_type_of_crude = ifelse(
      !str_detect(region_country_type_of_crude, "\\w") & str_detect(newest_pdf, "2006"),
      "Other",
      region_country_type_of_crude
    ),
  )

#############################
#############################
# TODO: FIX 2012 "Other" ROWS
#############################
#############################

# Because text_old_tibble has "Middle East" it shouldn't change with this fix
identical(text_old_tibble, text_old_tibble_fix_middle_east)

# Get regions again
regions_old <- text_old_tibble_fix_middle_east %>% 
  select(region_country_type_of_crude) %>% 
  # Filter to only the rows starting with a word character ("\\w")
  filter(str_detect(region_country_type_of_crude, "^\\w")) %>% # "^" is the regex for "starts with"
  # Trim whitespace
  mutate(region_country_type_of_crude = region_country_type_of_crude %>% str_trim()) %>% 
  # Get the unique regions
  distinct() %>% 
  # place in vector format
  pull()

regions_new <- text_new_tibble_fix_middle_east %>% 
  select(region_country_type_of_crude) %>% 
  # Filter to only the rows starting with a word character ("\\w")
  filter(str_detect(region_country_type_of_crude, "^\\w")) %>% # "^" is the regex for "starts with"
  # Trim whitespace
  mutate(region_country_type_of_crude = region_country_type_of_crude %>% str_trim()) %>% 
  # Get the unique regions
  distinct() %>% 
  # place in vector format
  pull()

regions_old
regions_new


# Add the regions column using a `case_when()` function

text_old_tibble_add_region <- text_old_tibble_fix_middle_east %>% 
  mutate(
    region = case_when(
      str_detect(region_country_type_of_crude, "^Middle East") ~ "Middle East",
      str_detect(region_country_type_of_crude, "^Africa") ~ "Africa",
      str_detect(region_country_type_of_crude, "^Australia") ~ "Australia",
      str_detect(region_country_type_of_crude, "^FSU") ~ "FSU",
      str_detect(region_country_type_of_crude, "^Europe") ~ "Europe",
      str_detect(region_country_type_of_crude, "^America") ~ "America",
      str_detect(region_country_type_of_crude, "^Asia") ~ "Asia",
      str_detect(region_country_type_of_crude, "^Other") ~ "Other",
      str_detect(region_country_type_of_crude, "^World") ~ "World",
      TRUE ~ NA_character_
    )
  ) %>% 
  fill(region, .direction = "up")

text_new_tibble_add_region <- text_new_tibble_fix_middle_east %>% 
  mutate(
    region = case_when(
      str_detect(region_country_type_of_crude, "^Middle East") ~ "Middle East",
      str_detect(region_country_type_of_crude, "^Africa") ~ "Africa",
      str_detect(region_country_type_of_crude, "^Australia") ~ "Australia",
      str_detect(region_country_type_of_crude, "^FSU") ~ "FSU",
      str_detect(region_country_type_of_crude, "^Europe") ~ "Europe",
      str_detect(region_country_type_of_crude, "^America") ~ "America",
      str_detect(region_country_type_of_crude, "^World") ~ "World",
      str_detect(region_country_type_of_crude, "^Asia") ~ "Asia",
      str_detect(region_country_type_of_crude, "^Other") ~ "Other",
      TRUE ~ NA_character_
    )
  ) %>% 
  fill(region, .direction = "up")

text_old_tibble_add_region %>% head(20)
text_new_tibble_add_region %>% head(20)

text_old_tibble_add_region %>% tail(20)
text_new_tibble_add_region %>% tail(20)

# We need to remove the regions from the region_country_type_of_crude column now.
# Sometimes the country column begins with other. 
# Again there's a double white space between region and country.
# I'll use this to cleanly remove other.

regions_old <- ifelse(regions_old == "Other", "Other  ", regions_old)
regions_new <- ifelse(regions_new == "Other", "Other  ", regions_new)

text_old_tibble_remove_region_from_region_country_type_of_crude <- text_old_tibble_add_region %>% 
  mutate(
    region_country_type_of_crude = region_country_type_of_crude %>% 
      str_trim(side = "left") %>% 
      # "^" is the regex for "starts with"
      str_remove(str_c("^", regions_old, collapse = "|")) %>% 
      str_trim()
  ) %>% 
  # Rename now that region is removed
  rename(country_type_of_crude = region_country_type_of_crude)

text_new_tibble_remove_region_from_region_country_type_of_crude <- text_new_tibble_add_region %>% 
  mutate(
    region_country_type_of_crude = region_country_type_of_crude %>% 
      str_trim(side = "left") %>% 
      # "^" is the regex for "starts with"
      str_remove(str_c("^", regions_new, collapse = "|")) %>% 
      str_trim()
  ) %>% 
  # Rename now that region is removed
  rename(country_type_of_crude = region_country_type_of_crude)

text_old_tibble_remove_region_from_region_country_type_of_crude %>% head(20)
text_new_tibble_remove_region_from_region_country_type_of_crude %>% head(20)

# Now we need to perform the final split. This time, instead of using `str_split_fixed()`, tidyr's `separate()` is used.

text_old_tibble_final_split_and_trim <- text_old_tibble_remove_region_from_region_country_type_of_crude %>% 
  separate(
    country_type_of_crude, 
    # Name of new columns
    into = c("country", "type_of_crude"), 
    # Separate at 2 or more whitespace
    sep = "\\s{2,}", 
    # Fill NAs on the left column, "country"
    fill = "left"
  ) %>% 
  mutate(
    across(
      everything(),
      str_trim        # trim the trailing and leading whitespace in every column
    )
  )

text_new_tibble_final_split_and_trim <- text_new_tibble_remove_region_from_region_country_type_of_crude %>% 
  separate(
    country_type_of_crude, 
    # Name of new columns
    into = c("country", "type_of_crude"), 
    # Separate at 2 or more whitespace
    sep = "\\s{2,}", 
    # Fill NAs on the left column, "country"
    fill = "left"
  ) %>% 
  mutate(
    across(
      everything(),
      str_trim        # trim the trailing and leading whitespace in every column
    )
  )

text_old_tibble_final_split_and_trim %>% head(20)
text_new_tibble_final_split_and_trim %>% head(20)

text_old_tibble_final_split_and_trim %>% tail(20)
text_new_tibble_final_split_and_trim %>% tail(20)

# I think the country column will actually be easier to clean with all the PDFs joined together.

# time to create a function

import_and_clean_crude_pdf <- function(pdf) {
  
  text <- pdftools::pdf_text(pdf)
  
  # Each page is currently one long string. So as a first step I'll split the page by the new line regular expression, "\n".
  
  text_split_newline <- str_split(text, "\n")
  
  ### Extracting Period
  
  # We need a way to id what period the data relates to. To do this I'm going to extract the period. There are two distinct patterns:
  
  # 1. "1-12/year" or "01-12/year"
  # 2. "Mon - year"
  
  period <- text_split_newline %>% 
    map(
      ~ str_subset(.x,
                   # Use str_c and separate by "|" for better readability
                   str_c(
                     "\\d{1,2}-\\d{2}/\\d{4}$", # "{one or two digits}-{exactly two digits}/{exactly four digits}{end of string}
                     "\\w{3}\\s-\\s\\d{3,4}$",  # "{exactly three word characters}{whitespace}-{whitespace}{three or four digits}{end of string}
                     sep = "|"
                   )
      ) %>%
        # Remove everything from the element except the last ten characters.
        str_sub(-10, -1) # negative numbers count backwards from the end of the string
    )
  
  # Replace the empty elements with NA
  period[lengths(period) == 0] <- NA_character_
  
  period <- period %>%
    unlist() %>%
    # Convert to tibble so we can use `fill()` function
    as_tibble() %>%
    fill(value, .direction = "down") %>%
    # there is a typo in 2018 and 2019
    mutate(value = ifelse(str_detect(newest_pdf, "2018-01-12") & value == " Nov - 201", "Nov - 2018", value),
           value = ifelse(str_detect(newest_pdf, "2019-01-12") & value == " Nov - 201", "Nov - 2019", value)) %>% 
    pull(value) %>%
    as.list()
  
  # Inspecting the pages it seems that all the rows that have the data we're interested in end with "%".
  
  text_data_subset <- map(text_split_newline, ~ str_subset(.x, "%$"))
  
  # I think it will be easier to split right to left so I'm actually going to reverse the strings.
  
  text_rev <- map(text_data_subset, ~ stringi::stri_reverse(.x))
  
  # The first split will occur at the first occurrence of a double white space, "\\s{2}". 
  
  text_rev_split1 <- map(text_rev, ~ str_split_fixed(.x, "\\s{2}", n = 2))
  
  # Once we trim the left hand white space, the second split will also occur at the first occurrence of a double white space, "\\s{2}". 
  
  text_rev_split2 <- map(
    text_rev_split1, 
    ~ cbind(
      .x[,1],     # We have to bind the split we already created or it will be dropped.
      .x[,2] %>%  
        str_trim(side = "left") %>% 
        str_split_fixed("\\s{2}", n = 2)
    )
  )
  
  # We have a problem now. Some of the elements only have a single white space between the next split.
  # Again I'll trim the left hand side white space. 
  # But now I'll explicitly enter the start and end of the elements to keep in this split and bind that to the remaining elements
  
  text_rev_split3 <- map(
    text_rev_split2,
    ~ cbind(
      .x[,1], # 1st split
      .x[,2], # 2nd split
      # Elements for new split
      .x[,3] %>% 
        str_trim(side = "left") %>%
        str_sub(1, 11),
      # Remaining Elements
      .x[,3] %>%
        str_trim(side = "left") %>%
        str_sub(12, -1)
    )
  )
  
  # Once we trim the left hand white space, the fourth split will again occur at the first occurrence of a double white space, "\\s{2}". 
  
  text_rev_split4 <- map(
    text_rev_split3,
    ~ cbind(
      .x[,1], # 1st split
      .x[,2], # 2nd split
      .x[,3], # 3rd split
      .x[,4] %>% 
        str_trim(side = "left") %>% 
        str_split_fixed("\\s{2}", n = 2)
    )
  )
  
  # I think the remaining splits will be easier in a tibble format and in correct order (not reverse order)
  
  col_names <- c("pct_of_total_imports", "cif_price", "total_value", "volume", "region_country_type_of_crude")
  
  text_tibble <- text_rev_split4 %>% 
    map2(
      .y = period,
      # Convert to tibble
      ~ as_tibble(.x, .name_repair = "minimal") %>% 
        # Rename Columns
        set_names(col_names) %>% 
        # Reverse back to correct order
        mutate(across(everything(), stringi::stri_reverse)) %>% 
        # Add period column
        mutate(period = .y)
    ) %>% 
    bind_rows() %>% 
    # Reverse Column Order
    select(ncol(.):1)
  
  # The first character in the region_country_type_of_crude column is always either white space or the regions. 
  # Conveniently, other than the region the elements that start with a character have only white space.
  # What's more is that if we can extract the region we can reuse the tidyr `fill()` function using the direction up to cleanly remove all the NA values
  # My first step is to get all the regions.
  
  # While not in the oldest and newest PDF, Asia and Other are also regions.
  # We have a problem now where "Middle East" is not getting returned in the regions_new even though it's present in the pdf.
  
  # The solution here is to find the rows where there are only white space in the region_country_type_of_crude column 
  # and replace the value of region_country_type_of_crude to "Middle East". 
  # This solution will not work in the 2006 PDF and the 2012 PDF.
  # For 2006 the solution is to fill the rows with only white space as "Other"
  # For 2012 I can't think of a solution other than manually changing the error rows to "Other"
  
  text_tibble_fix_middle_east <- text_tibble %>% 
    mutate(
      # Middle East fix
      region_country_type_of_crude = ifelse(
        !str_detect(region_country_type_of_crude, "\\w") & !str_detect(pdf, "2006"),
        "Middle East",
        region_country_type_of_crude
      ),
      # 2006 Other fix
      region_country_type_of_crude = ifelse(
        !str_detect(region_country_type_of_crude, "\\w") & str_detect(pdf, "2006"),
        "Other  ",
        region_country_type_of_crude
      ),
    )
  
  #############################
  #############################
  # TODO: FIX 2012 "Other" ROWS
  #############################
  #############################
  
  regions <- text_tibble_fix_middle_east %>% 
    select(region_country_type_of_crude) %>% 
    # Filter to only the rows starting with a word character ("\\w")
    filter(str_detect(region_country_type_of_crude, "^\\w")) %>% # "^" is the regex for "starts with"
    # Trim whitespace
    mutate(region_country_type_of_crude = region_country_type_of_crude %>% str_trim()) %>% 
    # Get the unique regions
    distinct() %>% 
    # place in vector format
    pull()
  
  # Add the regions column using a `case_when()` function
  
  text_tibble_add_region <- text_tibble_fix_middle_east %>% 
    mutate(
      region = case_when(
        str_detect(region_country_type_of_crude, "^Middle East") ~ "Middle East",
        str_detect(region_country_type_of_crude, "^Africa") ~ "Africa",
        str_detect(region_country_type_of_crude, "^Australia") ~ "Australia",
        str_detect(region_country_type_of_crude, "^FSU") ~ "FSU",
        str_detect(region_country_type_of_crude, "^Europe") ~ "Europe",
        str_detect(region_country_type_of_crude, "^America") ~ "America",
        str_detect(region_country_type_of_crude, "^Asia") ~ "Asia",
        str_detect(region_country_type_of_crude, "^Other") ~ "Other",
        str_detect(region_country_type_of_crude, "^World") ~ "World",
        TRUE ~ NA_character_
      )
    ) %>% 
    fill(region, .direction = "up")
  
  # We need to remove the regions from the region_country_type_of_crude column now.
  # Sometimes the country column begins with other. 
  # Again there's a double white space between region and country.
  # I'll use this to cleanly remove other.
  
  regions <- ifelse(regions == "Other", "Other  ", regions)
  
  text_tibble_remove_region_from_region_country_type_of_crude <- text_tibble_add_region %>% 
    mutate(
      region_country_type_of_crude = region_country_type_of_crude %>% 
        str_trim(side = "left") %>% 
        # "^" is the regex for "starts with"
        str_remove(str_c("^", regions, collapse = "|")) %>% 
        str_trim()
    ) %>% 
    # Rename now that region is removed
    rename(country_type_of_crude = region_country_type_of_crude)
  
  # Now we need to perform the final split. This time, instead of using `str_split_fixed()`, tidyr's `separate()` is used.
  
  text_tibble_final_split_and_trim <- text_tibble_remove_region_from_region_country_type_of_crude %>% 
    separate(
      country_type_of_crude, 
      # Name of new columns
      into = c("country", "type_of_crude"), 
      # Separate at 2 or more whitespace
      sep = "\\s{2,}", 
      # Fill NAs on the left column, "country"
      fill = "left"
    ) %>% 
    mutate(
      across(
        everything(),
        str_trim        # trim the trailing and leading white space in every column
      )
    )
  
}


# Combining all of the PDFs together
crude_imports_messy_country_tbl <- map(pdf_paths, import_and_clean_crude_pdf) %>% 
  bind_rows()

# Using skimr package to summarize missing values
crude_imports_messy_country_tbl %>% skim() # there are 4999 missing values in the country column

# Missing Value Fix 1

# When the type_of_crude column contains an empty string, i.e. "", and country is NA, replace country with ""
crude_imports_fix1_tbl <- crude_imports_messy_country_tbl %>% 
  mutate(country = ifelse(type_of_crude == "" & is.na(country), "", country))

crude_imports_fix1_tbl %>% skim() # down to 3774 missing values

# Missing Value Fix 2

# OK so this is hard to explain
# Step 1: 
#   - Find all the unique combinations of country and type of crude.
# Step 2: 
#   - Count the number of times each type_of_crude appears
# Step 3: 
#   - Filter so that the type_of_crude count is 2. This will give you one of two possibilities:
#       - A combination of two countries and a type of crude.
#       - A combination of a country and a Missing Value and a type of crude. This means that the type of crude only has ONE country associated with it not two or more. 
#         
# Now, this won't work if there is in fact two or more countries associated with this type of crude but those countries have ubiquitous missing values across that type_of_crude.
# Looking at the data I do think this is very unlikely, there is a lot of variance Month to Month with the missing values and having a country type_of_crude combination completely missed seems very unlikely.
# Just know that it is possible that values can be mislabeled but it should be very rare.
#
# Step 4:
#   - Arrange by type of crude first, country second. This will mean that the missing value is below the country.
# Step 5:
#   - use tidyr's `fill()` function with .direction = "down"
# Step 6:
#   - rename country to something else, i.e. "country_fill"
# Step 7:
#   - Join this data to our full dataset using a left join by type_of_crude, full data on the left our country_fill table on the right. 
# Step 8:
#   - replace the missing values in country with country_fill.


fix_2 <- crude_imports_fix1_tbl %>% 
  distinct(country, type_of_crude) %>% 
  group_by(type_of_crude) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  filter(n == 2) %>% 
  arrange(type_of_crude, country) %>% 
  fill(country, .direction = "down") %>% 
  distinct(country, type_of_crude) %>% 
  rename(country_fill = country)

crude_imports_fix2_tbl <- crude_imports_fix1_tbl %>% 
  left_join(fix_2, by = "type_of_crude") %>% 
  mutate(country = ifelse(is.na(country), country_fill, country)) %>% 
  select(-country_fill)

crude_imports_fix2_tbl %>% skim()

# Missing Value Fix 3

missing_toc <- crude_imports_fix2_tbl %>%
  filter(is.na(country)) %>% 
  distinct(type_of_crude) %>% 
  pull()

crude_imports_fix2_tbl %>% 
  filter(type_of_crude %in% missing_toc) %>% 
  distinct(country, type_of_crude) %>% 
  arrange(type_of_crude)

# I can assign Egypt to "Egyptian Heavy (<30o)" type of crude

# Libyan Arab Jamahiriya and Libya both refer to the same geographic location. 
# reference: https://www.un.org/en/about-us/member-states/libya#:~:text=Following%20the%20adoption%20by%20the,Libya%22%20and%20changing%20Libya's%20national
# I'm going to change "Libyan Arab Jamahiriya" to "Libya" I can assign the NA values for "Light (>40o)" type_of_crude to Libya.
# The "Heavy (<30o API)" still presents a challenge though.

# Exploring the PDFs I discovered Nigeria should be assigned to "Medium (" type of crude.

# I don't think Cameroon should have "Other Algeria Crude" as a possible type_of_crude. 
# This isn't an error of the data cleaning process. This appears in the 2014 PDF
# I'm not going to change the type_of_crude for Cameroon. 
# But I am going to assign Algeria to any missing values associated with "Other Algeria Crude"

# I'm going to assign VietNam to "Other Vietnam Crude"

crude_imports_fix3_tbl <- crude_imports_fix2_tbl %>% 
  mutate(country = ifelse(country == "Libyan Arab Jamahiriya", "Libya", country),
         country = case_when(type_of_crude == "Egyptian Heavy (<30o)" ~ "Egypt",
                             type_of_crude == "Light (>40o)"          ~ "Libya",
                             type_of_crude == "Medium ("              ~ "Nigeria",
                             type_of_crude == "Other Algeria Crude"   ~ "Algeria",
                             type_of_crude == "Other Vietnam Crude"   ~ "VietNam",
                             TRUE ~ country))

crude_imports_fix3_tbl %>% skim()

# Missing Value Fix 4

# When the leading value of the missing country value is equal to "Egypt" and the lagging value does not equal NA or "Libya" then country is Egypt
# When the leading value of the missing country value is equal to "Libya" and the lagging value does not equal NA or "Egypt" then country is Libya
# If that fails move to lagging values
# When the lagging value of the missing country value is equal to "Egypt" and the leading value does not equal NA or "Libya" then country is Egypt
# When the lagging value of the missing country value is equal to "Libya" and the leading value does not equal NA or "Egypt" then country is Libya

crude_imports_no_na_tbl <- crude_imports_fix3_tbl %>% 
  mutate(country = case_when(is.na(country) & lead(country) == "Egypt" & lag(country) != "Libya" & !is.na(lag(country)) ~ "Egypt",
                             is.na(country) & lead(country) == "Libya" & lag(country) != "Egypt" & !is.na(lag(country)) ~ "Libya",
                             is.na(country) & lag(country) == "Egypt" & lead(country) != "Libya" & !is.na(lead(country)) ~ "Egypt",
                             is.na(country) & lag(country) == "Libya" & lead(country) != "Egypt" & !is.na(lead(country)) ~ "Libya",
                             TRUE ~ country))

crude_imports_no_na_tbl %>% skim()

# Inspecting results

# 2005 PDF - No obvious errors

crude_imports_no_na_tbl %>% 
  select(period, region, everything()) %>% 
  filter(period == "01-12/2005") %>% View()

# 2006 PDF - No obvious errors

crude_imports_no_na_tbl %>% 
  select(period, region, everything()) %>% 
  filter(period == "01-12/2006") %>% View()

# 2007 PDF - No obvious errors

crude_imports_no_na_tbl %>% 
  select(period, region, everything()) %>% 
  filter(period == "01-12/2007") %>% View()

# 2008 PDF - No obvious errors

crude_imports_no_na_tbl %>% 
  select(period, region, everything()) %>% 
  filter(period == "01-12/2008") %>% View()

# 2009 PDF - No obvious errors

crude_imports_no_na_tbl %>% 
  select(period, region, everything()) %>% 
  filter(period == "1-12/2009") %>% View()

# 2010 PDF - No obvious errors

crude_imports_no_na_tbl %>% 
  select(period, region, everything()) %>% 
  filter(period == "1-12/2010") %>% View()

# 2011 PDF - No obvious errors

crude_imports_no_na_tbl %>% 
  select(period, region, everything()) %>% 
  filter(period == "1-12/2011") %>% View()

# 2012 PDF - Mislabeled Libya when it should be Egypt, still have to fix Other region

crude_imports_no_na_tbl %>% 
  select(period, region, everything()) %>% 
  filter(period == "1-12/2012") %>% View()

# 2013 PDF - Mislabeled Libya when it should be Egypt

crude_imports_no_na_tbl %>% 
  select(period, region, everything()) %>% 
  filter(period == "1-12/2013") %>% View()

# 2014 PDF - No obvious errors

crude_imports_no_na_tbl %>% 
  select(period, region, everything()) %>% 
  filter(period == "1-12/2014") %>% View()

# 2015 PDF - No obvious errors

crude_imports_no_na_tbl %>% 
  select(period, region, everything()) %>% 
  filter(period == "1-12/2015") %>% View()

# 2016 PDF - No obvious errors

crude_imports_no_na_tbl %>% 
  select(period, region, everything()) %>% 
  filter(period == "1-12/2016") %>% View()

# 2017 PDF - No obvious errors

crude_imports_no_na_tbl %>% 
  select(period, region, everything()) %>% 
  filter(period == "1-12/2017") %>% View()

# 2018 PDF - No obvious errors

crude_imports_no_na_tbl %>% 
  select(period, region, everything()) %>% 
  filter(period == "1-12/2018") %>% View()

# 2019 PDF - No obvious errors

crude_imports_no_na_tbl %>% 
  select(period, region, everything()) %>% 
  filter(period == "1-12/2019") %>% View()

# Fix Egypt Mislabel and 2012 Other Mislabel

crude_imports_untidy_numeric_tbl <- crude_imports_no_na_tbl %>% 
  select(period, region, everything()) %>% 
  mutate(
    country = ifelse(
      str_detect(period, "2012|2013") & country == "Libya" & lead(country) == "Egypt",
      "Egypt", 
      country
    ),
    region = ifelse(
      str_detect(period, "2012") & region == "Middle East" & lead(region) == "World" | country == "Other Countries",
      "Other",
      region
    )
  )

# Fix numeric columns

crude_imports_tidy_tbl <- crude_imports_untidy_numeric_tbl %>% 
  mutate(
    across(
      5:8, 
      ~ str_remove_all(.x, "%|\\s") %>% # Remove % and all white space
        str_replace_all(",", ".") %>%   # Replace all commas with decimal points
        as.numeric()                    # Convert to numeric
    )
  ) %>% 
  mutate(pct_of_total_imports = pct_of_total_imports/100) %>% # Convert to proportion
  rename(prop_of_total_imports = pct_of_total_imports)        # Rename

# Exporting Clean Data

crude_imports_by_year_tbl <- crude_imports_tidy_tbl %>% 
  filter(str_detect(period, "\\d{1,2}-\\d{2}/\\d{4}")) %>% 
  mutate(period = str_remove(period, "^\\d{1,2}-\\d{2}/") %>% as.numeric()) %>% 
  rename(year = period)

crude_imports_region_totals_year_tbl <- crude_imports_by_year_tbl %>% 
  filter(country == "", type_of_crude == "") %>% 
  select(-country, -type_of_crude)

crude_imports_by_year_country_type_of_crude_tbl <- crude_imports_by_year_tbl %>% 
  filter(country != "", type_of_crude != "")

nrow(crude_imports_region_totals_year_tbl) + nrow(crude_imports_by_year_country_type_of_crude_tbl) == nrow(crude_imports_by_year_tbl)

crude_imports_by_month_year_tbl <- crude_imports_tidy_tbl %>% 
  filter(!str_detect(period, "\\d{1,2}-\\d{2}/\\d{4}")) %>% 
  mutate(period = lubridate::my(period)) %>% 
  rename(month_year = period)

crude_imports_region_totals_by_month_year_tbl <- crude_imports_by_month_year_tbl %>% 
  filter(country == "", type_of_crude == "") %>% 
  select(-country, -type_of_crude)

crude_imports_by_month_year_country_type_of_crude_tbl <- crude_imports_by_month_year_tbl %>% 
  filter(country != "", type_of_crude != "")

nrow(crude_imports_region_totals_by_month_year_tbl) + nrow(crude_imports_by_month_year_country_type_of_crude_tbl) == nrow(crude_imports_by_month_year_tbl)

write_csv(crude_imports_region_totals_year_tbl, "./00_data/crude_imports_region_totals_year.csv")

write_csv(crude_imports_by_year_country_type_of_crude_tbl, "./00_data/crude_imports_by_year_country_type_of_crude.csv")

write_csv(crude_imports_region_totals_by_month_year_tbl, "./00_data/crude_imports_region_totals_by_month_year.csv")

write_csv(crude_imports_by_month_year_country_type_of_crude_tbl, "./00_data/crude_imports_by_month_year_country_type_of_crude.csv")
