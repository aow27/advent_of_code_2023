library(tidyverse)


source('AoC functions.R')
source('cookie.R')

download_advent(2023,
                1,
                funct = readr::read_delim,
                delim = ' ',
                skip_empty_rows = FALSE,
                col_names = c('content'))

input %>% 
  mutate(numbers = str_remove_all(content, '[^0-9]'),
         first = as.numeric(str_sub(numbers, 1, 1)),
         last = str_sub(numbers, -1, -1) %>% 
           as.numeric(),
         double = as.numeric(str_c(first,last))) %>% 
  summarise(sum(double)) 


## part 2

read_delim('two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen',
 delim = ' ',
 skip_empty_rows = FALSE,
 col_names = c('content')) 

input %>% 
  mutate(content1 = str_replace_all(content,  'one', 'one1one'),
         content1 = str_replace_all(content1, 'two', 'two2two'),
         content1 = str_replace_all(content1, 'three', 'three3three'),
         content1 = str_replace_all(content1, 'four', 'four4four'),
         content1 = str_replace_all(content1, 'five', 'five5five'),
         content1 = str_replace_all(content1, 'six', 'six6six'),
         content1 = str_replace_all(content1, 'seven', 'seven7seven'),
         content1 = str_replace_all(content1, 'eight', 'eight8eight'),
         content1 = str_replace_all(content1, 'nine', 'nine9nine'),
    
    extract1 = str_extract_all(content1,
                                   regex('([0-9])')),
          row = row_number()) %>% 
  unnest_longer(extract1, indices_to = 'id') %>% 
  mutate(extract1= as.numeric(extract1)) %>% 

  filter(id == min(id) |
           id == max(id), .by = row) %>% 
  
  mutate(num = n(), .by =row) %>%
  mutate(double = case_when(num == 1 ~ extract1*10+extract1,
                            id == 1 ~ lead(extract1) + extract1*10,
                            id != 1 ~ 0)) %>% #View()
  summarise(sum(as.numeric(double)))
