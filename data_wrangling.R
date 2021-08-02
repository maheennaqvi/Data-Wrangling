library(tidyverse)
library(readxl)

bikes_tbl           <- read_excel("00_data/bike_sales/data_raw/bikes.xlsx")
orderlines_tbl      <- read_excel("00_data/bike_sales/data_raw/orderlines.xlsx")
bike_orderlines_tbl <- read_rds("00_data/bike_sales/data_wrangled/bike_orderlines.rds")

bikes_tbl
orderlines_tbl
bike_orderlines_tbl %>% glimpse()

bike_orderlines_tbl %>% 
    select(order_date, order_id, order_line) 

bike_orderlines_tbl %>% 
    select(1:3)
bike_orderlines_tbl %>%
    select(starts_with("order_"))

bike_orderlines_tbl %>%
    select(order_date, total_price, category_1, category_2)

bike_orderlines_tbl %>%
    select(bikeshop_name:state, everything())

bike_orderlines_tbl %>% 
    select(starts_with("price"))

bike_orderlines_tbl %>% 
    select(total_price) %>%
    pull(total_price) %>% 
    mean()

bike_orderlines_tbl %>%
    pull(model)

bike_orderlines_tbl %>%
    select_if(is.character)

bike_orderlines_tbl %>%
    select_if(is.numeric)

bike_orderlines_tbl %>%
    select_if(~!is.numeric(.))

bikes_tbl %>% 
    select(model, price) %>%
    arrange(desc(price)) %>%
    View()

bikes_tbl %>% 
    select(model, price) %>% 
    filter(price > mean(price)) 

bikes_tbl %>%
    select(model, price) %>%
    filter((price > 5000) | (price < 1000)) %>%
    arrange(desc(price)) %>%
    View()

bikes_tbl %>%
    select(model, price) %>%
    filter((price > 6000), 
           model %>% str_detect("Supersix"))
           

bike_orderlines_tbl %>% 
    filter(category_2 %in% c ("Over Mountain", "Trail", "Endurance Road"))


bike_orderlines_tbl %>% 
    filter(category_2 %in% c ("Over Mountain", "Trail", "Endurance Road"))

bike_orderlines_tbl %>%
    filter(category_2 == "Over Mountain")

bike_orderlines_tbl %>%
    filter(category_2 != "Over Mountain")  

bike_orderlines_tbl %>% 
    filter(!(category_2 %in% c ("Over Mountain", "Trail", "Endurance Road")))

bikes_tbl %>%
    arrange(desc(price)) %>%
    slice(1:5)

bikes_tbl %>%
    arrange(price) %>%
    slice(1:5)

bikes_tbl %>%
    arrange(desc(price)) %>%
    slice((nrow(.)-4:nrow(.)))

bikes_tbl %>%
    arrange(desc(price)) %>%
    slice(93:97)

bike_orderlines_tbl %>%
    distinct(category_1)

bike_orderlines_tbl %>%
    distinct(category_1, category_2)

bike_orderlines_tbl %>%
    distinct(bikeshop_name, city, state)

bike_orderlines_prices <- bike_orderlines_tbl %>%
    select(order_date, model, quantity, price) %>%
    mutate(total_price = quantity * price)

bike_orderlines_prices %>%
    mutate(total_price = log(total_price))

bike_orderlines_prices %>%    
    mutate(total_price_log = log(total_price)) %>%
    mutate(total_price_sqrt = total_price^0.5)

bike_orderlines_prices %>%  
    mutate(is_supersix = model%>% str_to_lower() %>% str_detect("supersix"))%>%
    filter(is_supersix)

bike_orderlines_prices %>%
    mutate(total_price_binned = ntile(total_price, 4))

bike_orderlines_prices %>%
    mutate(total_price_binned = ntile(total_price, 3)) %>%
    mutate(total_price_binned2 = case_when(
        total_price > quantile(total_price, 0.75) ~ "High", 
        total_price > quantile(total_price, 0.33) ~ "Medium",
        TRUE ~ "Low"
    ))

bike_orderlines_prices %>%
    mutate(bike_type = case_when(
        model%>% str_to_lower() %>% str_detect("supersix") ~ "Supersix",
        model%>% str_to_lower() %>% str_detect("jekyll") ~ "Jekyll",
        TRUE ~ "Not Supersix or Jekyll"
    ))

bike_orderlines_tbl %>%
    summarise(
        revenue = sum (total_price)
    )

bike_orderlines_tbl %>%
    group_by(category_1) %>%
    summarise(revenue = sum (total_price))

bike_orderlines_tbl %>%
    group_by(category_1, category_2) %>%
    summarise(revenue = sum (total_price)) %>%
    ungroup () %>%
    arrange(desc(revenue))


bike_orderlines_tbl %>%
    group_by(category_1, category_2, frame_material) %>%
    summarise(revenue = sum (total_price)) %>%
    ungroup () %>%
    arrange(desc(revenue))

bike_orderlines_tbl %>%
    group_by(category_1, category_2) %>%
    summarize(
        count = n(),
        avg = mean(total_price),
        med = median(total_price),
        sd  = sd(total_price), 
        min = min(total_price),
        max = max(total_price)
    ) %>%
    ungroup() %>%
    arrange(desc(count))

bike_orderlines_missing <- bike_orderlines_tbl %>%
    mutate(total_price = c(rep(NA, 4), total_price[5:nrow(.)]))

bike_orderlines_missing %>%
    summarise_all(~sum(is.na(.)))

bike_orderlines_missing%>%
    summarise_all(~sum(is.na(.))/length(.))

bike_orderlines_missing %>%
    filter(!is.na(total_price))

bikeshop_revenue_tbl <- bike_orderlines_tbl %>%
    select(bikeshop_name, category_1, total_price) %>%
    group_by(bikeshop_name, category_1) %>%
    summarize(sales = sum(total_price)) %>%
    ungroup() %>% 
    arrange(desc(sales))

bikeshop_revenue_tbl %>%
    rename(
        'Bikeshop Name' = bikeshop_name,
        'Primary Caegory' = category_1,
        Sales = sales
    )

bikeshop_revenue_tbl %>%
    set_names(c ("Bikeshop Name", "Primary Category", "Sales"))

bikeshop_revenue_tbl %>%
    set_names(names(.) %>% str_replace("_", " ") %>% str_to_title())

bikeshop_revenue_formatted_tbl <- bikeshop_revenue_tbl %>%
    spread(key = category_1, value = sales) %>%
    arrange(desc(Mountain)) %>%
    rename('Bikeshop Name' = bikeshop_name) %>%
    mutate(
        Mountain = scales::dollar(Mountain),
        Road = scales::dollar(Road)
        )

bikeshop_revenue_formatted_tbl
bikeshop_revenue_formatted_tbl %>%
    gather(key = "category_1", value = "sales", Mountain, Road) %>%
    mutate(sales = sales %>% str_remove_all("\\$|,") %>% as.double()) %>%
    arrange(desc(sales))

orderlines_tbl
bikes_tbl

orderlines_tbl %>% 
    left_join(y = bikes_tbl, by = c("product.id" = "bike.id"))

bike_orderlines_tbl %>%
    select(-contains("order")) %>%
    bind_cols(
        bike_orderlines_tbl %>% select(order_id)
    )
train_tbl <- bike_orderlines_tbl %>%
    slice(1:(nrow(.)/2))

train_tbl

test_tbl <- bike_orderlines_tbl %>%
    slice((nrow(.)/2 + 1):nrow(.))

test_tbl
train_tbl %>% 
    bind_rows(test_tbl)

bike_orderlines_tbl %>%
    select(order_date)  %>%
    mutate(order_date = as.character(order_date)) %>%
    separate(col = order_date, into = c("year", "month", "day"), sep = "-", remove = FALSE) %>%
    mutate(
        year = as.numeric(year),
        month = as.numeric(month), 
        day = as.numeric(day)
    ) %>%
    unite(order_date_unite, year, month, day, sep = "-", remove = FALSE) %>%
    mutate(order_date_unite = as.Date(order_date_unite))
        
