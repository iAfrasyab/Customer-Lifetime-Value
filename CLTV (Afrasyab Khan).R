#AFRASYAB KHAN

library(tidyverse)
library(lubridate)

df_salesdata <- read.csv("C:Users/afras/Downloads/RStudio")

#Average purchase frequency
df_salesdata |>
  mutate(invoicedate = strptime(InvoiceDate, format = "%m/%d/%Y %H:%M"),
    month = lubridate::month(invoicedate, label = TRUE, abbr = TRUE),
    year = lubridate::year(invoicedate)) |>
  drop_na() |>
  group_by(CustomerID, month, year) |>
  summarise(n_transaction = n_distinct(InvoiceNo)) |>
  ungroup() |>
  summarise(avg_purchase_freq = mean(n_transaction)) -> avg_purchase_frequency

#Average purchase value
df_salesdata |>
  mutate(subtotal = Quantity * UnitPrice) |>
  group_by(CustomerID) |>
  summarise(total_value = sum(subtotal)) |>
  summarise(avg_purchase_value = mean(total_value)) -> avg_purchase_value

#Average lifespan
df_salesdata |>
  mutate(invoice_no = strptime(InvoiceDate, format = "%m/%d/%Y %H:%M")) |>
  group_by(CustomerID) |>
  summarise(last_purchase = max(invoice_no),
            first_purchase = min(invoice_no),
    lifespan = difftime(last_purchase, first_purchase, units = "days")) |>
  mutate(monthly_lifespan = as.integer((lifespan) / 30)) |>
  summarise(average_lifespan = mean(monthly_lifespan)) -> avg_lifespan

CLV <- avg_purchase_frequency * avg_purchase_value * avg_lifespan * (0.3)
print(CLV)