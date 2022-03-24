# Time Series Analysis and Forecasting in R

## Project Description

We are starting a new project with Company X, a multifamily housing company, and they are asking us to demonstrate direct and indirect benefits of utilizing a science-based revenue management system. Although they believe that their rule-based pricing process performs well, they are always looking for new opportunities to improve the company’s profitability. On the other hand, they also want to know what business process changes are required to implement such a process as their CEO thinks that their current pricing strategy can be improved significantly. That is the reason they hired us to determine optimal price for their units given the recent market trends and future move-in and move-out forecasts.
As a part of the project kick-off, we conducted a business process review to develop a better understanding of the current situation. Following points summarize the key findings:
- Business is highly seasonal. There is a surge of demand during summer months.
- Environment is highly competitive. Customers shop around and look for the best value.
- Prices are set at unit plan level each month. Price of each unit plan, i.e. STD S&A, is determined based on the number of available units for whole unit type, i.e. STDs. As supply gets scarce, price increases.
- Price among unit plans is aligned according to the size of each unit plan. Note that value of a square feet drops as the unit size gets larger.

## Deliverables

As the project lead, your job is to;
- Load data using R.
- Analyze general trends and report key statistics:
i. Monthly occupancy at the site and unit type level, assuming a unit is occupied between a rental’s start date and end date.
ii. Number of move-ins, renewals and move-outs at the monthly and yearly level.
- Find a good forecasting approach, use hold out sampling to test the validity of the approach and report its accuracy with a measure of your choice.
- Provide insights on how to improve forecast accuracy in general.
