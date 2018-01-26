# Making Pour Decisions

## Overview
This repository contains an R Shiny app using wine review data from Kaggle. [Click here](https://siobhanmccarter.shinyapps.io/wineapp/) to try out the deployed app!

## The data
For this project, I used a [dataset from Kaggle](https://www.kaggle.com/zynicide/wine-reviews/data) that collected wine reviews from a selection of sommeliers. The set includes over 120,000 wines with features such as quality rating, price, winery, grape, and a number of details about the growing region. 

## The app
I wanted my app to output a variety of wines that are give the user the opportunity to enter inputs. If they have a country, price range, quality range, or grape in mind, this app will give show them a map (showing the count of the specified variety per country) plus a table of relevant information. 

The second tab shows the quality vs. price for the specified country and variety, along with the average price and quality. This should give the user an idea of how much they might have to spend for that quality/variety of wine, and highlights any price vs. quality trends that may arise when viewing the data.
