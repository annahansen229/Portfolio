from google.cloud import bigquery
import pandas as pd
import numpy as np


# def getChart(name, year, gender):

# values used for testing queries
# name = 'Anna'
# year = '1985'
# gender = 'F'

# instantiate the bq client
client = bigquery.Client('cloud-f21-anna-hansen-ahansen')
    
# the big query public dataset I am querying
dataset = 'bigquery-public-data.usa_names.usa_1910_2013'

# placeholder image will be updated later with real url
chartURL = 'https://placekitten.com/200/300'

# ----------------------------------------------------
#           get data for the chart of the name
# ----------------------------------------------------
chartDataQueryString = (
    f'SELECT year, sum(number) as total '
    f'FROM `{dataset}` '
    f'WHERE gender="{gender}" '
    f'AND name="{name}" '
    f'GROUP BY name, year '
    f'ORDER BY year '
)

df = client.query(chartDataQueryString).to_dataframe()

#TODO need to create chart and save it somewhere then set chartURL to the url

# return chartURL