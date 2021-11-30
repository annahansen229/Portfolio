from google.cloud import bigquery
import pandas
import numpy
import matplotlib


# def getChart(name, gender):

# values used for testing queries
name = 'Anna'
gender = 'F'

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
plot = df.plot(x='year', y='total', title=f'Babies Named {name} 1910-2013', legend=False) #kind='line' is default
fig = plot.get_figure()
fig.savefig("output.jpg")

#TODO need to create chart and save it somewhere then set chartURL to the url

# return chartURL