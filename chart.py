from google.cloud import bigquery
from google.cloud import storage
import pandas
import numpy
import matplotlib


# def getChart(name, gender):

# values used for testing queries
name = 'Anna'
gender = 'F'
filename = f'{name}-chart.jpg'
# instantiate the bq client
bq_client = bigquery.Client('cloud-f21-anna-hansen-ahansen')
    
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

df = bq_client.query(chartDataQueryString).to_dataframe()
plot = df.plot(x='year', y='total', title=f'Babies Named {name} 1910-2013', legend=False) #kind='line' is default
fig = plot.get_figure()
fig.savefig(filename)

#TODO need to create chart and save it somewhere then set chartURL to the url

#save to gcs bucket
bucket_name = 'ahansen-finalproject'
object_name = filename

storage_client = storage.Client('cloud-f21-anna-hansen-ahansen')
bucket = storage_client.bucket(bucket_name)
blob = bucket.blob(object_name)
blob.upload_from_filename(object_name)
blob.make_public()
chartURL = f'storage.cloud.google.com/{bucket_name}/{object_name}'
print(chartURL)
# return chartURL