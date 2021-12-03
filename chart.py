from google.cloud import bigquery
from google.cloud import storage
import pandas
import numpy
import matplotlib
import os


def getChart(name, gender):
    # ----------------------------------------------------
    #           get data for the chart of the name
    # ----------------------------------------------------
    
    # instantiate the bq client
    bq_client = bigquery.Client('cloud-f21-anna-hansen-ahansen')
        
    # the big query public dataset I am querying
    dataset = 'bigquery-public-data.usa_names.usa_1910_current'
    
    # query string
    chartDataQueryString = (
        f'SELECT year, sum(number) as total '
        f'FROM `{dataset}` '
        f'WHERE gender="{gender}" '
        f'AND name="{name}" '
        f'GROUP BY name, year '
        f'ORDER BY year '
    )

    # run query
    job = bq_client.query(chartDataQueryString)
    
    # test how many rows were returned
    rows = job.result()
    if rows.total_rows < 1:
        # no data in table
        chartURL = ''
        return chartURL
    else:
        # ----------------------------------------------------
        #           generate and save chart image
        # ----------------------------------------------------
        filename = f'{name}-chart.jpg'

        df = job.to_dataframe()
        plot = df.plot(x='year', y='total', title=f'Babies Named {name} 1910-2020', legend=False) #kind='line' is default
        fig = plot.get_figure()
        fig.savefig(filename)

        # ----------------------------------------------------
        #           upload to storage bucket
        # ----------------------------------------------------
        bucket_name = 'ahansen-finalproject'
        object_name = filename

        # instantiate the storage client
        storage_client = storage.Client('cloud-f21-anna-hansen-ahansen')
        
        # access the bucket and save the file to it publicly
        bucket = storage_client.bucket(bucket_name)
        blob = bucket.blob(object_name)
        blob.upload_from_filename(object_name)
        blob.make_public()
        
        chartURL = f'https://storage.cloud.google.com/{bucket_name}/{object_name}'

        # cleanup - delete local copy of file
        if os.path.exists(filename):
            os.remove(filename)
        
        return chartURL