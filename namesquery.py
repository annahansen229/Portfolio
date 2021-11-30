from google.cloud import bigquery

def getNamesData(name, year, gender):

    # values used for testing queries
    # name = 'Anna'
    # year = '1985'
    # gender = 'F'

    # instantiate the bq client
    client = bigquery.Client('cloud-f21-anna-hansen-ahansen')
    
    # the big query public dataset I am querying
    dataset = 'bigquery-public-data.usa_names.usa_1910_2013'

    # values to be sent back begin as empty strings in case query returns no result
    topName = ''
    nameRank = ''

    # ----------------------------------------------------
    #           get the top name for the year
    # ----------------------------------------------------
    topNameQueryString = (
        f'SELECT name '
        f'FROM `{dataset}` '
        f'WHERE gender="{gender}" '
        f'AND year={year} '
        f'GROUP BY name '
        f'ORDER BY sum(number) DESC '
        f'LIMIT 1'
    )

    results = client.query(topNameQueryString)

    # I would like to directly access the row rather than iterating over the object, but I can't find out how. This approach assumes there will only be one row, which only works because my query results are limited to one row. If there were more than one row this would not work as-is.
    for row in results:
        topName = row['name']

    # ----------------------------------------------------
    #           get the rank of the name
    # ----------------------------------------------------
    nameRankQueryString = (
        f'SELECT ranking '
        f'FROM (SELECT name, sum(number), RANK() OVER (ORDER BY SUM(number) DESC) as  ranking '
            f'FROM `{dataset}` '
            f'WHERE gender="{gender}" '
            f'AND year={year} '
            f'GROUP BY name '
            f'ORDER BY sum(number) DESC) '
        f'WHERE name="{name}" '
    )

    results = client.query(nameRankQueryString)

    # This has the same problem described above. 
    for row in results:
        nameRank = row['ranking']

    return topName, nameRank


