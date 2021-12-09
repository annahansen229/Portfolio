from google.cloud import bigquery

def getNamesData(name, year, gender):
    """
    Executes the queries to return the rank of the name/gender entered by the user
    and the top name for the year/gender entered by the user.
    """
    # instantiate the BigQuery client
    client = bigquery.Client('cloud-f21-anna-hansen-ahansen')
    
    # the big query public dataset I am querying
    dataset = 'bigquery-public-data.usa_names.usa_1910_current'

    # values to be sent back begin as empty strings in case query returns no result
    topName = ''
    nameRank = 0

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

    # if the query is not empty this will grab the top name from the single result returned
    # if the query is empty nothing will happen here and the top name will stay blank
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

    # if the query is not empty this will grab the rank of the name from the single result returned
    # if the query is empty nothing will happen here and the rank name will stay 0 
    for row in results:
        nameRank = row['ranking']

    return topName, nameRank


