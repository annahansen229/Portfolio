from google.cloud import bigquery

def namesquery(name, year, gender):
    client = bigquery.Client()

    dataset = 'bigquery-public-data.usa_names.usa_1910_2013'

    topNameQueryString = (
        f'SELECT name'
        f'FROM {dataset}'
        f'WHERE gender={gender}'
        f'AND year={year}'
        f'GROUP BY name'
        f'ORDER BY sum(number) DESC'
        f'LIMIT 1'
    )

    nameRankQueryString = (
        f'SELECT ranking'
        f'FROM (SELECT name, sum(number), RANK() OVER (ORDER BY SUM(number) DESC) as  ranking'
            f'FROM {dataset}'
            f'WHERE gender={gender}'
            f'AND year={year}'
            f'GROUP BY name'
            f'ORDER BY sum(number) DESC)'
        f'WHERE name={name}'
    )

    chartDataQueryString = (
        f'SELECT year, sum(number) as total'
        f'FROM {dataset}'
        f'WHERE gender={gender}'
        f'AND name={name}'
        f'GROUP BY name, year'
        f'ORDER BY year'
    )

    topNameQueryJob = client.query(topNameQueryString)
    nameRankQueryJob = client.query(nameRankQueryString)
    chartDataQueryJob = client.query(chartDataQueryString)

    topName = topNameQueryJob.result()
    nameRank = nameRankQueryJob.result()
    chartData = chartDataQueryJob.result()

    print topName

    return ([name, year])
