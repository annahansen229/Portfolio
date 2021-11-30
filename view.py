from flask import render_template, request
from flask.views import MethodView
from namesquery import getNamesData

class View(MethodView):
    def post(self):
        """
        Accepts POST requests, and processes the form
        """
        # form fields
        # request.form['name']
        # request.form['year']
        # request.form['gender']
        name = request.form['name']
        year = request.form['year']
        gender = request.form['gender']

        topName, nameRank, chartURL = getNamesData(name, year, gender)
        
        # re-write gender value for display on page
        if gender == 'M':
            gender = 'male'
        elif gender == 'F':
            gender = 'female'

        # handles an unranked name
        rankString = ''
        if len(nameRank) < 1:
            rankString = 'not ranked'
        else:
            rankString = f'ranked #{nameRank} of all {gender} names'

        # handles a year with no top name
        popularNameString = ''
        if len(topName) < 1:
            popularNameString = f'There was no most popular {gender} name in the year {year}.'
        else:
            popularNameString = f'The most popular {gender} name in the year {year} was {topName}.'

        data = dict(name=name, year=year, gender=gender, rankString=rankString, popularNameString=popularNameString, topMovie='post popular movie', topSong='post popular song', chartURL=chartURL)
        
        return render_template('view.html', data=data)
