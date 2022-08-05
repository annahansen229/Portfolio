from flask import render_template, request
from flask.views import MethodView
from namesquery import getNamesData
from chart import getChart

class View(MethodView):
    def post(self):
        """
        Accepts POST requests, and processes the form
        """
        name = request.form['name']
        year = request.form['year']
        gender = request.form['gender']

        topName, nameRank = getNamesData(name, year, gender)
        chartURL = getChart(name, gender)
        
        # re-write gender value for display on page
        if gender == 'M':
            gender = 'male'
        elif gender == 'F':
            gender = 'female'

        data = dict(name=name, year=year, gender=gender, nameRank=nameRank, topName=topName, topMovie='post popular movie', topSong='post popular song', chartURL=chartURL)
        
        return render_template('view.html', data=data)
