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
        #TODO handle empty values

        # re-do gender for display on page
        if gender == 'M':
            gender = 'male'
        else:
            gender = 'female'

        data = dict(name=name, year=year, gender=gender, nameRank=nameRank, popularName=topName, topMovie='post popular movie', topSong='post popular song', chartURL=chartURL)
        
        return render_template('view.html', data=data)
