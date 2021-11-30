from flask import render_template, request
from flask.views import MethodView

class View(MethodView):
    def post(self):
        """
        Accepts POST requests, and processes the form
        """
        # form fields
        # request.form['name']
        # request.form['year']
        # request.form['gender']



        data = dict(name=request.form['name'], year=request.form['year'], gender=request.form['gender'],nameRank='post rank', popularName='post popular name', topMovie='post popular movie', topSong='post popular song')
        
        return render_template('view.html', data=data)
