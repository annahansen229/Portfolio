from flask import render_template
from flask.views import MethodView

class View(MethodView):
    def get(self):
        data = dict(name='My name', year='My birth year', nameRank='My name rank', popularName='My most popular name', topMovie='My most popular movie', topSong='My most popular song')
        
        return render_template('view.html', data=data)
