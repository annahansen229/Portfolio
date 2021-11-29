from flask import render_template
from flask.views import MethodView
import gbmodel

class View(MethodView):
    def get(self):
        model = gbmodel.get_model()
        reviews = [dict(department=row[0], course_no=row[1], quarter=row[2], year=row[3], instructor=row[4], review=row[5], signed_on=row[6]) for row in model.select()]
        return render_template('view.html',reviews=reviews)
