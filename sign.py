from flask import redirect, request, url_for, render_template
from flask.views import MethodView

class Sign(MethodView):
    def get(self):
        return render_template('sign.html')

    def post(self):
        """
        Accepts POST requests, and processes the form;
        Redirect to index when completed.
        """
        # form fields
        # request.form['name']
        # request.form['year']

        return redirect(url_for('view'))
