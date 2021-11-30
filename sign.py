from flask import redirect, request, url_for, render_template
from flask.views import MethodView

class Sign(MethodView):
    def get(self):
        return render_template('sign.html')
