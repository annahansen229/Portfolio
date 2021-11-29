# Copyright 2016 Google Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

"""
A simple course review flask app.
Data is stored in a serverless NoSQL database that looks something like the following:

+------------------+-----------+----------+------+---------------+-------------+------------+
| department       | course_no | quarter  | year | instructor    | review      | signed_on  |
+==================+===========+==========+======+===============+=============+------------+
| Computer Science | CS530     | Fall     | 2021 | Wu-Chang Feng | Hello World | 2012-05-28 |
+------------------+-----------+----------+------+---------------+-------------+------------+

"""

from .Model import Model
from datetime import datetime
from google.cloud import datastore

def from_datastore(entity):
    """Translates Datastore results into the format expected by the
    application.

    Datastore typically returns:
        [Entity{key: (kind, id), prop: val, ...}]

    This returns:
        [ department, course_no, quarter, year, instructor, review, date ]
    where department, course_no, quarter, year, instructor, and review are Python strings and where date is a Python datetime
    """
    if not entity:
        return None
    if isinstance(entity, list):
        entity = entity.pop()
    return [entity['department'],entity['course_no'],entity['quarter'],entity['year'],entity['instructor'],entity['review'],entity['date']]

class model(Model):
    def __init__(self):
        self.client = datastore.Client('cloud-f21-anna-hansen-ahansen')

    def select(self):
        """
        Gets all rows from the database
        Each row contains: department, course number, quarter, year, instructor, review, and date
        :return: List of lists containing all rows of database
        """
        query = self.client.query(kind = 'course-review')
        entities = list(map(from_datastore,query.fetch()))
        return entities

    def insert(self, department, course_no, quarter, year, instructor, review):
        """
        Inserts entry into database
        :param department: String
        :param course_no: String
        :param quarter: String
        :param year: String
        :param instructor: String
        :param review: String
        :return: True
        :raises: Database errors on connection and insertion
        """
        key = self.client.key('course-review')
        rev = datastore.Entity(key)
        rev.update( {
            'department': department,
            'course_no' : course_no,
            'quarter' : quarter,
            'year' : year,
            'instructor' : instructor,
            'review' : review,
            'date' : datetime.today()
            })
        self.client.put(rev)
        return True
