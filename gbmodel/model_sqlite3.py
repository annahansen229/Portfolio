"""
A simple course review flask app.
Data is stored in a SQLite database that looks something like the following:

+------------------+-----------+----------+------+---------------+-------------+------------+
| department       | course_no | quarter  | year | instructor    | review      | signed_on  |
+==================+===========+==========+======+===============+=============+------------+
| Computer Science | CS530     | Fall     | 2021 | Wu-Chang Feng | Hello World | 2012-05-28 |
+------------------+-----------+----------+------+---------------+-------------+------------+

This can be created with the following SQL (see bottom of this file):

    create table course_reviews (department text, course_no text, quarter text, year text, instructor text, review, signed_on date);

"""
from datetime import date
from .Model import Model
import sqlite3
DB_FILE = 'reviews.db'    # file for our Database

class model(Model):
    def __init__(self):
        # Make sure our database exists
        connection = sqlite3.connect(DB_FILE)
        cursor = connection.cursor()
        try:
            cursor.execute("select count(rowid) from course_reviews")
        except sqlite3.OperationalError:
            cursor.execute("create table course_reviews (department text, course_no text, quarter text, year text, instructor text, review, signed_on date)")
        cursor.close()

    def select(self):
        """
        Gets all rows from the database
        Each row contains: department, course number, quarter, year, instructor, review, and date
        :return: List of lists containing all rows of database
        """
        connection = sqlite3.connect(DB_FILE)
        cursor = connection.cursor()
        cursor.execute("SELECT * FROM course_reviews")
        return cursor.fetchall()

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
        params = {'department':department, 'course_no':course_no, 'quarter':quarter, 'year':year, 'instructor':instructor, 'review':review, 'date':date.today()}
        connection = sqlite3.connect(DB_FILE)
        cursor = connection.cursor()
        cursor.execute("insert into course_reviews (department, course_no, quarter, year, instructor, review, signed_on) VALUES (:department, :course_no, :quarter, :year, :instructor, :review, :date)", params)

        connection.commit()
        cursor.close()
        return True
