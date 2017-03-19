import unittest
from unittest.mock import Mock

import analyse_pomodoro
from analyse_pomodoro import Event, EventType 
import sys
import datetime

class AnalysePomodoroTestCase(unittest.TestCase):

    def test_get_pomodoro_filename(self):
        analyse_pomodoro.get_current_date = Mock(return_value="2017-03-18")
        self.assertEqual(analyse_pomodoro.get_pomodoro_filename(), "2017-03-18.txt")

    def test_get_pomodoro_filename_with_arg(self):
        sys.argv = ["2017-03-17.txt"]
        self.assertEqual(analyse_pomodoro.get_pomodoro_filename(), "2017-03-17.txt")

    def test_get_pomodoro_filepath(self):
        analyse_pomodoro.POMODORO_DIRECTORY = "/home/test/"
        analyse_pomodoro.get_pomodoro_filename = Mock(return_value="2017-03-18.txt")
        self.assertEqual(analyse_pomodoro.get_pomodoro_filepath(), "/home/test/2017-03-18.txt")

    def test_extract_events_from_file(self):
        with open("fixtures/2017-02-01.txt") as pomodorofile:
            event = analyse_pomodoro.extract_events_from_file(pomodorofile)
        self.assertEqual([Event(EventType.START, datetime.datetime(year=2017, month=2, day=1, hour=16, minute=0)),
                         Event(EventType.END, datetime.datetime(year=2017, month=2, day=1, hour=16, minute=21)),
                         Event(EventType.START, datetime.datetime(year=2017, month=2, day=1, hour=16, minute=26)),
                         Event(EventType.INTERRUPT, datetime.datetime(year=2017, month=2, day=1, hour=16, minute=36))], event)

    # def test_count_pomodoro(self):
    #     events = [(Event.START, datetime.datetime(year=2017, month=2, day=1, hour=16, minute=0)),
    #               (Event.END, datetime.datetime(year=2017, month=2, day=1, hour=16, minute=21)),
    #               (Event.START, datetime.datetime(year=2017, month=2, day=1, hour=16, minute=26)),
    #               (Event.INTERRUPT, datetime.datetime(year=2017, month=2, day=1, hour=16, minute=36))]

    #     pass
