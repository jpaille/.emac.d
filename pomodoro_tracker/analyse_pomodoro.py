import datetime
import os
import sys
from collections import namedtuple
from enum import Enum

import pytz

POMODORO_DIRECTORY = ""

class EventType(Enum):
    START = 1
    END = 2
    INTERRUPT = 3

Event = namedtuple("Event", ['type', 'time'])

def get_current_date():
    return pytz.timezone('Europe/Paris').localize(datetime.datetime.now()).strftime("%Y-%m-%d")

def get_pomodoro_filename():
    if len(sys.argv) == 1:
        return os.path.join(sys.argv[0])
    return os.path.join("{}.txt".format(get_current_date()))

def get_pomodoro_filepath():
    return os.path.join(POMODORO_DIRECTORY, get_pomodoro_filename())

def extract_events_from_file(pomodoro_file):
    """Return a list of `Event`"""
    events = []
    for line in pomodoro_file.readlines():
        event_string = line[:2]
        time_string = "{} -- {}".format(os.path.basename(pomodoro_file.name)[:-4], line[2:].strip())
        if event_string == 'p:':
            events.append(Event(EventType.START,
                                datetime.datetime.strptime(time_string, "%Y-%m-%d -- %H:%M")))
        elif event_string == 'b:':
            events.append(Event(EventType.END,
                                datetime.datetime.strptime(time_string, "%Y-%m-%d -- %H:%M")))
        elif event_string == 'i:':
            events.append(Event(EventType.INTERRUPT,
                                datetime.datetime.strptime(time_string, "%Y-%m-%d -- %H:%M")))
    return events

def count_pomodoros(events):
    for index, event in enumerate(events):
        if event.type == EventType.START and events[index + 1].type == EventType.END:
            if 

def analyse_pomodoro():
    with open(get_pomodoro_filepath(), "w+") as pomodoro_file:
        events = extract_events_from_file(pomodoro_file)
    count_pomodoros(events)


if __name__ == '__main__':
    pass
    ##analyse_pomodoro()
