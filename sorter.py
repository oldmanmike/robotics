#!python

import sys
"""
from jaraco.nxt import Connection
from jaraco.nxt import messages
from jaraco.nxt import _enum as enum
from jaraco.nxt import locator
from jaraco.nxt.routine import (
	cycle_motor_a,
)
"""
import time


def touchSensor():
    pass


def lightSensor():
    pass


def detectBrick(lightValue):
    pass 


def sort(lightValue):
    pass


def move():
    pass


def motor():
    pass

"""
def querySorter(conn,port):
	conn.send(messages.GetInputValues(port))
	raw = conn.recieve()
	dat = ', '.join('%d' % getattr(input_res, field) for field in input_res.fields)
	sys.stdout.write(dat)


def runSorter():
	conn = Connection('/dev/tty.NXT49-DevB')
	port = enum.InputPort(1)
	print(port)
	conn.send(messages.SetInputMode(
		port,
		messages.SensorType.switch,
		messages.SensorMode.boolean,
	))
	print(', '.join(field[:4] for field in messages.InputValues.fields))
	querySorter(conn,port)
	#cycle_motor_a(conn)
	conn.close()
"""

#__name__ == '__main__' and runSorter()
