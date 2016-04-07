import time
import sys
from jaraco.nxt import Connection, messages
from jaraco.nxt.routine import get_port
from jaraco.nxt.messages import RegulationMode, RunState
from jaraco.nxt import _enum as enum

conn = Connection("/dev/tty.NXT-DevB")
right_port = enum.InputPort(1)
left_port = enum.InputPort(2)
light_port = left_port
motor_port_a = get_port('a', messages.OutputPort)
motor_port_b = get_port('b', messages.OutputPort)
conn.send(messages.SetInputMode(light_port, messages.SensorType.light_active, messages.SensorMode.raw))


Blue_Reading = 540
Gray_Reading = 580


def moveMotor(conn,port,power,time):
    cmd = messages.SetOutputState(port, motor_on = True, set_power = power, run_state = messages.RunState.running)
    conn.send(cmd)
    time.sleep(time)
    cmd = messages.SetOutputState(port, motor_on = False)
    conn.send(cmd)


def calibrateMAX(conn,port,n):
    for i in xrange(n):
        conn.send(messages.GetInputValues(port))
        x = conn.receive().normalized_value
        readingList + x
    return (max(readingList))


def calibrateAVG(conn,port,n):
    for i in xrange(n):
        conn.send(messages.GetInputValues(port))
        x = conn.receive().normalized_value
        readingList + x
    return (sum(readingList)/len(readingList))


try:
    conn.send(messages.GetInputValues(right_port))
    Current_Reading = conn.receive().normalized_value
    print Current_Reading
    moveMotor(conn,motor_port_a,75,.35)
    moveMotor(conn,motor_port_b,75,.35)
    while Current_Reading >= Blue_Reading:
        conn.send(messages.GetInputValues(light_port))
        Current_Reading = conn.receive().normalized_value
        print Current_Reading
        while ((Current_Reading < Gray_Reading) and (Current_Reading > Blue_Reading)):
            cmd = messages.SetOutputState(motor_port_a, motor_on = True, set_power = 70, run_state = messages.RunState.running)
            conn.send(cmd)
            time.sleep(.35)
            cmd = messages.SetOutputState(motor_port_a, motor_on = False)
            conn.send(cmd)
            time.sleep(.05)
            conn.send(messages.GetInputValues(light_port))
            Current_Reading = conn.receive().normalized_value
            print Current_Reading
        while Current_Reading >= Gray_Reading:
                cmd = messages.SetOutputState(motor_port_b, motor_on = True, set_power = 70, run_state = messages.RunState.running)
                conn.send(cmd)
                time.sleep(.35)
                cmd = messages.SetOutputState(motor_port_b, motor_on = False)
                conn.send(cmd)
                time.sleep(.05)
                conn.send(messages.GetInputValues(light_port))
                Current_Reading = conn.receive().normalized_value
                print Current_Reading
    time.sleep(1)
    RUN = False

except KeyboardInterrupt:
    cmd = messages.SetOutputState(motor_port_a)
    conn.send(cmd)
    cmd = messages.SetOutputState(motor_port_b)
    conn.send(cmd)
    time.sleep(1)
    RUN = False
