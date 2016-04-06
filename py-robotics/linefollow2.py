import time
import sys
from jaraco.nxt import Connection, messages
from jaraco.nxt.routine import get_port
from jaraco.nxt.messages import RegulationMode, RunState
from jaraco.nxt import _enum as enum

conn = Connection("COM17")
light_port = enum.InputPort(4)
motor_port_a = get_port('a', messages.OutputPort)
motor_port_b = get_port('b', messages.OutputPort)
conn.send(messages.SetInputMode(light_port, messages.SensorType.light_active, messages.SensorMode.raw))

Blue_Reading = 540
Gray_Reading = 580

def readSensor(conn,port,sensor_type,mode):
    conn.send(messages.SetInputMode(port, messages.SensorType.(sensor_type), messages.SensorMode.(mode)))


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
    conn.send(messages.GetInputValues(light_port))
    Current_Reading = conn.receive().normalized_value
    print Current_Reading
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
