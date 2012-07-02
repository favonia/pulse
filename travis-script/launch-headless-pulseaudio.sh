#!/bin/sh

eval `dbus-launch`
export DBUS_SESSION_BUS_ADDRESS
export DBUS_SESSION_BUS_PID
export DBUS_SESSION_BUS_WINDOWID

pulseaudio --start
