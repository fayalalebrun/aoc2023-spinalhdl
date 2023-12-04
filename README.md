# Advent of Code 2023 SpinalHDL Solutions

This repository contains my solutions for Advent of Code 2023, written in SpinalHDL. While it may seem unusual to write algorithmic programs in a hardware definition language, this serves as an exercise for me to get familiar with SpinalHDL and more comfortable writing algorithms directly in digital hardware.

The code should all be synthesizable, but I did not take any step to optimize it in terms of hardware components. The goal is to solve as many problems as possible, and therefore I decided to limit performance optimization to only those problems which needed it.

When the input is not ideal for consumption by a hardware algorithm and parsing is not a central part of the challenge, I do take the liberty to do some pre-processing on the input in Scala.

## Installing SpinalHDL
Go to <https://spinalhdl.github.io/SpinalDoc-RTD/master/SpinalHDL/Getting%20Started/Install%20and%20setup.html> for installation instructions.

## How to simulate
Inputs go in `inputs/`, where the input for each name is called `dayNUMBER.txt`. In order to simulate the hardware for a certain day, you can run:
```sh
sbt "runMain projectname.DayNUMBERSim"
```

Make sure you replace `NUMBER` by the day number you want to run.
