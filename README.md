# ETX

[![Build Status](https://github.com/Sciss/ETX/workflows/Scala%20CI/badge.svg?branch=main)](https://github.com/Sciss/ETX/actions?query=workflow%3A%22Scala+CI%22)

This project contains my personal set-up for a live improvisation project.
It is an extension of [Wolkenpumpe](https://codeberg.org/sciss/Wolkenpumpe).

All code here
is (C)opyright 2014&ndash;2022 by Hanns Holger Rutz. All rights reserved. This project is released under the
[GNU General Public License](https://codeberg.org/sciss/ETX/raw/main/LICENSE) v3+ and comes with absolutely no warranties.
To contact the author, send an e-mail to `contact at sciss.de`.

## building

Builds with sbt against Scala 2.12. Use `sbt run`, or `sbt assembly` to create a self-contained jar (you may need the
latter if you want to add the tablet library to the system's class path).

## running

To use the Wacom controls, add environment variable `LD_LIBRARY_PATH=lib` to the JVM run
(currently only Linux native library included).

Note: the look-and-feel _Submin_ currently doesn't run on JDK 17 -- use JDK 11.

## network connection

We use [JackTrip](https://github.com/jacktrip/jacktrip/releases). On Debian 11, when using the GUI version of
JackTrip, you need to use version 1.6.2, newer versions have broken UI (they might need a QT newer than the one
installed on Debian 11).
