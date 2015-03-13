#!/bin/sh

cd ~/data

################################################################################
# Default values
################################################################################
deltadays=0

################################################################################
# Read command-line options
################################################################################
while getopts d:o: name
do
    case $name in
		d)    deltadays="$OPTARG";;
		[?])   printf "Usage: %s:  [-d deltadays]\n" $0
			exit 2;;
    esac
done

################################################################################
# Set year, month, day variables
################################################################################
deltadays1=`expr $deltadays - 1`
set -- `perl -w -e 'use Date::Calc qw(Add_Delta_Days Today);printf("%04d %02d %02d\n", (Add_Delta_Days(Today(1), '$deltadays1')));'`
year1=$1
month1=$2
day1=$3

set -- `perl -w -e 'use Date::Calc qw(Add_Delta_Days Today);printf("%04d %02d %02d\n", (Add_Delta_Days(Today(1), '$deltadays')));'`
year2=$1
month2=$2
day2=$3

################################################################################
# AERONET
################################################################################
echo === AERONET ===
grep "== $year1-$month1-$day1" aeronet/log/*/*.log
grep "== $year2-$month2-$day2" aeronet/log/*/*.log
echo

################################################################################
# Meteo
################################################################################
echo === Meteo ===
#grep "== $year1-$month1-$day1" meteo/log/*/*.log
grep "== $year2-$month2-$day2" meteo/log/*/*.log
echo

################################################################################
# MODIS
################################################################################
echo === MODIS ===
grep "== $year2-$month2-$day2" modis/log/*/*.log
echo

