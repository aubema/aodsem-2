#!/bin/sh

################################################################################
# Functions
################################################################################
. /opt/lib/bash/functions

################################################################################
# Default values
################################################################################
aodsemDir=/home3/level3
logDir=/home/level3/log
predir=`pwd`

# Bzip the previous month
set -- `perl -w -e 'use Date::Calc qw(Add_Delta_YM Today);printf("%04d %02d\n", (Add_Delta_YM(Today(1), 0, -2)));'`
year=$1
month=$2
#bzip2 /home/level3/data/meteo/asc/$year.$month.??/*/*.asc

for deltaMonth in 0 -1
do
	################################################################################
	# Set year, month
	################################################################################
	set -- `perl -w -e 'use Date::Calc qw(Add_Delta_YM Today);printf("%04d %02d\n", (Add_Delta_YM(Today(1), 0, '$deltaMonth')));'`
	year=$1
	month=$2
	
	echo Processing $year $month
	
	logFile=$logDir/$year.$month
	
	if [ ! -e $logDir ]; then
		mkdir -p $logDir
	fi
	
	date > $logFile
			
	################################################################################
	# Create files link for AODSEM
	################################################################################
	#~/bin/data2aodsem.sh -y $year -m $month -o $aodsemDir
done

################################################################################
# AODSEM
################################################################################
		
################################################################################
# AODSEM PGM results file to NetCDF format
################################################################################
echo AAAA $year $month

#~/bin/aodsem2nc.sh -y $year -m $month -i $aodsemDir/results

# Check the exit status of the last command executed	
_mailError $? $year.$month:aodsem2nc

date >> $logFile

