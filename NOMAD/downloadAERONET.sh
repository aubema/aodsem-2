#!/bin/sh

# Minimum
#  Crontab à GMT - 5 (hiver) ->  19:00
#  Crontab à GMT - 4 (été)   ->  20:00
# 
#   -> A partir de 20h heure locale (été + hiver) 
#

################################################################################
# Functions
################################################################################
. /opt/lib/bash/functions

################################################################################
# Default values
################################################################################
deltadays=-1
outputDir=$HOME/data/aeronet
execute1=1
execute2=1

################################################################################
# Read command-line options
################################################################################
while getopts d:o:xy name
do
    case $name in
		d)    deltadays="$OPTARG";;
		o)    outputDir="$OPTARG";;
		x)    execute1=1;;
		y)    execute2=1;;
		[?])   printf "Usage: %s:  [-d deltadays] [-o outputDir] [-x] [-y]\n" $0
			exit 2;;
    esac
done

#for dirZIP in $outputDir/zip/????.??.??
#do
#	bDirZIP=`basename $dirZIP`
#	nbZIP=`ls $dirZIP/*.zip 2>/dev/null | wc -l`
#	
#	echo $bDirZIP: $nbZIP
#done

#exit 

################################################################################
# Set year, month, day variables
################################################################################
if [ $deltadays -le 0 ]; then
	set -- `perl -w -e 'use Date::Calc qw(Add_Delta_Days Today);printf("%04d %02d %02d\n", (Add_Delta_Days(Today(1), '$deltadays')));'` 
	year=$1
	month=$2
	day=$3
else
	year=`echo $deltadays | cut -c1-4`
	month=`echo $deltadays | cut -c5-6`
	day=`echo $deltadays | cut -c7-8`
fi

echo $year-$month-$day
################################################################################
# Set log file
################################################################################
logDir=$outputDir/log/$year.$month.$day

if [ ! -e $logDir ]; then
	mkdir -p $logDir
fi

logfile=$logDir/$year$month$day.log
date +"== %F %T ==" >> $logfile

################################################################################
# Download AERONET files
################################################################################
outputDirZIP=$outputDir/zip/$year.$month.$day

if [ ! -e $outputDirZIP ]; then
	mkdir -p $outputDirZIP
fi			

nbZIP=`ls $outputDirZIP/*.zip | wc -l`

if [ $nbZIP -eq 0 ]; then	
	~/bin/downloadAERONET.pl \
		--dinf=$year-$month-$day  \
		--dsup=$year-$month-$day  \
		--outputDir=$outputDirZIP \
		2>&1 >> $logfile
	
	# Check the exit status of the last command executed	
	_mailError $? $year.$month.$day:downloadAERONET

	nbZIP=`ls $outputDirZIP/*.zip | wc -l`
	
#	if [ $nbZIP -lt 34 ]; then	
#		rm -f $outputDirZIP/*.zip
#		nbZIP=-1
	
		# Report Warning
#		_mailWarning 1 $year.$month.$day:nbZIP_not_34
#	fi
	
	nbZIPNew=$nbZIP
else
	nbZIPNew=0
fi			

################################################################################
# Filter AERONET files
################################################################################
outputDirXY=$outputDir/xy/$year.$month.$day

if [ ! -e $outputDirXY ]; then
	mkdir -p $outputDirXY
fi			

nblev10XY=`ls $outputDirXY/lev10/*/*.xy | wc -l`
nblev15XY=`ls $outputDirXY/lev15/*/*.xy | wc -l`

if [ $nbZIP -gt 0 ] && [ $nblev10XY -eq 0 ] ; then
	~/bin/aeronet2xy.pl \
		$outputDirZIP/*.zip      \
		--outputDir=$outputDirXY \
		--level=lev10 \
		--level=lev15 \
		2>&1 >> $logfile
	
	# Check the exit status of the last command executed	
	_mailError $? $year.$month.$day:aeronet2xy
	
	nblev10XY=`ls $outputDirXY/lev10/*/*.xy | wc -l`
	nblev15XY=`ls $outputDirXY/lev15/*/*.xy | wc -l`
	
	if [ $nblev10XY -eq 0 ]; then
		rm -f $outputDirXY/lev10/*/*.xy 
		rm -f $outputDirXY/lev15/*/*.xy
		nblev10XY=-1
		nblev15XY=-1
	
		# Report Warning
		_mailWarning 1 $year.$month.$day:nblev10XY_eq_0		
	fi
		
	nblev10XYNew=$nblev10XY
	nblev15XYNew=$nblev15XY
else
	nblev10XYNew=0
	nblev15XYNew=0	
fi
	
################################################################################
# Create pgm files
################################################################################
outputDirPGM=$outputDir/pgm/$year.$month.$day
nblev10PGM=`ls $outputDirPGM/lev10/*.pgm | wc -l`
nblev15PGM=`ls $outputDirPGM/lev15/*.pgm | wc -l`

if [ $nblev10XY -gt 0 ] && [ $nblev10PGM -eq 0 ]; then	
	for level in lev10 lev15
	do
		outputDirPGM2=$outputDirPGM/$level
				
		if [ ! -e $outputDirPGM2 ]; then
			mkdir -p $outputDirPGM2
		fi			
		
		~/bin/xy2pgm.pl \
			--inputDir=$outputDirXY/$level/* \
			--outputDir=$outputDirPGM2       \
			2>&1 >> $logfile
			
		# Check the exit status of the last command executed	
		_mailError $? xy2pgm $logfile		
		
		if [ $level = "lev10" ]; then
			nblev10PGM=`ls $outputDirPGM/lev10/*.pgm | wc -l`
		else
			nblev15PGM=`ls $outputDirPGM/lev15/*.pgm | wc -l`		
		fi
		
		if [ $nblev10PGM -eq 0 ]; then
			rm -f $outputDirPGM/lev10/*/*.xy 
			rm -f $outputDirPGM/lev15/*/*.xy
			nblev10PGM=-1
			nblev15PGM=-1
			
			# Report Warning
			_mailWarning 1 $year.$month.$day:nblev10PGM_eq_0				
		fi
	done
	
	nblev10PGMNew=$nblev10PGM
	nblev15PGMNew=$nblev15PGM	
else
	nblev10PGMNew=0
	nblev15PGMNew=0	
fi

################################################################################
# Statistics
################################################################################
date +"== %F %T ($year-$month-$day) $nbZIPNew zip/$nblev10XYNew+$nblev15XYNew xy/$nblev10PGMNew+$nblev15PGMNew pgm ==" >> $logfile
