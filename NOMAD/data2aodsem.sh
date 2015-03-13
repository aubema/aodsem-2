#!/bin/sh

################################################################################
# Default values
################################################################################
year=2005
month=02
inputDirRoot=/home/level3/data
outputDirRoot=/home3/level3/

################################################################################
# Read command-line options
################################################################################
while getopts y:m:i:o: name
do
    case $name in
		y)	  year="$OPTARG";;
		m)    month="$OPTARG";;
		i)    inputDirRoot="$OPTARG";;
		o)    outputDirRoot="$OPTARG";;		
		[?])   printf "Usage: %s:  [-y year] [-m month] [-i inputDirRoot] [-o outputDirRoot]\n" $0
			exit 2;;
    esac
done

################################################################################
# Notes:
#
#   base-de-donnee/YYYY/MM/ 
#
#     et dans ce repertoire on doit trouver tous les fichiers du mois. Ceci ne 
#     s'applique pas aux donnees d'origine comme les grib et les hdf mais seulement 
#     aux donnees ingerees par AODSEM comme .pgm et asc.
#     La base-de-donnee peut etre modis, aeronet L15, puis GEM
################################################################################

################################################################################
# modis
################################################################################
echo "Linking modis images..."
outputDir=$outputDirRoot/modis/$year/$month

if [ ! -e $outputDir ]; then
	mkdir -p $outputDir
fi

for src in $inputDirRoot/modis/pgm/$year.$month.??/*
do
	dest=$outputDir/`basename $src`

	if [ ! -L $dest ]; then
		ln -s $src $dest
	fi
done

################################################################################
# aeronet L15
################################################################################
echo "linking aeronet level 1.5 data..."
outputDir=$outputDirRoot/aeronet_L15/$year/$month

if [ ! -e $outputDir ]; then
	mkdir -p $outputDir
fi

for src in $inputDirRoot/aeronet/pgm/$year.$month.??/lev15/*
do
	dest=$outputDir/`basename $src`
	
	if [ ! -L $dest ]; then
		ln -s $src $dest
	fi
done

################################################################################
# Convert grib to ascii format
################################################################################
echo "convert grib to ascii format..."
outputDir=$HOME/data/meteo

cd $outputDir/grib

for dataDate in $year.$month.??
do
	if [ $dataDate = "$year.$month.??" ]; then
		break
	fi

	for hh in 00 12
	do
		outputDirGRIB=$outputDir/grib/$dataDate/${hh}
		outputDirASC=$outputDir/asc/$dataDate/${hh}
		if [ ! -e $outputDirASC ]; then
			mkdir -p $outputDirASC
		fi	
		echo "starting CMC2asc.pl for "$outputDirASC	
			~/bin/CMC2asc.pl \
				--inputDir=$outputDirGRIB  \
				--outputDir=$outputDirASC  
		
	done
done

################################################################################
# GEM
# 
# Notes:
#   repertoire 00:
#     2004121400_P000.ana.asc -> 2004121400_000_ana_asc
#     2004121400_P006.ana.asc -> 2004121406_006_ana_asc
#     2004121400_P006.pcp.asc -> 2004121400_006_pcp_asc
#     2004121400_P012.pcp.asc -> 2004121406_006_pcp_asc

#   repertoire 12:
#     2004121412_P000.ana.asc -> 2004121412_000_ana_asc
#     2004121412_P006.ana.asc -> 2004121418_006_ana_asc
#     2004121412_P006.pcp.asc -> 2004121412_006_ana_asc
#     2004121412_P012.pcp.asc -> 2004121418_006_ana_asc 
################################################################################
outputDir=$outputDirRoot/GEM/$year/$month

if [ ! -e $outputDir ]; then
	mkdir -p $outputDir
fi

# Uncompress the files
#echo "Uncompress the meteo asc files..."
#for src in $inputDirRoot/meteo/asc/$year.$month.??/??/*.asc.bz2
#do
#	if [ $src = "$inputDirRoot/meteo/asc/$year.$month.??/??/*.asc.bz2" ]; then
#		echo "break"
#		break
		
#	fi
#	
#	bunzip2 $src
#done
echo "Creating symbolic links for meteo ascii files..."
for src in $inputDirRoot/meteo/asc/$year.$month.??/00/*00_P000.ana.asc
do
	dest=$outputDir/`basename $src | cut -c1-8`00_000_ana.asc
	
	if [ ! -L $dest ]; then
		ln -s $src $dest
	fi
done

for src in $inputDirRoot/meteo/asc/$year.$month.??/00/*00_P006.ana.asc
do
	dest=$outputDir/`basename $src | cut -c1-8`06_006_ana.asc

	if [ ! -L $dest ]; then
		ln -s $src $dest
	fi
done

for src in $inputDirRoot/meteo/asc/$year.$month.??/00/*00_P006.pcp.asc
do
	dest=$outputDir/`basename $src | cut -c1-8`00_006_pcp.asc

	if [ ! -L $dest ]; then
		ln -s $src $dest
	fi
done

for src in $inputDirRoot/meteo/asc/$year.$month.??/00/*00_P012.pcp.asc
do
	dest=$outputDir/`basename $src | cut -c1-8`06_006_pcp.asc

	if [ ! -L $dest ]; then
		ln -s $src $dest
	fi
done

for src in $inputDirRoot/meteo/asc/$year.$month.??/12/*12_P000.ana.asc
do
	dest=$outputDir/`basename $src | cut -c1-8`12_000_ana.asc

	if [ ! -L $dest ]; then
		ln -s $src $dest
	fi
done

for src in $inputDirRoot/meteo/asc/$year.$month.??/12/*12_P006.ana.asc
do
	dest=$outputDir/`basename $src | cut -c1-8`18_006_ana.asc

	if [ ! -L $dest ]; then
		ln -s $src $dest
	fi
done

for src in $inputDirRoot/meteo/asc/$year.$month.??/12/*12_P006.pcp.asc
do
	dest=$outputDir/`basename $src | cut -c1-8`12_006_pcp.asc

	if [ ! -L $dest ]; then
		ln -s $src $dest
	fi
done

for src in $inputDirRoot/meteo/asc/$year.$month.??/12/*12_P012.pcp.asc
do
	dest=$outputDir/`basename $src | cut -c1-8`18_006_pcp.asc

	if [ ! -L $dest ]; then
		ln -s $src $dest
	fi
done
