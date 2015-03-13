#!/usr/bin/perl -w

use strict;
use Getopt::Long;
use Pod::Usage;
use PDL;
use PDL::IO::Grib;
use File::Basename;

use lib("/home/level3/bin");
use MeteoNetCDF;

main();

###############################################################################
#  Main
#
#    Parameters:
#      None
#
#    Returns:
#      None
################################################################################
sub main {
	my $help  = 0;
	my @inDir = ();
	my @outDir = ();
	
	GetOptions('help|?'   => \$help,
			'inputDir=s'  => \@inDir,
			'outputDir=s' => \@outDir)
			or pod2usage(1);
	
	pod2usage(1) if $help;
	
	# Set undefined values
	@inDir  = (".") unless (scalar(@inDir)  > 0);		   
	@outDir = (".") unless (scalar(@outDir) > 0);		   

	# Create output directory if necessary			
	mkdir($outDir[0]) unless (-e $outDir[0]);
		
	# Get the date and hour
	my @vecFile = `ls "$inDir[0]"/CMC_glb_UGRD_ISBL_1000_latlon1x1_*_P000.grib`;
		
	foreach my $file (@vecFile) {
		if (basename($file) =~ /CMC_glb_UGRD_ISBL_1000_latlon1x1_(\d\d\d\d)(\d\d)(\d\d)(\d\d)_P000.grib/) {		
			my @date = ($1, $2, $3, $4, 0, 0);
			
			print "Find @date\n";
			loopParameter($inDir[0], $outDir[0], \@date); 
		}
	}	
}

sub loopParameter {
	my ($inputDir, $outputDir, $refDate) = @_;

	my @lonInfo = (-180, 1, 361);
	my @latInfo = ( -85, 1, 171);	
	my $fillValue = -9999;
	
	my $ncobj = new MeteoNetCDF(\@lonInfo, \@latInfo);
	$ncobj->setVecTime(pdl([0 ,6, 12, 18]), sprintf("hours since %4d-%02d-%02d %02d:%02d", @$refDate));
	$ncobj->setVecLevel(pdl([1000, 925, 850, 700, 500, 400, 300, 250, 200, 150, 100, 50]), "hPa");	
		
	foreach my $parameter ("UGRD", "VGRD", "HGT", "TMP", "DEPR", "APCP") {	
		my $varName    = lc($parameter);
		my $outputFile = sprintf("${Center}_${model}.%4d%02d%02d%02d.nc", @$refDate);	
		my ($data, $longName, $units) = loopHour($parameter, $inputDir, $refDate, $fillValue);	
		
		print "Writing parameter: $varName\n";						
		$ncobj->write("$outputDir/$outputFile", $varName, $longName, $units, $data, float([$fillValue]));
	}
}

sub loopHour {
	my ($parameter, $inputDir, $refSate, $fillValue) = @_;

	my @vecHour   = ($parameter eq "APCP" ? (6, 12, 18) : (0, 6, 12, 18));
	my $hourInd   = ($parameter eq "APCP" ? 1 : 0);	
	
	my $data     = zeroes(float, 361, 171, scalar(@vecLevel), 4);
	my $longName = "n/a";
	my $units    = "n/a";
					
	foreach my $hour (@vecHour) {			
		my ($data2, $longName2, $units2) = readGrib($parameter, $inputDir, $refDate, $hour, $fillValue);
				
		$data->slice(":,:,:,$hourInd") .= $data2;
		$longName = $longName2 if defined($longName2);
		$units    = $units2    if defined($units2);
		
		$hourInd++;
	}	
	
	$data->reshape(361, 171, 4) if ($parameter eq "APCP");  ## Only 1 level
	
	return ($data, $longName, $units);	
}

sub readGrib {
	my ($parameter, $inputDir, $refDate, $hour, $fillValue) = @_;

	my $Center         = "CMC";
	my $model          = "glb";
	my $grid_qualifier = "latlon1x1";	
	my $date           = sprintf("%4d%02d%02d%02d", @$refDate);
	my $Phour          = sprintf("P%03d", $hour);
					
	my $level_type = ($parameter eq "APCP" ? "SFC" : "ISBL");	
	my @vecLevel   = ($parameter eq "APCP" ? (0) : (1000, 925, 850, 700, 500, 400, 300, 250, 200, 150, 100, 50));
	
	my $data     = zeroes(float, 361, 171, scalar(@vecLevel));
	my $longName = undef;
	my $units    = undef;
					
	foreach my $level (@vecLevel) {
		my $gribFile = "${Center}_${model}_${parameter}_${level_type}_${level}_${grid_qualifier}_${date}_${Phour}.grib";				
		
		if (-e "$inputDir/$gribFile") {
			my $gh = new PDL::IO::Grib("$inputDir/$gribFile");
			my $f  = $gh->anyfield();														
			
			if (!defined($longName)) {
				my $val = $f->pds_attribute(9);
				
				if ($val eq  7) { $longName = "Geopotential height" ; $units = "Gpm"; }		# GZ  GEOPOTENTIEL
				if ($val eq 11) { $longName = "Temperature"         ; $units = "K"; }		# TT  TEMPERATURE
				if ($val eq 18) { $longName = "Dew point depression"; $units = "K"; }		# ES  ECART DU POINT DE ROSEE
				if ($val eq 33) { $longName = "u-component of wind" ; $units = "m/s"; }		# UU  COMPOSANTE U DU VENT (SELON L AXE DES X)
				if ($val eq 34) { $longName = "v-component of wind" ; $units = "m/s"; }		# VV  COMPOSANTE V DU VENT (SELON L AXE DES Y)
				if ($val eq 61) { $longName = "Total precipitation" ; $units = "kg/m2"; }	# PR  PRECIP
			}
						
			$data->slice(":,:,$levelInd") .= $f->read_data($gh->{_FILEHANDLE});
		}
		else {
			print "$inputDir/$gribFile unavailable\n";
			
			$data->slice(":,:,$levelInd") .= ones(float, 361, 171) * $fillValue;					
		}				
		
		$levelInd++;				
	}
					
	return ($data, $longName, $units);
}

=head1 NAME

grib2nc.pl - 

=head1 SYNOPSIS

grib2nc.pl [OPTIONS]

=head2 Options

Recognized options include:

=over 4

=item -h, --help

Display this help screen.

=item -inputDir=INPUTDIR

Set the input directory (default: .)

=item -outputDir=OUTPUTDIR

Set the output directory (default: .)

