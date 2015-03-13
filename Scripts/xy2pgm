#!/usr/bin/perl -w

use strict;
use Getopt::Long;
use Pod::Usage;
use POSIX qw();
use PDL;
use File::Basename;

main();				
				
sub main {
	my $help  = 0;	
	my @latitude  = ();
	my @longitude = ();
	my @minPoints = ();
	my @inputDir = ();
	my @outputDir = ();
	my @angstrom  = ();
	
	GetOptions('help|?'   => \$help,
			'lat=s'       => \@latitude,
			'lon=s'       => \@longitude,
			'mp=s'        => \@minPoints,
			'inputDir=s'  => \@inputDir,
			'outputDir=s' => \@outputDir,
			'ang=s'       => \@angstrom)
			or pod2usage(1);
	
	pod2usage(1) if $help;
	$longitude[0] = "200:0.4:275" if (scalar(@longitude) == 0);
	$latitude[0]  =  "30:0.4:125"  if (scalar(@latitude)  == 0);
	$minPoints[0] = 3           if (scalar(@minPoints) == 0);
	$inputDir[0]  = "."         if (scalar(@inputDir)  == 0);
	$outputDir[0] = "."         if (scalar(@outputDir) == 0);
			
	my @lonInfo = split(/:/, $longitude[0]);
	my @latInfo = split(/:/, $latitude[0]);

	if (scalar(@ARGV) == 0) {	
		# Make all AOT_* 
		my @vecAotFilename	= `ls $inputDir[0]/*.AOT_*.xy 2>/dev/null`;
		chomp(@vecAotFilename);
		execute(\@vecAotFilename, \@latInfo, \@lonInfo, $minPoints[0], $outputDir[0]);
						
		# Make AOT_* from Angstrom file 
		my @angParam =(	"440-870Angstrom",
						"500-870Angstrom");
		my @aotParam = (["AOT_440"],
						["AOT_500", "AOT_670", "AOT_870", "AOT_1020", "AOT_1640"]);		
		my @targetwl = ([470], 
						[550, 860, 940, 1240, 2130]);				
									
		for (my $j = 0; $j < scalar(@angParam); $j++) {		
			my @vecAngFilename = `ls $inputDir[0]/*.$angParam[$j].xy 2>/dev/null`;
			chomp(@vecAngFilename);
			
			foreach my $angFilename (@vecAngFilename) { # 1 Angstrom file for 1 particular date		
				print "Reading $angFilename -> ";
		
				my $mat2 = readXY($angFilename, \@latInfo, \@lonInfo, $minPoints[0]);
				if (!defined($mat2)) {
					print "Not enough points\n";				
					next;
				}
				print "Done\n";
				
				my ($base,$path,$type) = fileparse($angFilename, "\.$angParam[$j]\.xy");			
								
				for (my $i = 0; $i < scalar(@{$aotParam[$j]}); $i++) {					
					@vecAotFilename = ("$path$base.$aotParam[$j][$i].xy");					
					execute(\@vecAotFilename, \@latInfo, \@lonInfo, $minPoints[0], $outputDir[0], $mat2, $targetwl[$j][$i]);	
				}				
			}					
		}		
		return;
	}
		
	my ($angstromFilename, $mat2, $wl2) = (undef, undef, undef);	
	
	if (scalar(@angstrom) > 0) {	
		($angstromFilename, $wl2) = split(/,/, $angstrom[0]);
			
		print "Reading $angstromFilename -> ";
		
		$mat2 = readXY($angstromFilename, \@latInfo, \@lonInfo, $minPoints[0]);
		if (!defined($mat2)) {
			print "Not enough points\n";				
			return;
		}	
				
		print "Done\n";
	}
	
	execute(\@ARGV, \@latInfo, \@lonInfo, $minPoints[0], $outputDir[0], $mat2, $wl2);	
}	

sub execute {
	my ($refVecFilename, $refLatInfo, $refLonInfo, $minPoints, $outputDir, $mat2, $wl2)  = @_;
			
	my $outputFilename;
	my $mat3;
	
	foreach my $inputFilename (@$refVecFilename) {
		my ($base,$path,$type) = fileparse($inputFilename, qr{\.xy});
		
		print "Reading $inputFilename -> ";
										
		my $mat1 = readXY($inputFilename, $refLatInfo, $refLonInfo, $minPoints);		
		if (!defined($mat1)) {
			print "Not enough points\n";				
			next;
		}
										
		if (defined($mat2)) {						
			my $wl1 = $1 if ($inputFilename =~ m/\S+_(\d+)\.\S+/);
			$mat3 = calculate($mat1, $wl1, $mat2, $wl2);
			$outputFilename = "$outputDir/" . substr($base, 0, 12) . ".AOT_$wl2.pgm";
		}					
		else {
			$mat3 = $mat1;
			$outputFilename = "$outputDir/" . $base . ".pgm";					
		}										
				
		# Discard if the number of valid points is under minPoints
		if ((!defined($mat3)) || (sum($mat3 != 65.534) < $minPoints)) {
			print "Not enough points\n";			
			next;
		}
					
		if (-e $outputFilename) {
			print "$outputFilename already exist\n";					
		}
		else {		
			print "Writing $outputFilename\n";
			writePGM($outputFilename, $mat3, $refLatInfo, $refLonInfo);				
		}				
	}	
}	

sub calculate {
	my ($mat1, $wl1, $mat2, $wl2) = @_;

	return undef unless defined($mat1);
	return undef unless defined($wl1);
	return undef unless defined($mat2);
	return undef unless defined($wl2);
	
	my $mask = ($mat1 != 65.534) * ($mat2 != 65.534);			
	my $mat3 = $mat1 * ($wl1/$wl2) ** $mat2;
	
	return $mat3 * $mask + 65.534 * ($mask == 0);		
}

sub readXY {
	my ($filename, $refLatInfo, $refLonInfo, $minPoints) = @_;	
	#my $xsize = POSIX::floor(($$refLonInfo[1] - $$refLonInfo[0]) / $$refLonInfo[2]) + 1;
	#my $ysize = POSIX::floor(($$refLatInfo[1] - $$refLatInfo[0]) / $$refLatInfo[2]) + 1;			
	my $xsize = $$refLonInfo[2];
	my $ysize = $$refLatInfo[2];			
	my $mat   = ones($xsize, $ysize) * 65.534;
	my $count = 0;
	
	#open(FILE, "$filename") || die("$filename: $!");
	open(FILE, "$filename") || return undef;
	
	while (my $line = <FILE>) {
		chomp($line);
		
		if ($line =~ m/(\S+)\s+(\S+)\s+(\S+)/) {
			my $latitude  = $1;
			my $longitude = $2 + ($2 < 0 ? 360 : 0);
			my $data      = $3;
			
			my $i = POSIX::floor(($longitude - $$refLonInfo[0]) / $$refLonInfo[1] + 0.5);
			my $j = POSIX::floor(($latitude  - $$refLatInfo[0]) / $$refLatInfo[1] + 0.5);
			
			next if (($i < 0)||($i >= $xsize));
			next if (($j < 0)||($j >= $ysize));
			
			$mat->set($i, $ysize - 1 - $j, $data);												
			$count++;
		}
	}
		
	close(FILE);
	
	$mat = undef if ($count < $minPoints);
	
	return $mat;
}

sub writePGM {
	my ($filename, $mat, $refLatInfo, $refLonInfo) = @_;

	return unless defined($mat);
	
	my @vecDate = ();
	if (basename($filename) =~ m/^(\d\d\d\d)(\d\d)(\d\d)(\d\d)(\d\d).*/) {
		@vecDate = ($4, $5, 0, $3, $2, $1);
	}
	else {
		die("Cannot extract date from filename: $filename");
	}
			
	open(FILE, "> $filename") || die("$filename: $!");
	
	print FILE "P2\n";
	print FILE "# AOD map from AERONET data\n";
	print FILE "# date @vecDate\n";
	print FILE sprintf("# pizsiz %f\n", $$refLatInfo[1]); 
	print FILE sprintf("# lat0 %f\n", $$refLatInfo[0]);
	print FILE sprintf("# lon0 %f\n", $$refLonInfo[0]); 
	print FILE $mat->getdim(0) . " " . $mat->getdim(1) . "\n";
	print FILE "65535\n";
		
	for (my $j = 0; $j < $mat->getdim(1); $j++) {
		for (my $i = 0; $i < $mat->getdim(0); $i++) {		
			print FILE POSIX::floor($mat->at($i,$j) * 1000 + 0.5) . " ";
		}	
		print FILE "\n";
	}
	
	close(FILE);
}

=head1 NAME

xy2pgm - 

=head1 SYNOPSIS

xy2pgm [OPTIONS] xyfiles  

=head2 Options

Recognized options include:

=over 4

=item -h, --help

Display this help screen.

=item -lat=latmin:latstep:nblat

Set the latitude grid (default: 30:1:50) Latitude must be positive.

=item -lon=lonmin:lonstep:nblon

Set the longitude grid (default: 200:1:110) Longitude must be positive.

=item -mp=MINIMUMPOINTS

Set the minimum valid points to create pgm (default: 3)

=item -outputDir=OUTPUTDIR

Set the output directory (default: .)

=item -ang=ANGSTROMFILENAME,WAVELENGTH

Create a pgm file from angstrom file and wavelength
