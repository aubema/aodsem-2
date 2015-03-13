#!/usr/bin/perl -w

# Usage:
#   script_name nc_filename pgm_filename

use strict;
use File::Basename;
use PDL;
use PDL::NetCDF;
use Date::Calc;
use POSIX qw(strftime);
use Getopt::Long;
use Pod::Usage;

my $debug = 0;

my $everyHour  = 3;     # hours ->  0, 3, 6, 9, 12, 15, 18, 21  
my $domainHour = 0.5;   #           [23:30, 00:30], [2:30, 3:30], [5:30, 6:30] ...  

main();

sub main {
	my $help  = 0;	
	my @minPoints = ();
	my @outputDir = ();
	
	GetOptions('help|?'   => \$help,
			'mp=s'        => \@minPoints,
			'outputDir=s' => \@outputDir,
			'd'           => \$debug)
			or pod2usage(1);
	
	pod2usage(1) if $help;
	$minPoints[0] = 3   if (scalar(@minPoints) == 0);
	$outputDir[0] = "." if (scalar(@outputDir) == 0);

	my $validDomain = $domainHour / $everyHour;
	my $lastDiff  = undef;
	my $lastNcFilename = undef;
	my @lastDataDate = ();
	
	@ARGV = sort(@ARGV);
	foreach my $ncFilename (@ARGV) {
		my ($base,$path,$type) = fileparse($ncFilename, qr{\.nc});	
		
		if ($base =~ m/NOMAD_L2\.(\d\d\d\d)(\d\d)(\d\d)(\d\d)(\d\d)/) {		
			my @dataDate = ($1, $2, $3, $4, $5, 0);
								
			my $hour = $dataDate[3] + $dataDate[4]/60.0 + $dataDate[5]/3600.0;						
			my $hour3 = $hour / $everyHour; # 3 = 3 hours interval
			my $hour3Round = POSIX::floor($hour3 + 0.5);
			my $diff = abs($hour3 - $hour3Round);
			
			# Filter the data that are outside the valid range 
			# (every 3 hours within 30 min range)
			if ($diff > $validDomain) {  # 0.17 = 0.5h / 3h = 30 mins / 180 mins
				print "$ncFilename -> discarded\n";
				next;
			}
			
			# Set the hour, round the min. and sec. 
			my @oldDataDate = @dataDate;  # FOR DEBUG
			my $newHour = $hour3Round * $everyHour; # 3 = 3 hours interval
			@dataDate = Date::Calc::Add_Delta_DHMS(@dataDate, 1, 0, 0, 0) if ($newHour == 24);
			$dataDate[3] = $newHour % 24;
			$dataDate[4] = 0;
			$dataDate[5] = 0;			
									
			print "@oldDataDate\t@dataDate\t" if ($debug);
																	
			#  If the dataDate has changed, save the last valid data
			if (defined($lastDiff)) {			
				my ($Dd,$Dh,$Dm,$Ds) = Date::Calc::Delta_DHMS(@lastDataDate, @dataDate);
				my $diffHours = $Dd * 24 + $Dh + $Dm/60.0 + $Ds/3600.0;
							
				# The data date has changed	
				if ($diffHours > 0) {
					print "write\t" if ($debug);			
					# Write the last valid data		
					my $pgmFilename = sprintf("$outputDir[0]/%04d%02d%02d%02d%02d.pgm", @lastDataDate);	
					nc2pgm($lastNcFilename, $pgmFilename, \@lastDataDate, $minPoints[0]);
				}
				else { # The data date has not changed
					if ($diff > $lastDiff) {
						print "$ncFilename -> skipped\n";
						print "\n" if ($debug);			
						next;
					}				
				}
			}
			
			# Save a copy
			$lastDiff       = $diff;
			$lastNcFilename = $ncFilename;
			@lastDataDate   = @dataDate;			
			print "keep\n" if ($debug);			
		}
		else {
			die("Cannot extract date from $ncFilename\n");						
		}
	}

	# Final write
	if (defined($lastDiff)) {
		print "final write\n" if ($debug);			
		my $pgmFilename = sprintf("$outputDir[0]/%04d%02d%02d%02d%02d.pgm", @lastDataDate);	
		nc2pgm($lastNcFilename, $pgmFilename, \@lastDataDate, $minPoints[0]);
	}
}

sub nc2pgm {
	# Process command line arguments and options
	# ===========================================
	my ($ncFilename, $pgmFilename, $refPgmDate, $minPoints) = @_;
	my ($name, $path, $suffix) = fileparse($pgmFilename, ".pgm");
	
	my $matData;
	my @refDate;
	my @curDate = (0, 0, 0, 0, 0, 0);
	my $dateUnit;

	my $ncobj = PDL::NetCDF->new ("$ncFilename") || die ("$ncFilename: $!");

	my $longitude0;
	my $longitude1;
	my $latitude0;
	my $vecTime;

	my $varlist = $ncobj->getvariablenames();

	foreach my $var (@$varlist){
		if ($var eq "lon") {
			$longitude0 = $ncobj->get($var)->at(0);
			$longitude1 = $ncobj->get($var)->at(1);
		}
		elsif ($var eq "lat") {
			$latitude0  = $ncobj->get($var)->at(0);
		}
		elsif ($var eq "time") {
			$vecTime = $ncobj->get($var);

			# Get the reference time
			my $timeUnits = $ncobj->getatt("units", $var);

			if ($timeUnits =~ m/^(.+) since (.+)-(.+)-(.+) (.+):(.+)/) {
				@refDate = ($2, $3, $4, $5, $6, 0);
				$dateUnit = $1;
			}
			elsif ($timeUnits =~ m/^(.*) since.*/) {
				print "$1\n";
				die();
			}
			else {
				die("timeUnits $timeUnits unknown\n")
			}
		}
		#else {
		elsif ($var eq "aod0550") {
			#print "Found variable $var\n";
			my $longName    = $ncobj->getatt("long_name", $var);
			my $addOffset   = $ncobj->getatt("add_offset", $var);
			my $scaleFactor = $ncobj->getatt("scale_factor", $var);
			my $fillValue;
			eval { $fillValue = $ncobj->getatt("_FillValue", $var); };

			$matData = $ncobj->get($var, {NOCOMPRESS => 1});

			my $filter;

			if (defined($fillValue)) {
				$filter = ($matData != $fillValue);
			}

			$matData = $matData * $scaleFactor + $addOffset;

			if (defined($filter)) {
				$filter = $filter * ($matData < 65.534) * ($matData >= 0);
			}
			else {
				$filter = ($matData < 65.534) * ($matData >= 0);
			}

			# Discard if the number of valid points is under 3
			if (sum($filter > 0) < $minPoints) {
				print "$ncFilename -> Not enough points\n";			
				next;
			}
			
			$matData = rint($matData * 1000 * $filter + ($filter == 0) * 65534);

			if (!defined($vecTime)) {				
				$pgmFilename = "$path$var.${name}$suffix";
				#$pgmFilename = "$path${name}_$var$suffix";
				writePGM($pgmFilename, $matData, $longName, $longitude0, $latitude0, $longitude1 - $longitude0, \@curDate, $refPgmDate);
				$ncobj->close;
				exit;
			}

			die("Only one time is supported\n")	if (nelem($vecTime) > 1);			
			
			for (my $i = 0; $i < nelem($vecTime); $i++) {
				if ($dateUnit eq "days") {
					@curDate = Date::Calc::Add_Delta_DHMS(@refDate, at($vecTime, $i), 0, 0, 0)
				}
				elsif ($dateUnit eq "minutes") {
					@curDate = Date::Calc::Add_Delta_DHMS(@refDate, 0, 0, at($vecTime, $i), 0)
				}
				else {
					die("dateUnit $dateUnit unknown\n")
				}

				#$pgmFilename = "$path$name$var" . "_" . sprintf("%04d%02d%02d%02d%02d" , @curDate) . "$suffix";
				$pgmFilename = "${path}NOMAD_L2.$var.${name}$suffix";
				#$pgmFilename = "$path${name}_$var$suffix";
	
				if (! -e $pgmFilename) {
					print "$ncFilename -> $pgmFilename ($var)\n";
					writePGM($pgmFilename, $matData->slice(":,:,$i"), $longName, $longitude0, $latitude0, $longitude1 - $longitude0, \@curDate, $refPgmDate);
				}
				else {
					print "$pgmFilename already exist\n";
				}
			}	
		}
	}

	$ncobj->close;
}

sub writePGM {
	my ($filename, $matData, $longName, $longitude0, $latitude0, $pixsiz, $refNcDate, $refPgmDate) = @_;

	my $xsize = $matData->slice(':,0,0')->nelem();
	my $ysize = $matData->slice('0,:,0')->nelem();

	open(FILE, "> $filename") || die ("$filename: $!\n");

	print FILE "P2\n";
	print FILE "# $longName\n";
	print FILE "# date $$refPgmDate[3] $$refPgmDate[4] $$refPgmDate[5] $$refPgmDate[2] $$refPgmDate[1] $$refPgmDate[0]\n";
	print FILE "# dateorig $$refNcDate[3] $$refNcDate[4] $$refNcDate[5] $$refNcDate[2] $$refNcDate[1] $$refNcDate[0]\n";	
	print FILE "# pixsiz $pixsiz\n";
	print FILE "# lat0 $latitude0\n";
	print FILE "# lon0 $longitude0\n";
	print FILE "$xsize $ysize\n";
	print FILE "65535\n";

	for (my $j = $ysize - 1; $j >= 0; $j--) {
		for (my $i = 0; $i < $xsize; $i++) {
			print FILE $matData->at($i, $j, 0) . " "; 
		}
		print FILE "\n";
	}

	close(FILE);
}

=head1 NAME

nc2pgm - 

=head1 SYNOPSIS

nc2pgm [OPTIONS] ncfiles  

=head2 Options

Recognized options include:

=over 4

=item -h, --help

Display this help screen.

=item -mp=MINIMUMPOINTS

Set the minimum valid points to create pgm (default: 3)

=item -outputDir=OUTPUTDIR

Set the output directory (default: .)


