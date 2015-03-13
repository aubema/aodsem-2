#!/usr/bin/perl -w

use strict;
use Getopt::Long;
use Pod::Usage;
use Date::Calc;
use Archive::Zip;
use Archive::Zip::MemberRead;
use POSIX;

my $debug = 0;

my $everyHour  = 3;     # hours ->  0, 3, 6, 9, 12, 15, 18, 21  
my $domainHour = 0.5;   #           [23:30, 00:30], [2:30, 3:30], [5:30, 6:30] ...  

main();

sub main {
	my $help  = 0;
	my @level = ();
	my @outDir = ();
	my @tinf = ();
	my @tsup = ();
	my @vecEveryHour = ();
	my @vecDomainHour = ();
	my $timeInf = undef;
	my $timeSup = undef;
	
	GetOptions('help|?' => \$help,
			'level=s'  => \@level,
			'outputDir=s' => \@outDir,
			'tinf=s'   => \@tinf,
			'tsup=s'   => \@tsup,
			'everyHour=s' => \@vecEveryHour,
			'domainHour=s' => \@vecDomainHour,
			'd'        => \$debug)
			or pod2usage(1);
	
	pod2usage(1) if $help;
	
	# Set the default values
	@level = ("lev10") unless (scalar(@level) > 0);		   
	@outDir = (".")   unless (scalar(@outDir) > 0);		   
	$everyHour  = $vecEveryHour[0]  if (scalar(@vecEveryHour)  > 0);
	$domainHour = $vecDomainHour[0] if (scalar(@vecDomainHour) > 0);
		
	if (!($level[0] =~ m/lev10|lev15|lev20/))	{
		die("Unknown level $level[0]");
	}
		
	# Date 2002-12-31 00:00:00
	#$timeInf = Date::Calc::Date_to_Time(split(/[ :\,]/, $tinf[0])) if (scalar(@tinf) > 0);
	#$timeSup = Date::Calc::Date_to_Time(split(/[ :\,]/, $tsup[0])) if (scalar(@tsup) > 0);
	
	# Date 20021231000000
	$timeInf = Date::Calc::Date_to_Time(($tinf[0] =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)(\d\d)(\d\d)/)) if (scalar(@tinf) > 0);
	$timeSup = Date::Calc::Date_to_Time(($tsup[0] =~ m/(\d\d\d\d)(\d\d)(\d\d)(\d\d)(\d\d)(\d\d)/)) if (scalar(@tsup) > 0);	
		
	print "Start time: $timeInf ($tinf[0])\n" if defined($timeInf);
	print "Stop time:  $timeSup ($tsup[0])\n" if defined($timeSup);
	
	# Create output directory
	mkdir($outDir[0]) unless (-e $outDir[0]);											
			
	foreach my $inputFilename (@ARGV) {
		foreach my $lev (@level) {						
			my $outputDir = "$outDir[0]/$lev";
			mkdir($outputDir) unless (-e $outputDir);											
						
			print "Processing $inputFilename, Level $lev\n";
			my $count = readAeronetZipFile($inputFilename, $lev, $outputDir, $timeInf, $timeSup);
			print "\t$count files in $outputDir\n";
		}
	}
}

sub readAeronetZipFile {
	my ($filename, $level, $outDir, $timeInf, $timeSup) = @_;
	my $zip        = new Archive::Zip($filename);	
	my @vecMembers = $zip->memberNames();
  	my $count = 0;
	
	my @vecDataInf = ();
	my @vecDataSup = ();
	@vecDataInf = Date::Calc::Time_to_Date($timeInf) if (defined($timeInf));
	@vecDataSup = Date::Calc::Time_to_Date($timeSup) if (defined($timeSup));
		
	foreach my $member (@vecMembers) {		
		if ($member =~ m/^(\d\d)(\d\d)(\d\d)_(\d\d)(\d\d)(\d\d)_[\w\-]+.$level/) {
			print "\tFound $member\n";			
						
			my $fh = new Archive::Zip::MemberRead($zip, $member);			
			$count += parseAeronetFile($fh, $outDir, $timeInf, $timeSup);  			
			$fh->close();			
		}					
	}	
	
	return $count;
}

sub parseAeronetFile {
	my ($fh, $outDir, $timeInf, $timeSup) = @_;				
	my $level = undef;
	my @label = ();
	my $locations = undef;
	my $longitude = undef;
	my $latitude  = undef;
	my $elevation = undef;
	my $lastDiff  = undef;
	my @lastData  = ();
	my @lastDataDate = ();
	my $count = 0;
	my $validDomain = $domainHour / $everyHour;
			
	# Label description:
	#
	# ( 0) Date(dd-mm-yy),	(13) Water(cm),		(25) 440-870Angstrom,
	# ( 1) Time(hh:mm:ss),	(14) %Error_1020,	(26) 380-500Angstrom,
	# ( 2) Julian_Day,		(15) %Error_870,	(27) 440-675Angstrom,
	# ( 3) AOT_1020,		(16) %Error_670,	(28) 500-870Angstrom,
	# ( 4) AOT_870,			(17) %Error_500,	(29) 340-440Angstrom,
	# ( 5) AOT_670,			(18) %Error_440,	(30) 440-675Angstrom(Polar),
	# ( 6) AOT_500,			(19) %Error_380,	(31) Last_Processing_Date,
	# ( 7) AOT_440,			(20) %Error_340,	(32) Solar_Zenith_Angle
	# ( 8) AOT_380,			(21) %Error_532,
	# ( 9) AOT_340,			(22) %Error_535,
	# (10) AOT_532,			(23) %Error_1640,
	# (11) AOT_535,			(24) %WaterError,
	# (12) AOT_1640,
	my @vecCol = (3,4,5,6,7,8,9,12,13,25,26,27,28,29,30); # 15 parameters			
	
  	while (my $line = $fh->getline()) {
		chomp($line);

		# Detect product level (line 1)
		if ($line =~ m/^Level/) {
		}
		# Locations info (line 2)
		elsif ($line =~ m/^Location=(.*),long=(.*),lat=(.*),elev=(.*),Nmeas/) {
			$locations = $1;
			$longitude = $2;
			$latitude  = $3;
			$elevation = $4;
		}
		# Locations info (line 2)
		elsif ($line =~ m/^Locations=(.*),long=(.*),lat=(.*),elev=(.*),Nmeas/) {
			$locations = $1;
			$longitude = $2;
			$latitude  = $3;
			$elevation = $4;
		}
		# (line 3)
		elsif ($line =~ m/^AOT Level (.*),All/) {
			$level = $1;
		}
		# (line 4)	Label
		elsif ($line =~ m/^Date/) {
			@label = split(/,/, $line);
		}
		# Data (line 5+)
		elsif ($line =~ m/^\d\d\:\d\d\:\d\d\d\d/) {
			my @data = split(/,/, $line);
			my @dataDate = (reverse(split(/:/, $data[0])), split(/:/, $data[1]));
			
			my $dataTime = Date::Calc::Date_to_Time(@dataDate);
			
			next if ((defined($timeInf)) && ($dataTime < $timeInf));
			last if ((defined($timeSup)) && ($dataTime > $timeSup));
			
			my $hour = $dataDate[3] + $dataDate[4]/60.0 + $dataDate[5]/3600.0;						
			my $hour3 = $hour / $everyHour; # 3 = 3 hours interval
			my $hour3Round = POSIX::floor($hour3 + 0.5);
			my $diff = abs($hour3 - $hour3Round);
						
			# Filter the data that are outside the valid range 
			# (every 3 hours within 30 min range)
			next if ($diff > $validDomain);  # 0.17 = 0.5h / 3h = 30 mins / 180 mins
			
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
					$count += writeToFile($outDir, \@lastDataDate, \@lastData, \@label, $latitude, $longitude, $locations, $elevation, \@vecCol);
				}
				else { # The data date has not changed
					if ($diff > $lastDiff) {
						print "\n" if ($debug);			
						next;
					}				
				}
			}
			
			# Save a copy
			$lastDiff     = $diff;
			@lastData     = @data;
			@lastDataDate = @dataDate;			
			print "keep\n" if ($debug);			
		}
	}
	
	# Final write
	if (defined($lastDiff)) {
		print "final write\n" if ($debug);			
		$count += writeToFile($outDir, \@lastDataDate, \@lastData, \@label, $latitude, $longitude, $locations, $elevation, \@vecCol);
	}

	return $count;
}

sub writeToFile {
	my ($outDir, $refDataDate, $refData, $refLabel, $latitude, $longitude, $locations, $elevation, $refVecCol) = @_;	
	my $count = 0;

	my @dataDate = reverse(@$refDataDate);
	$dataDate[4] = $dataDate[4] - 1;
	$dataDate[5] = $dataDate[5] - 1900;	
	
	foreach my $i (@$refVecCol) {					
		#my $outputFilename = POSIX::strftime("$outDir/%Y%m%d%H%M", @dataDate) . $$refLabel[$i] . ".xy";
		#my $outputFilename = POSIX::strftime("$outDir/$$refLabel[$i]/%Y.%m/%d%H%M", @dataDate) . ".xy";
		my $outputDir = POSIX::strftime("$outDir/%Y.%m", @dataDate);	
		mkdir($outputDir) unless (-e $outputDir);											
					
		my $outputFilename = POSIX::strftime("$outputDir/%Y%m%d%H%M.$$refLabel[$i].xy", @dataDate);
		
		if ($$refData[$i] =~ m/^[\d\-]/) {		
			open(FILE, ">> $outputFilename") || die("$outputFilename: $!");
			print FILE "$latitude $longitude $$refData[$i] Lieu : $locations altitude : $elevation\n";
			close(FILE);
		}
		
		$count++;
	}
	
	return $count;
}

=head1 NAME

aero2xy - 

=head1 SYNOPSIS

aero2xy [OPTION] zipfiles  

=head2 Options

Recognized options include:

=over 4

=item -h, --help

Display this help screen.

=item -level=LEVEL

Select the product level: lev10, lev15, lev20 (default: lev10)

=item -outputDir=OUTPUTDIR

Set the output directory (default: .)

=item -tinf=TIME

Set the time lower limit. Format YYYYMMDDHHmmss (ex.: "20021231000000") (default: none)

=item -tsup=TIME

Set the time upper limit. Format YYYYMMDDHHmmss (ex.: "20021231 000000") (default: none)

=item -everyHour=EVERYHOUR

Set the hour interval. (default: 3)

=item -domainHour=DOMAINHOUR

Set the hour domain. (default: 0.5)