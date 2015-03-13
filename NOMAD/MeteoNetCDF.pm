#!/usr/bin/perl -w

package MeteoNetCDF;
################################################################################
# Class use to write AOD data in a standard format.
#
################################################################################
use strict;
use POSIX qw();
use PDL;
use PDL::NetCDF;

################################################################################
# Class constructor - Set the attributes of the object.
#
#   Parameters:
#	  $refLon 	Reference to a vector of longitude info (lonMin, lonMax, lonStep)
#     $refLat	Reference to a vector of latitude info  (latMin, latMax, latStep)
#
#   Returns:
#     Class reference
################################################################################
sub new {
	my ($proto, $refLonInfo, $refLatInfo) = @_;
	my $class = ref($proto) || $proto;
	my $self  = {};

	# Initialize class attributes
	$self->{fillvalue}      = undef;
	$self->{scaleFactor}  	= undef;
	$self->{units}			= undef;
	$self->{addOffset}  	= undef;
	$self->{scaleFactor}  	= undef;
	$self->{longName}  		= undef;
	$self->{varName}  		= undef;
	$self->{time}           = undef;
	$self->{timeUnits} 		= undef;
	$self->{level}          = undef;
	$self->{levelUnits}     = undef;
	$self->{date} 		    = ();
	$self->{longitude}		= undef;
	$self->{latitude}		= undef;
	$self->{ncobj}			= undef;
	$self->{filename}       = undef;
	
	if (defined($refLonInfo)) {	
		my $indSup = $$refLonInfo[2] - 1;
		$self->{longitude} = (pdl [0..$indSup]) * $$refLonInfo[1] + $$refLonInfo[0];
	}
	
	if (defined($refLatInfo)) {	
		my $indSup = $$refLatInfo[2] - 1;
		$self->{latitude} = (pdl [0..$indSup]) * $$refLatInfo[1] + $$refLatInfo[0];
	}
			
	bless ($self, $class);
	return $self;
}

################################################################################
# Class destructor
#
#   Parameters:
#     None
#
#   Returns:
#     None
################################################################################
sub DESTROY {
	my $self = shift;
	
	$self->{ncobj}->close if defined($self->{ncobj});
}

sub close {
	my $self = shift;
	
	$self->{ncobj}->close if defined($self->{ncobj});	
}

################################################################################
# Set the different attributes
#
#   Parameters:
#     $value	Value to set for a particular attribute
#
#   Returns:
#     None
################################################################################
sub setUnits       { my ($self, $value) = @_; $self->{units}       = $value;}	
sub setAddOffset   { my ($self, $value) = @_; $self->{addOffset}   = $value;}	
sub setScaleFactor { my ($self, $value) = @_; $self->{scaleFactor} = $value;}	
sub setFillValue   { my ($self, $value) = @_; $self->{fillValue}   = $value;}	
sub setVecLongitude{ my ($self, $value) = @_; $self->{longitude}   = $value;}	
sub setVecLatitude { my ($self, $value) = @_; $self->{latitude}    = $value;}	

sub setVecTime { 
	my ($self, $value1, $value2) = @_; 
	
	$self->{time}        = $value1;
	$self->{timeUnits}   = $value2;
}	

sub setVecLevel { 
	my ($self, $value1, $value2) = @_; 
	
	$self->{level}        = $value1;
	$self->{levelUnits}   = $value2;
}	

################################################################################
# Get the different attributes
#
#   Parameters:
#     None
#
#   Returns:
#     Value of a particular attribute
################################################################################
sub getFillValue   { my ($self) = @_; return $self->{fillValue};}	
sub getDate        { my ($self) = @_; return @{$self->{date}};}	

################################################################################
# Set the data time
#
#   Parameters:
#     $refDate			Reference of the data date
#     $refStartDate 	Reference of a reference date
#     $units			Units of the time to store
#
#   Returns:
#     None
################################################################################
sub setTime {
	my ($self, $refDate, $refStartDate, $units) = @_;

	$self->{date} = $refDate;
		
	if ($units =~ m/minutes/) {
		my ($Dd,$Dh,$Dm,$Ds) = Date::Calc::Delta_DHMS(@$refStartDate, @$refDate);
		my $minutes = $Dd*24*60 + $Dh*60 + $Dm + $Ds/60;
		
		$self->{time}      = pdl [$minutes];
		$self->{timeUnits} = sprintf("minutes since %04d-%02d-%02d %02d:%02d", @$refStartDate);
	}
	else {
		die("Date units not supported -> $units");
	}		
}

################################################################################
# Copy attributes and axes from one NetCDF object 
#
#   Parameters:
#     $ncObj			NetCDF obj
#     $varName 			Variable name
#
#   Returns:
#     None
################################################################################
sub setAttributesAndAxes {
	my ($self, $ncObj, $varName) = @_;
	
	# Copy attributes
	eval {
	$self->{timeUnits}   = $ncObj->getatt("units", "time");
	};
	
	$self->{units}       = $ncObj->getatt("units",        $varName);			
	$self->{scaleFactor} = $ncObj->getatt("scale_factor", $varName);			
	$self->{addOffset}   = $ncObj->getatt("add_offset",   $varName);				
	
	eval {
	$self->{fillValue}   = $ncObj->getatt("_FillValue",   $varName);				
	};
	
	# Copy axes
	$self->{longitude}   = $ncObj->get("lon");
	$self->{latitude}    = $ncObj->get("lat");
	
	eval {	
	$self->{time}        = $ncObj->get("time");	
	};
}


################################################################################
# Define
#
################################################################################
sub define {
	my ($self, $filename) = @_;
			
	# Create a netCDF file with the COARDS file standard
	# ==================================================	
	$self->{ncobj} = PDL::NetCDF->new (">$filename");
	$self->{filename} = $filename;
					
	# Global Attributes
	# =================
	$self->{ncobj}->putatt("NOMAD, GRID to NetCDF",  'title');
	$self->{ncobj}->putatt('CARTEL, Universite de Sherbrooke',     'institution');
	$self->{ncobj}->putatt(POSIX::strftime("%F %T %z", localtime), 'creation_date');

	# Dimensions
	# ==========
	$self->{ncobj}->put('lon', ['lon'], short($self->{longitude}));
	$self->{ncobj}->putatt('longitude', 'long_name', 'lon');
	$self->{ncobj}->putatt('degrees_east', 'units' , 'lon');
	$self->{ncobj}->putatt('even', 'point_spacing' , 'lon');
	$self->{ncobj}->putatt(float([360.0]), 'modulo', 'lon');

	$self->{ncobj}->put('lat', ['lat'], short($self->{latitude}));
	$self->{ncobj}->putatt('latitude', 'long_name' , 'lat');
	$self->{ncobj}->putatt('degrees_north', 'units', 'lat');
	$self->{ncobj}->putatt('even', 'point_spacing' , 'lat');
			
	$self->{ncobj}->put('level', ['level'], short($self->{level}));
	$self->{ncobj}->putatt('Isobaric level', 'long_name', 'level');
	$self->{ncobj}->putatt($self->{levelUnits}, 'units', 'level');
	
	$self->{ncobj}->put('time', ['time'], short($self->{time}));
	$self->{ncobj}->putatt('time', 'long_name', 'time');
	$self->{ncobj}->putatt($self->{timeUnits}, 'units', 'time');
}

################################################################################
# Append data to NetCDF file
#
#   Parameters:
#     $filename		Output NetCDF filename
#	  $varName		Variable name
#     $longname		Variable long name
#     $data			Data to write
#
#   Returns:
#     None
################################################################################
sub write {	
	my ($self, $filename, $varName, $longName, $units, $data, $fillValue) = @_;
	
	if (defined($self->{filename})) {	
		if ($self->{filename} ne $filename) {		
			$self->{ncobj}->close if defined($self->{ncobj});
			undef($self->{ncobj});
			undef($self->{filename});
		}
	}
	
	unless (defined($self->{ncobj})) { 
		$self->define($filename);
	}
			
	if ($data->getndims() == 4)	{
		$self->{ncobj}->put($varName, ['time', 'level', 'lat', 'lon'], $data);	
	}
	elsif ($data->getdim(2) == $self->{time}->nelem()) {
		$self->{ncobj}->put($varName, ['time', 'lat', 'lon'], $data);		
	}
	else {
		my @dims = $data->dims();		
		$self->{ncobj}->put($varName, ['time', 'level', 'lat', 'lon'], $data->reshape(@dims, 1));		
	}
	
	# Variables
	# =========
	$self->{ncobj}->putatt($longName, 'long_name',    $varName);
	$self->{ncobj}->putatt($units,    'units',        $varName);
	$self->{ncobj}->putatt($fillValue, '_FillValue',  $varName) if defined($fillValue);
}

1;
