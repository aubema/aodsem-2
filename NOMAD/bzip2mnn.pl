#!/usr/bin/perl -w

#use Compress::Bzip2; # qw(:all :constant :utilities :gzip);
use PerlIO::via::Bzip2 level => 9;


main(@ARGV);

sub main {
	my ($filename) = @_;


    open(IN, "< $filename"); 	
    open(OUT, ">:via(Bzip2)", "$filename.bz2");
	
	while (<IN>) {	
		print OUT;
	}

	close(IN);			
	close(OUT);			
}