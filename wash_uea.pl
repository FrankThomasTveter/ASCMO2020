#!/usr/bin/perl
#
use List::Util qw[min max];
my $precision=0;
foreach my $file (@ARGV) {
    if ($file =~ m/^-(\d*)$/) {
        $precision=max(0,min($1,3));
    } else {
        #print ">>>>>>>> Processing $file\n";
        open(my $fh, '<:encoding(UTF-8)', $file)
            or die "Could not open file '$file' $!";
        
	print "YEAR MEAN\n";
        while (my $row = <$fh>) {
            chomp $row;
	    if ($row =~ m/^#/) {
		# ignore comments
	    } elsif ($row =~ /^\s*(\d+)((\s+\S+){12})\s+(\S+)/) {
		print "$1 $4\n";
            }
        }
        close ($fh);
    }
};
