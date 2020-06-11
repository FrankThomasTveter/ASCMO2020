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
        
	print "JJ PPM DIFF DIDD\n";
        while (my $row = <$fh>) {
            chomp $row;
	    if ($row =~ m/^#/) {
		# ignore comments
	    } elsif ($row =~ /^\s*(\S*)\s*(\S*)\s*(\S*)\s*(\S*)\s*(\S*)\s*(\S*)\s*(\S*)\s*(\S*)\s*(\S*)/) {
		my $yy   = $1;
		my $mm   = $2;
		my $dd   = $3;
		my $jj   = $4;
		my $ppm  = $5;
		my $ds   = $6;
		my $yyma = $7;
		my $yymd = $8;
		my $yymm = $9;
		my $diff=$ppm-$yyma;
		my $didd=$ppm-$yymd;
		print "$jj $ppm $diff $didd\n";
            } else {
                print ">>>>>>>$row\n";
            }
        }
        close ($fh);
    }
};
