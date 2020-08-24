#!/usr/bin/perl
# Script written and mantained by GI
if( $#ARGV == -1 ) {
	$dirname = 'datafiles';
} else {
	$dirname = $ARGV[0];
}
opendir(DIR,$dirname) or die( "Sorry, cannot open $dirname...\n" );
@dircontent = readdir(DIR);
closedir(DIR);

foreach $f( @dircontent ) {
	if( $f =~ m/HD/ && $f =~ m/equil/ && $f !~ m/BAD/ ) {
		$newf = $f;
		$newf =~ s/HD/LD/;
		$newf =~ s/_equil//;
		print " + Replacing old version of equil for $newf...\n";
		system('rm','-f',"$dirname/$newf");
		system('mv',"$dirname/$f","$dirname/$newf");
	}
}
exit 0;
