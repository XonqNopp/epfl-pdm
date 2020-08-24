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

$filename = 'fns_equil.m';
open(FN,">".$filename) or die( " Sorry, cannot write in $filename...\n" );
print FN "clear filenames_equil;\n";
foreach $f( @dircontent ) {
	if( $f =~ /LD/ && $f !~ /BAD/ && $f !~ /OLD/ ) {
		$bis = $f;
		$bis =~ s/.*LD_//g;
		$bis =~ s/\.mat//g;
		print FN "filenames_bis.$bis = '$f';\n";
	}
}
print FN "filenames_bis = orderfields( filenames_bis );\n";
# put std at top and
# make second structure with only std to run tests...
print FN "\n";
print FN "filenames_equil.dir = 'datafiles';\n";
print FN "filenames_equil.std = '';\n";
print FN "\n";
print FN "fn_fields = fieldnames( filenames_bis );\n";
print FN "for ii = 1:length(fn_fields)\n";
print FN "   eval( [ 'filenames_equil.' fn_fields{ii} ' = filenames_bis.' fn_fields{ii} ';' ] );\n";
print FN "end\n";
print FN "clear ii fn_fields filenames_bis;\n";
#print FN "filenames_equil\n";
close(FN);
print " $filename rewritten.\n";
exit 0;

