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

$filename = 'fns.m';
$stdST = 0;
$stdNoST = 0;
open(FN,">".$filename) or die( " Sorry, cannot write in $filename...\n" );
print FN "clear filenames fina;\n";
foreach $f( @dircontent ) {
	if( $f =~ /HD/ && $f !~ /BAD/ && $f !~ /X3STonly/ && $f !~ /OLD/ ) {
		if( $f =~ m/stdNoST/ ) {
			$stdNoST = 1;
		} elsif( $f =~ m/stdST/ ) {
			$stdST = 1;
		}
		$bis = $f;
		$bis =~ s/.*HD_//g;
		$bis =~ s/\.mat//g;
		print FN "filenames_bis.$bis = '$f';\n";
	}
}
print FN "filenames_bis = orderfields( filenames_bis );\n";
# put std at top and
# make second structure with only std to run tests...
print FN "\n";
print FN "filenames.dir = '$dirname';\n";
if( $stdNoST == 1 ) {
	print FN "filenames.stdNoST = '';\n";
}
if( $stdST == 1 ) {
	print FN "filenames.stdST = '';\n";
}
print FN "\n";
print FN "fn_fields = fieldnames( filenames_bis );\n";
print FN "for ii = 1:length(fn_fields)\n";
print FN "   eval( [ 'filenames.' fn_fields{ii} ' = filenames_bis.' fn_fields{ii} ';' ] );\n";
print FN "end\n";
print FN "clear ii fn_fields filenames_bis;\n";
#print FN "filenames\n";
print FN "\n";
print FN "fina.dir = filenames.dir;\n";
print FN "fina.stdNoST = filenames.stdNoST;\n";
close(FN);
print " $filename rewritten.\n";
exit 0;
