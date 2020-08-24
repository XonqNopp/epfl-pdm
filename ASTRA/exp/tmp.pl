#!/usr/bin/perl
# Script written and mantained by GI
opendir(DIR,'.') or die('Z');
@dd=readdir(DIR);
closedir(DIR);
foreach $d(@dd){
	if( $d =~ m/[0-9]{5}_1_/ ) {
		$newf = $d;
		$newf =~ s/([0-9]{5}_)1_/\1/;
		print " + Moving $d to $newf...\n";
		system('svn','mv',$d,$newf);
	}
}
exit 0;
