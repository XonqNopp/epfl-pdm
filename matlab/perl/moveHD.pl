#!/usr/bin/perl
# Script written and mantained by GI
$myhome = '/home/induni';
if( $#ARGV == -1 ) {
	@heavy = `du -h --max-depth=0 datafiles/* | grep HD`;
	opendir( DIR, 'datafiles' ) or die( ' Cannot open datafiles directory...\n' );
	@datafiles = readdir( DIR );
	closedir( DIR );
	opendir( DIR2, "$myhome/NoTivoli/astra_datafiles" ) or die( ' Cannot open datafiles backup directory...\n' );
	@bckupdir = readdir( DIR2 );
	closedir( DIR2 );
	foreach $df( @datafiles ) {
		if( $df =~ m/HD/ ) {
			$is_backup = 0;
			foreach $bc( @bckupdir ) {
				if( $df eq $bc ) { $is_backup = 1; }
			}
			$size = 0;
			foreach $h( @heavy ) {
				if( $h =~ m/$df/ ) {
					chomp($h);
					$size = $h;
					$size =~ s/^([0-9]*(k|M|G)).*/\1/;
				}
			}
			$old_name = "datafiles/$df";
			$new_name = "$myhome/NoTivoli/astra_datafiles/$df";
			if( $is_backup ) {
				print " - Replacing older version for $df ($size)...\n";
				system( 'rm', $new_name );
				system( 'mv', $old_name, $new_name );
				system( 'scp', '-p', $new_name, "induni\@crpppc275:astra_datafiles_backup/$df" );
			} else {
				print " + Moving $df ($size)...\n";
				system( 'mv', $old_name, $new_name );
				system( 'scp', '-p', $new_name, "induni\@crpppc275:astra_datafiles_backup/$df" );
			}
		}
	}
} else {
	print "Not done yet...\n";
}

exit 0;

