#!/usr/bin/perl
# Script written and mantained by GI
$myhome = '/home/induni';
$datapath = "datafiles";
$bupath = "/NoTivoli/induni/astra_datafiles_PdM_report";
$bupc = "induni\@crpppc275:astra_datafiles_backup";
if( $#ARGV == -1 ) {
	@heavy = `du -h --max-depth=0 $datapath/* | egrep \\(L\\|H\\)D`;
	opendir( DIR, $datapath ) or die( " Cannot open $datapath directory...\n" );
	@datafiles = readdir( DIR );
	closedir( DIR );
	opendir( DIR2, $bupath ) or die( " Cannot open $bupath directory...\n" );
	@bckupdir = readdir( DIR2 );
	closedir( DIR2 );
	foreach $df( @datafiles ) {
		if( $df =~ m/(L|H)D/ ) {
			$is_backup = 0;
			foreach $bc( @bckupdir ) {
				if( $df eq $bc ) { $is_backup = 1; }
			}
			$size = 0;
			foreach $h( @heavy ) {
				if( $h =~ m/$df/ ) {
					chomp($h);
					$size = $h;
					$size =~ s/^([0-9]*(k|K|M|G)).*/\1/;
				}
			}
			if( $is_backup ) {
				$bash_in = "find $datapath/$df -newer $bupath/$df \! -type l -print";
				@bash_out = `$bash_in`;
				if( $bash_out[0] eq '' ) {
					print " o Backup of $df ($size) is up to date.\n";
				} else {
					print " - Replacing older version of backup for $df ($size)...\n";
					system( 'cp', '-p', "$datapath/$df", "$bupath/$df" );
					system( 'scp', '-p', "$datapath/$df", "$bupc/$df" );
				}
			} else {
				print " + No backup found, saving $df ($size)...\n";
				system( 'cp', '-p', "$datapath/$df", "$bupath/$df" );
				system( 'scp', '-p', "$datapath/$df", "$bupc/$df" );
			}
		}
	}
} else {
	$thispath = shift(@ARGV);
	if( $thispath == $bupath ) {
	} else {
	}
	print "Not done yet...\n";
}

exit 0;
