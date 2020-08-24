#!/usr/bin/perl
# Script written and mantained by GI
$bash = 'quota -s 2> /dev/null';
@back = `$bash`;
foreach $b( @back ) {
	chomp($b);
	if( $b =~ m/14649/ ) {
		$allowed = 14649;
		$used = $b;
		$used =~ s/^ *([0-9]*)M.*/\1/;
		$ratio = 100 * $used / $allowed;
		if( $b =~ m/\*/ ) {
			# TOO MUCH
			$delay = $b;
			$delay =~ s/.*([0-9]+days?).*/\1/;
			if( $used > 16114 ) {
				$NT = sprintf( "\$\$\$ NoTivoli : %.1f %% used ($delay) \$\$\$\n", $ratio );
			} else {
				$NT = sprintf( "#   NoTivoli : %.1f %% used ($delay) #\n", $ratio );
			}
		} else {
			$NT = sprintf( "    NoTivoli : %.1f %% used\n", $ratio );
		}
	} elsif( $b =~ m/4883/ ) {
		$allowed = 4883;
		$used = $b;
		$used =~ s/^ *([0-9]*)M.*/\1/;
		$ratio = 100 * $used / $allowed;
		if( $b =~ m/\*/ ) {
			# TOO MUCH
			$delay = $b;
			$delay =~ s/.*([0-9]+days?).*/\1/;
			if( $used > 5372 ) {
				$local = sprintf( "\$\$\$ Local    : %.1f %% used ($delay) \$\$\$\n", $ratio );
			} else {
				$local = sprintf( "#   Local    : %.1f %% used ($delay) #\n", $ratio );
			}
		} else {
			$local = sprintf( "    Local    : %.1f %% used\n", $ratio );
		}
	}
}
print "$local";
print "$NT";

exit 0;
