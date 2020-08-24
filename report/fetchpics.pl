#!/usr/bin/perl
# Script written and mantained by GI
$shot = "40080";
$t0 = "0.8";
#@reports  = ( "abstract.tex", "intro.tex", "mhd.tex", "confinement.tex", "implementation.tex", "simulations.tex", "conclusion.tex", "acknowledgements.tex", "data.tex", "sbr.tex", "graphs.tex" );
@reports = @ARGV;
@picgen = ();
@picstd = ();
@picX3  = ();
@picDn  = ();
$picpath = "../matlab/pics";
@default_figs = ( 'te', 'LTe', 'ne', 'Lne', 'ti', 'p_e', 'jtot', 'ibsped', 'q', 'shear', 'upl' );
@default_cycle = ( 'te', 'LTe', 'ne', 'Lne', 'ti', 'p_e', 'jtot', 'ibsped', 'q', 'shear', 'upl' );
@default_profs = ( 'te', 'lte', 'ne', 'lne', 'ti', 'p_e', 'itot', 'ibs', 'q', 'shear', 'upl' );

foreach $report( @reports ) {
	print "  Reading $report...\n";
	open( REP, "<", $report ) or die( " Report not openable :-(\n" );
	@rep = <REP>;
	close(REP);
	$repinline = join(' ',@rep);
	$repinline =~ s/  +/ /g;
	$repinline =~ s/\\subfloat\[\\footnotesize[^\]]*\]\{//g;
	chomp( $repinline );
	@repsplit = split( ' ',$repinline );
	foreach $r( @repsplit ) {
		chomp( $r );
		if( $r =~ /^\\/ && $r !~ /\#[0-9]/ && $r !~ /thisArg/ ) {
			if( $r =~ /includegraphics/ ) {
			# Standard case #
				$r =~ s/^.*includegraphics(\[[^\]]*\])?\{//;
				#$r =~ s/\\label\{[^}]*\}//;
				$r =~ s/\}.*$//;
				$picname = "$r ";
				#print GEN $picname;
				push( @picgen, $picname );
			} elsif( $r =~ /AllFigsBoth/ && $r !~ /newenvironment/ && $r !~ /Ref/ && $r !~ /Sub/ && $r !~ /\\end/ ) {
			# AllFigsBoth #
				$case = $r;
				$case =~ s/^.*AllFigsBoth\}\{([^}]*)\}.*/\1/;
				$rho1 = '0.754';
				$rho2 = '0.860';
				$r =~ s/^.*AllFigsBoth\}\{[^}]*\}\{([^}]*)\}.*/\1/;
				$picname1 = "$picpath/$shot"."_$t0"."_$r"."_$rho1"."_results_$case.eps ";
				$picname2 = "$picpath/$shot"."_$t0"."_$r"."_$rho2"."_results_$case.eps ";
				if( $candidate =~ /cycle/ || $case =~ /std/ ) {
					push( @picstd, $picname1 );
					push( @picstd, $picname2 );
				} elsif( $case =~ /X3/ ) {
					push( @picX3, $picname1 );
					push( @picX3, $picname2 );
				} else {
					push( @picDn, $picname1 );
					push( @picDn, $picname2 );
				}
			} elsif( $r =~ /AllFigs/ && $r !~ /newenvironment/ && $r !~ /Ref/ && $r !~ /Sub/ && $r =~ /^\\begin/ ) {
			# AllFigs #
				$case = $r;
				$case =~ s/^.*AllFigs\}\{([^}]*)\}.*/\1/;
				$rho = $r;
				$rho =~ s/^.*AllFigs\}(\{[^}]*\}){2}\{([^}]*)\}.*/\2/;
				$candidate = $r;
				$candidate =~ s/^.*AllFigs\}(\{[^}]*\}){5}\{([^}]*)\}.*/\2/;
				if( $candidate =~ /resultsplotO/ ) {
					$candidate = 'results_cycle';
				} elsif( $candidate =~ /resultsplot/ ) {
					$candidate = '_results_'.$case;
				} else {
					$candidate = "rhosOK_$case";
				}
				$r =~ s/^.*AllFigs\}(\{[^}]*\}){3}\{([^}]*)\}.*/\2/;
				@qqs = split( ',', $r );
				foreach $q( @qqs ) {
					if( $candidate =~ /^_results/ ) {
						$picname1 = "$picpath/$shot"."_$t0"."_$q"."_0.754$candidate.eps ";
						$picname2 = "$picpath/$shot"."_$t0"."_$q"."_0.860$candidate.eps ";
						if( $candidate =~ /cycle/ || $case =~ /std/ ) {
							push( @picstd, $picname1 );
							push( @picstd, $picname2 );
						} elsif( $case =~ /X3/ ) {
							push( @picX3, $picname1 );
							push( @picX3, $picname2 );
						} else {
							push( @picDn, $picname1 );
							push( @picDn, $picname2 );
						}
					} else {
						$picname = "$picpath/$shot"."_$t0"."_$q"."_$candidate.eps ";
						if( $candidate =~ /cycle/ || $case =~ /std/ ) {
							push( @picstd, $picname );
						} elsif( $case =~ /X3/ ) {
							push( @picX3, $picname );
						} else {
							push( @picDn, $picname );
						}
					}
				}
			} elsif( $r =~ /resultsplotO/ ) {
			# resultsplotO #
				$r =~ s/^.*resultsplotO\{//;
				$r =~ s/\}.*$//g;
				$picname = "$picpath/$shot"."_$t0"."_".$r."_results_cycle.eps ";
				push( @picstd, $picname );
			} elsif( $r =~ /resultsplot/  ) {
			# resultsplot #
				$rho = $r;
				$rho =~ s/^.*resultsplot(\{[^}]*\}){2}\{([^}]*)\}.*/\2/;
				$case = $r;
				$case =~ s/^.*resultsplot\{([^}]*)\}.*/\1/;
				$r =~ s/^.*resultsplot(\{[^}]*\}){3}\{//;
				$r =~ s/\}.*$//;
				$picname = "$picpath/$shot"."_$t0"."_".$r."_".$rho."_results_"."$case.eps ";
				if( $case =~ /X3/ ) {
					#print XXX $picname;
					push( @picX3, $picname );
				} else {
					#print DN $picname;
					push( @picDn, $picname );
				}
			} elsif( $r =~ /rhosOKplot/ ) {
			# rhosOKplot #
				$case = $r;
				$case =~ s/^.*rhosOKplot\{([^}]*)\}.*/\1/;
				$r =~ s/^.*rhosOKplot\{[^}]*\}\{//;
				$r =~ s/\}.*$//;
				$picname = "$picpath/$shot"."_$t0"."_".$r."_rhosOK_$case.eps ";
				if( $case =~ /X3/ ) {
					print XXX $picname;
				} else {
					print DN $picname;
				}
			}
		}
	}
}

# Print GEN
$filegen = "picgen.mk";
open(GEN,">",$filegen) or die( " picgen cannot be written\n" );
print GEN "PICGEN = ";
foreach $p( @picgen ) {
	print GEN $p;
}
close(GEN);
# print STD
$filestd = "picstd.mk";
open(STD,">",$filestd) or die( " picstd cannot be written\n" );
print STD "PICSTD = ";
foreach $p( @picstd ) {
	print STD $p;
}
close(STD);
# Print X3
$fileX3  = "picX3.mk";
open(XXX,">",$fileX3) or die( " picX3 cannot be written\n" );
print XXX "PICX3  = ";
foreach $p( @picX3 ) {
	print XXX $p;
}
close(XXX);
# Print Dn
$fileDn  = "picDn.mk";
open(DN,">",$fileDn) or die( " picDn cannot be written\n" );
print DN  "PICDN  = ";
foreach $p( @picDn ) {
	print DN $p;
}
close(DN);

exit 0;
