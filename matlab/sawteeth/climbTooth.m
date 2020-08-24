function time_back = climbTooth( astra_out, t0, width, varargin )
% time_back = climbTooth( astra_out, t0, width, varargin )
%   Function that takes the start and the end of a tooth with 3 points between
%       astra_out
%       t0 : time at which you want to start the analysis
%       width : width of the time window
%       varargin{ 1 } : 1 to have the plot (default)

% Indexes definitions
i0 = iround( astra_out.t, t0 );
i2 = iround( astra_out.t, t0 + width );
% Variables definitions
te = astra_out.te( 1, i0:i2);
ne = astra_out.ne( 1, i0:i2);
t = astra_out.t( i0:i2 );
% Compute the derivative
[ ab dTe ] = interpos( t, te );
[ ab dne ] = interpos( t, ne );
clear ab;

% Find peaks of derivative which are extrema of sawtooth
fs=find( dTe < -20 );
fsn = find( dne < 500 );
%fprintf( '  Be careful: this script assumes that you have a good astra_out.\n  It uses the property that the derivative of the temperature in the center\n    is below -20 only for the top and the bottom of the sawtooth and that\n    the derivative for the bottom is higher than for the top.\n\n' );
dfs = diff( fs );
idx = find( max( dfs ) == dfs );
if isempty( idx )
	error( 'climbTooth:low_def', '  Definition is not sufficient !' );
end
ix = idx( 1 );
c1 = fs( ix );
if length( idx ) > 1 && dfs( ix + 1 ) > 1
	c2 = fs( ix + 2 );
else
	c2 = fs( ix + 1 );
end
%keyboard
if c2 - c1 <= 5
	%error( 'climbTooth:low_def', '  Definition is not sufficient ! Check your window size.' );
%elseif c2 - c1 > 3 & c2 - c1 <= 5
	warning( '  Check results, may be wrong because of low definition. Check your window size.' );
end
Npts = 5;
step = ceil( (c2-c1) / (Npts-1) );
%keyboard
for jk = 1:Npts-2
	a(jk) = c1 + jk * step;
end
if a(end) == c2 & a(end) > a(end-1) + 1
	a(end) = a(end) - 1;
end
lisp = [ c1 a c2 ];
%keyboard

if size( varargin, 2 ) < 1 || isempty( varargin{ 1 } ) || varargin{ 1 } == 1
	% Plot
	figure;
	% Subplot 1
	subplot(2,1,1);
	set( gca, 'fontsize', 16 );
	plot( t, te, 'b', 'markersize', 15, 'linewidth', 2 );
	hold on;
	plot( t(lisp), te(lisp), 'or', 'markersize', 15, 'linewidth', 2 );
	hold off;
	xlabel( 't' );
	ylabel( 'Te' );
	grid( 'on' );
	% Subplot 2
	subplot(2,1,2);
	set( gca, 'fontsize', 16 );
	plot( t, dTe, 'b', 'markersize', 15, 'linewidth', 2 );
	xlabel( 't' );
	ylabel( 'd Te / dt' );
	grid( 'on' );
	hold on;
	plot( t(lisp), dTe(lisp), 'or', 'markersize', 15, 'linewidth', 2 );
	hold off;
	% Common properties
	zoom( 'on' );
	print( '-dpsc', [ 'pics/' int2str( astra_out.shot ) '_saw_Te_dTE.ps' ] );
end
%keyboard
time_back = ( i0 - 1 ) + lisp;
end
