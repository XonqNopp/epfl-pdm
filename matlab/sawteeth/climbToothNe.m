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
%te = astra_out.te( 1, i0:i2);
ne = astra_out.ne( 1, i0:i2);
t = astra_out.t( i0:i2 );
% Compute the derivative
%[ ab dTe ] = interpos( t, te );
[ ab dne ] = interpos( t, ne );
clear ab;

% Find peaks of derivative which are extrema of sawtooth
%fs=find( dTe < -20 );
%fs = find( dne < 500 );

fs = findapprox( dne, 0 );
dfs = abs( diff( dne( fs ) ) );
idx = find( dfs > 200 );
disp( '   Parameter must be changed. Check derivative points with 39799' );
nidx = fs( idx + 1 );
dnen = nidx( find( dne( nidx ) < 0 ) );
%keyboard
c1 = dnen(1);
c2 = dnen(2) - 1;
cmax = fs( find( c1 == fs ) + 1 );

Npts = 4;
step = ceil( (c2-cmax) / (Npts-1) );
%keyboard
for jk = 1:Npts-2
	a(jk) = c1 + jk * step;
end
if a(end) == c2 & a(end) > a(end-1) + 1
	a(end) = a(end) - 1;
end
lisp = [ c1 cmax a c2 ];
%keyboard

if size( varargin, 2 ) < 1 || isempty( varargin{ 1 } ) || varargin{ 1 } == 1
	% Plot
	figure;
	% Subplot 1
	ax(1) = subplot(2,1,1);
	set( gca, 'fontsize', 16 );
	plot( t, ne, 'b', 'markersize', 15, 'linewidth', 2 );
	hold on;
	plot( t(lisp), ne(lisp), 'or', 'markersize', 15, 'linewidth', 2 );
	hold off;
	xlabel( 't' );
	ylabel( 'ne' );
	grid( 'on' );
	% Subplot 2
	ax(2) = subplot(2,1,2);
	set( gca, 'fontsize', 16 );
	plot( t, dne, 'b', 'markersize', 15, 'linewidth', 2 );
	xlabel( 't' );
	ylabel( 'd ne / dt' );
	grid( 'on' );
	hold on;
	plot( t(lisp), dne(lisp), 'or', 'markersize', 15, 'linewidth', 2 );
	hold off;
	% Common properties
	linkaxes( ax, 'x' );
	zoom( 'on' );
	print( '-dpsc', [ 'pics/' int2str( astra_out.shot ) '_saw_ne_dnE.ps' ] );
end
%keyboard
time_back = ( i0 - 1 ) + lisp;
end
