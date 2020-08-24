function plotMovie( astra_out, quantity, varargin )
% plotMovie( astra_out, quantity, varargin )
%

shot   = astra_out.shot;
rhovol = astra_out.rhovol;
t      = astra_out.t;
t0     = astra_out.t0(1);
q = eval( [ 'astra_out.' quantity ] );
qmax = max( max( q ) );
qmin = min( min( q ) );
imax =  ceil( qmax );
imin = floor( qmin );
while ( imax - qmax ) > 0.5
	imax = imax - 0.5;
end
while ( qmin - imin ) > 0.5
	imin = imin + 0.5;
end
itime = size( t, 2 );

ifi=figure;
set( gca, 'fontsize', 16 );
plot( rhovol(:,1), q(:,1), '-b', 'markersize', 15, 'linewidth', 2 );
xlabel( '\rho_{Vol}' );
ylabel( quantity );
grid( 'on' );
title( [ '#' int2str( shot ) ' - t_0 = ' num2str( t0 ) ' - t_{sim} = ' num2str( t(1) ) ] );
axis( [ 0 1 imin imax ] );
pause;

for ii = 1:itime
	figure(ifi);
	set( gca, 'fontsize', 16 );
	plot( rhovol(:,ii), q(:,ii), '-b', 'markersize', 15, 'linewidth', 2 );
	xlabel( '\rho_{Vol}' );
	ylabel( quantity );
	grid( 'on' );
	title( [ '#' int2str( shot ) ' - t_0 = ' num2str( t0 ) ' - t_{sim} = ' num2str( t(ii) ) ] );
	axis( [ 0 1 imin imax ] );
	pause( 0.050 );% 50ms
end

%legend( ##, ##, 'Location', 'Best' );
%zoom( 'on' );
%print( '-dpsc', [ '##.ps' ] );
end
