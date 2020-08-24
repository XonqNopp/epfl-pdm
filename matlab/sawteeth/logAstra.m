function tau = logAstra( astra_out, qty, sig, it1, it2, varargin )
% tau = logAstra( astra_out, qty, sig, it1, it2, varargin )
%    Returns the characteristic time for the sawtooth considered
%     varargin{ 1 } : close the figures

if isinteger(it1) & isinteger(it2)
	t = astra_out.t( it1:it2 );
else
	t = astra_out.t;
	i1 = iround( t, it1 );
	i2 = iround( t, it2 );
	it1=i1;
	it2=i2;
	t  = t(it1:it2);
end
t = t - t(1);
t0 = t;
if iscell( qty )
	Q = eval( [ 'astra_out.' qty '( 1, it1:it2 )' ] );
	ylab = qty;
else
	Q = qty(it1:it2);
	ylab = '';
end
Q0 = Q;
if sig == 1
	sgn = '';
else
	sgn = '-';
end
logQ = log( Q - 0.999 * min(Q) );

% PLOT %
f1 = figure;
set( gca, 'fontsize', 16 );
plot( t, logQ, '-b', 'markersize', 15, 'linewidth', 2 );
xlabel( 't' );
ylabel( [ 'log(' ylab ')' ] );
grid( 'on' );
fprintf( '\n  Choose the domain to plot\n\n' );
[ xp yp ] = ginput(2);
% PLOT 2 %
iit1 = iround( t, xp(1) );
iit2 = iround( t, xp(2) );
t = t( iit1:iit2 );
Q = Q( iit1:iit2 );
logQ = logQ( iit1: iit2 );
clf;
set( gca, 'fontsize', 16 );
plot( t, logQ, '-b', 'markersize', 15, 'linewidth', 2 );
xlabel( 't' );
ylabel( [ 'log(' ylab ')' ] );
grid( 'on' );
fprintf( '\n  Choose start and end points for the linear fit\n\n' );
[ xg yg ] = ginput(2);
% FIT %
t1 = iround( t, xg(1) );
t2 = iround( t, xg(2) );
tfit = t( t1:t2 )';
if sig == 1
	logfit = logQ( t1:t2 )';
else
	logfit = -logQ( t1:t2 )';
end
QFit = fit( tfit, logfit, 'poly1' );
origi = QFit(0);
slope = QFit(1) - origi;
tau = abs( 1 / slope );
% FINAL PLOT %
% LOG
clf;
set( gca, 'fontsize', 16 );
plot( t, logQ, '-b', 'markersize', 15, 'linewidth', 2 );
xlabel( 't' );
ylabel( [ 'log(' ylab ')' ] );
grid( 'on' );
hold on;
logfit = eval( [ sgn 'QFit(t)' ] );
plot( t, logfit, '--r', 'markersize', 15, 'linewidth', 2 );
legend( 'data', 'fit', 'Location', 'Best' );
title( [ 'slope : ' num2str( slope ) ] );
zoom( 'on' );
print( '-dpsc', [ 'pics/logAstra' int2str( astra_out.shot ) sgn ylab '_' int2str(it1) '_' int2str(it2) 'LOG.ps' ] );
% NORMAL
f2 = figure;
set( gca, 'fontsize', 16 );
plot( t0, Q0, '-b', 'markersize', 15, 'linewidth', 2 );
hold on;
%expfit = exp( QFit(t-tfit(1)) ./ QFit( -tfit(1) ) );
expfit = exp( - ( t0 - tfit(1) ) ./ tau );
%keyboard
if sig == 1
	Qplot = expfit;
else
	Qplot = 1 - expfit;
end
Qplot = min(Q) + abs(max(Q)-min(Q)) .* Qplot;
plot( t0, Qplot, '--r', 'linewidth', 2 );
%plot( t, minminQ + Qplot, '--r', 'linewidth', 2 );
%plot( t, minminQ + deltaQ .* Qplot, '--r', 'linewidth', 2 );
xlabel( 't' );
ylabel( ylab );
title( [ '\tau = ' num2str( tau ) ] );
legend( 'data', 'fit', 'Location', 'Best' );
grid( 'on' );
zoom( 'on' );
print( '-dpsc', [ 'pics/logAstra' int2str( astra_out.shot ) sgn ylab '_' int2str(it1) '_' int2str(it2) '.ps' ] );
%keyboard
if length( varargin ) >= 1 && ~isempty( varargin{1} ) && varargin{1} == 1
	close(f1);
	close(f2);
end
end
