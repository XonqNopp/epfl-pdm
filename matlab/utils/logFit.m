function tau = logFit( t, q )
% tau = logFit( t, q )
%   Returns the characteristic time for the
%     exponentially decay qunatity q

q0 = q;
t0 = t;
logq = log(abs(q));
%logq = log(max(q,1e-100));

% PLOT %
%t = t0;
%q = q0;
%logq = log(abs(q));
f1 = figure;
set( gca, 'fontsize', 16 );
plot( t, logq, '-b', 'linewidth', 2 );
xlabel( 't' );
ylabel( 'log' );
grid( 'on' );
disp( '  Choose the domain to plot' );
[ xp yp ] = ginput(2);
%close(f1);

% PLOT 2 %
iit1 = iround( t, xp(1) );
iit2 = iround( t, xp(2) );
t = t( iit1:iit2 );
q = q( iit1:iit2 );
logq = logq( iit1: iit2 );

itp = 0;
tau = 0;
while itp == 0
	if tau == 0
		%f1 = figure;
		figure(f1);
		clf;
		set( gca, 'fontsize', 16 );
		plot( t, logq, '-b', 'linewidth', 2 );
		xlabel( 't' );
		ylabel( 'log' );
		grid( 'on' );
		disp( '  Choose start and end points for the linear fit' );
		[ xg yg ] = ginput(2);

		% FIT %
		t1 = iround( t, xg(1) );
		t2 = iround( t, xg(2) );
		tfit = t( t1:t2 )';
		logfit = logq( t1:t2 )';
		qFit = fit( tfit, logfit, 'poly1' );
		origi = qFit(0);
		slope = qFit(1) - origi;
		tau = abs( 1 / slope );
	end

	% FINAL PLOT %
	% LOG %
	figure(f1);
	clf;
	set( gca, 'fontsize', 16 );
	plot( t, logq, '-b', 'linewidth', 2 );
	xlabel( 't' );
	ylabel( 'log' );
	grid( 'on' );
	hold on;
	plot( t, qFit(t), '--r', 'linewidth', 2 );
	legend( 'data', 'fit', 'Location', 'Best' );
	title( [ 'slope : ' num2str( slope ) ' - tau = ' num2str( tau * 1000 ) 'ms' ] );
	zoom( 'on' );
	% NORMAL
	f2 = figure;
	set( gca, 'fontsize', 16 );
	plot( t0, q0, '-b', 'markersize', 15, 'linewidth', 2 );
	hold on;
	expfit = exp( - ( t0 - tfit(1) ) ./ tau );
	qplot = expfit;
	qplot = min(q) + abs(max(q)-min(q)) .* qplot;
	plot( t0, qplot, '--r', 'linewidth', 2 );
	xlabel( 't' );
	title( [ '\tau = ' num2str( tau * 1000 ) 'ms' ] );
	legend( 'data', 'fit', 'Location', 'Best' );
	grid( 'on' );
	zoom( 'on' );

	itp = input( '   0 for refit, 1 or empty for ok : ' );
	if isempty( itp )
		itp = 1;
	end
	if itp == 2
		itp = 0;
		tau = input( '  Choose a manual tau [ms] : ' );
		tau = tau / 1000;
	elseif itp == 0
		tau = 0;
	end
	close(f1);
	close(f2);
end

end
