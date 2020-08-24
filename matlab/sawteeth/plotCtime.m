function plotCtime( astra_out, quantities, it1, it2, varargin )
% plotCtime( astra_out, quantities, it1, it2, varargin )
%    Plots the characteristic timescales of a sawtooth
%    quantities : cell containing the quantities you want to plot
%    varargin{ 1 } : 1 for print on separated figures, 0 for all on one subplot without print (default)
%            { 2 } : number of room left in subplot to plot from other script 

N = numel( quantities );
%dd = 3;% Try something with this, n_e is not well plotted
dd = 0;
t = astra_out.t( it1-dd:it2+dd );
t = t - t(1);
t1 = astra_out.t( it1 );
t2 = astra_out.t( it2 );
t1bis = t(1);
t2bis = t(end);
%tprime = t - t1;
if length( varargin ) >= 1 && ~isempty( varargin{ 1 } ) && varargin{ 1 } > 0
	dosub = 1;
else
	dosub = 0;
end
if length( varargin ) >= 2 && ~isempty( varargin{ 2 } ) && varargin{ 2 } > 0
	room = varargin{ 2 };
else
	room = 0;
end

figure;
set( gca, 'fontsize', 16 );
name = '';

for i = 1:N
	qty = quantities{ i };
	if dosub
		%name = [ name '_' qty ];
		subplot( N+room, 1, i );
	end
	if strcmp( qty, 's1' )
		%plots1( astra_out, 2, [ it1 it2 ] );
		q = qty1( astra_out, 'shear', [ int2str(it1-dd) ':' int2str(it2+dd) ] );
	else
		q = eval( [ 'astra_out.' qty '( 1, it1-dd:it2+dd )' ] );
	end
	set( gca, 'fontsize', 16 );
	plot( t, q, '-k', 'linewidth', 2 );
	minq = min( q );
	maxq = max( q );
	if strcmp( qty, 'te' )
		it0fit = find( q == minq );
		%%% HEM %%%
		dte_it = diff(q(1:it0fit*2));
		good_its = find( dte_it > 0.01 );
		it0fit = good_its(1);
		%%% HEM %%%
		t0fit = t( it0fit );
		taufit = logAstra( astra_out, 'te', -1, it1, it2, 1 );
		taue = taue1( astra_out, it1 );
		chie = astra_out.he( end, it1 );
		a = astra_out.ametr( end, it1 );
		a2 = a * a;
		%imax = find( max(q) == q );
		%idces = iround( q(1:imax), 0.9 * max(q) );
		% See what to do with this (90%)
		deltaq = maxq - minq;
		hold on;
		plot( t, minq + deltaq .* ( 1 - exp( - ( t - t0fit ) ./ taue ) ), '--r', 'linewidth', 2 );
		plot( t, minq + deltaq .* ( 1 - exp( - ( t - t0fit ) .* ( chie / a2 ) ) ), '--b', 'linewidth', 2 );
		plot( t, minq + deltaq .* ( 1 - exp( - ( t - t0fit ) ./ taufit ) ), '--m', 'linewidth', 2 );
		hold off;
		legend( 'T_e', '\tau_e', 'a^2/\chi_e', 'fit', 'Location', 'East' );
		title( [ '\tau_e = ' num2str( taue * 1000. ) 'ms, a^2/\chi_e = ' num2str( ( a2 / chie ) * 1000. ) 'ms, \tau_{fit} = ' num2str( taufit * 1000. ) 'ms' ] );
	elseif strcmp( qty, 'ne' )
		taue = taue1( astra_out, it1 );
		%chie = astra_out.he( end, it1 );
		%a = astra_out.ametr( end, it1 );
		%a2 = a * a;
		taufit1 = logAstra( astra_out, 'ne', -1, it1, it2, 1 );
		taufit2 = logAstra( astra_out, 'ne', 1, it1, it2, 1 );
		hold on;
		deltaq = maxq - minq;
		plot( t, minq + deltaq .* ( 1 - exp( - t / taue ) ), '--b', 'linewidth', 2 );
		plot( t, minq + deltaq .* ( 1 - exp( - t / taufit1 ) ), '--r', 'linewidth', 2 );
		plot( t, minq + deltaq .* ( exp( - t / taufit2 ) ), '--m', 'linewidth', 2 );
		hold off;
		legend( 'n_e', '\tau_E', 'fit1', 'fit2', 'Location', 'NorthEast' );
		title( [ '\tau_E = ' num2str( taue * 1000 ) 'ms, \tau_{fit1} = ' num2str( taufit1 * 1000. ) 'ms, \tau_{fit2} = ' num2str( taufit2 * 1000. ) 'ms, x = ' num2str( taue / taufit1 ) ] );
	elseif strcmp( qty, 'q' )
		taufit = logAstra( astra_out, 'q', 1, it1, it2, 1 );
		sit1 = int2str( it1 );
		r1 = qty1( astra_out, 'ametr', sit1 );
		kappa1 = qty1( astra_out, 'elon', sit1 );
		r1bar2 = kappa1 * r1^2;
		r1bar = sqrt( r1bar2 );
		Te0 = astra_out.te( 1, it1 );
		taur = 1e4 * r1bar2 / (1.6^2) * ( Te0 / 20 )^(3/2.);
		m = 9.3e-31;
		nu = astra_out.nuee( 1, it1 );
		e = 1.6e-19;
		ne = 1e19 .* astra_out.ne( 1, it1 );
		eta = m * nu / ( e^2 * ne );
		mu0 = 4e-7 * pi;
		rhoq = sqrt( eta * taufit / mu0 );
		%disp( [ 'Characteristic radius for the safety factor according to fit : ' num2str( rhoq ) ] );
		%taur = mu0 * a2 / eta;
		hold on;
		deltaq = maxq - minq;
		plot( t, minq + deltaq .* ( exp( - t / taur ) ), '--r', 'linewidth', 2 );
		plot( t, minq + deltaq .* ( exp( - t / taufit ) ), '--b', 'linewidth', 2 );
		hold off;
		legend( 'q', '\tau_{\eta}', 'fit', 'Location', 'NorthEast' );
		title( [ '\tau_{\eta} = ' num2str( taur * 1000. ) 'ms, \tau_{fit} = ' num2str( taufit * 1000. ) 'ms, \rho_q / r_1 = ' num2str( rhoq / r1 ) ] );
	end
	xlabel( 'time' );
	ylabel( qty );
	grid( 'on' );
	axis( [ t1bis t2bis 0.95*minq 1.05*maxq ] );
	if ~dosub
		print( '-dpsc', [ 'pics/' int2str( astra_out.shot ) 'Ct_' qty '.ps' ] );
		zoom( 'on' );
		last_fig = figure;
	end
end
zoom( 'on' );
if dosub
	subplot( N+room, 1, N + min( 1, room ) );
	%if room == 0
		%print( '-dpsc', [ 'pics/' int2str( astra_out.shot ) 'Ct' name '.ps' ] );
	%end
else
	close(last_fig);
end
end
