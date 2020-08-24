function varargout = plotsNt( astra_out, quantities, t0, rho0, varargin )
% varargout = plotsNt( astra_out, quantities, t0, rho0, varargin )
%   quantities is a structure which fieldnames are the quantities to plot
%      the value of each field defines how to plot it
%        0 : ask the user
%        1 : max of qty within range given by user
%        2 : same but max of gradient of qty
%   varargin{ 1 } : 1 to have tau_fit log, 2 to have decreasing exp fit,
%                   -1 to have cross-plot, 0 otherwise (default)
%           { 2 } : additional name for pics
%           { 3 } : final plot (default)
%
% varargout{ 1 } : taus from man fit
%

if size( varargin, 2 ) >= 1 && ~isempty( varargin{1} )
	if varargin{1} == 1
		dofit = 'log';
	elseif varargin{1} > 1
		dofit = 'man';
	else
		dofit = '';
	end
	if varargin{1} < 0
		cp = 'x-';
	else
		cp = '-';
	end
else
	cp = '-';
	dofit = '';
end

if size( varargin, 2 ) >= 3 && ~isempty( varargin{3} ) 
	doplot = varargin{3};
else
	doplot = 1;
end

shot     = astra_out.shot;
t        = astra_out.t;
rhovol   = astra_out.rhovol( :, end );
volum    = astra_out.volum;
pressure = astra_out.ne .* astra_out.te;
R0       = 0.88;

qqs = fieldnames( quantities );
if length( rho0 ) > 1
	if length( rho0 ) ~= length( qqs )
		error( ' Either you give 1 rho0 for all, either you give one for each, meaning length(rho0)==length(quantities) !' );
	end
	for qq = 1:length( qqs )
		s_irho{qq} = int2str( iround( rhovol, rho0(qq) ) );
	end
else
	if rho0 < 0
		% Plotting at max(grad p)
		% Er... How to do this in here???
	else
		for uu = 1:length( qqs )
			s_irho{uu} = int2str( iround( rhovol, rho0 ) );
		end
	end
end
s_it = ':';
x2 = 'rhovol';
numplots = length( qqs );
it_0 = iround( t, t0 );
it_minus = iround( t, t0 - 0.002 );
t0bis = floor(t0*1000)/1000;
range = 0.05;% 50ms

%%% PREPARE... %%%
jj = 0;
mustcompute.pressure = 1;
mustderive.ibs  = 1;
mustderive.iohm = 1;
mustderive.itot = 1;
mustderive.icd  = 1;
mustderive.ipol = 1;
mustderive.ipl  = 1;
for ii = 1:numplots
	if isfield( mustcompute, qqs{ii} )
		switch qqs{ii}
			case 'pressure'
				qq = eval( [ 'astra_out.ne( ' s_irho{ii} ', : ) .* astra_out.te( ' s_irho{ii} ', : )' ] );
			otherwise
				error( [ ' Please implement the case ' qqs{ii} ] );
		end
	elseif isfield( mustderive, qqs{ii} )
		qp = eval( [ 'astra_out.' qqs{ii} ] );
		for kk = 1:length(t)
			[ aa qo(:,kk) ] = interpos( volum(:,kk), qp(:,kk) );
		end
		qo = ( 2 * pi * R0 ) .* qo;
		%keyboard
		qq = eval( [ 'qo(' s_irho{ii} ', : )' ] );
		clear aa;
		qqs{ii} = strrep( qqs{ii}, 'i', 'j' );
		figure;
		set( gca, 'fontsize', 16 );
		plot( rhovol, qo );%, ##, 'markersize', 15, 'linewidth', 2 );
		xlabel( 'rhovol' );
		ylabel( qqs{ii} );
		grid( 'on' );
		zoom( 'on' );
	else
		qq = eval( [ 'astra_out.' qqs{ii} '( ' s_irho{ii} ', : )' ] );
	end
	%keyboard
	if max( abs( qq ) ) / min( abs( qq ) ) - 0.01 > 1
		qq_minus = qq( it_minus );
		qq2 = qq - qq_minus;
		if mean( qq2 ) < 0
			qq2 = -qq2;
		end
		jj = jj + 1;
		qq3 = qq2 ./ max( qq2 );
		toplot( jj, : ) = qq3;
		toplot2( jj, : ) = [ max(qq)/min(qq) max(qq)/min(qq) ];
		if strcmp( dofit, 'log' )
			caps = '_logfit';
			disp( [ '*  Fitting ' qqs{ii} '...' ] );
			taufit(jj) = logFit( t(it_0:end), qq3(it_0:end) );
			lqq{ jj } = [ qqs{ii} ' \tau=' num2str( taufit(jj) * 1000 ) 'ms' ];
		elseif strcmp( dofit, 'man' )
			caps = '_manfit';
			i3 = find( max(qq3)==qq3 );
			qq4 = qq3(i3:end);
			%iis = iround( qq3(i3:end), 0.01 )+i3-1;
			% iround does not work with non-monotonic :-P
			lookingfor = 0.01;
			margins = 0.001;
			iis = find( qq4 > lookingfor - margins & qq4 < lookingfor + margins );
			if length(iis) ~= 1
				uu = abs( qq4(iis) - 0.01 );
				uss = find( uu == min(uu) );
				iis = iis(uss);
			end
			iis = iis + i3 - 1;
			%keyboard
			if isempty(iis) | abs( qq3(iis) - 0.01 ) > 0.005
				disp( [ '  No fit for ' qqs{ii} '...' ] );
				lqq{ jj } = qqs{ii};
			else
				factor = sqrt( -log(0.01) );
				taufit(jj) = qq3(iis) / factor;
				lqq{ jj } = [ qqs{ii} ' \tau=' num2str( taufit(jj) * 1000 ) 'ms' ];
			end
		else
			caps = '';
			lqq{ jj } = qqs{ii};
		end
	else
		disp( [ '     ' qqs{ii} ' is not varying enough, removing from list...' ] );
	end
end
%keyboard
if exist( 'taufit', 'var' )
	for ij = 1:length(qqs)
		eval( [ 'taus_ms.' qqs{ij} ' = taufit(ij) * 1000;' ] );
	end
	taufields = fieldnames(taus_ms);
	for ij = 1:length(taufields)
		if eval( [ 'taus_ms.' taufields{ij} ] ) == 0
			taus_ms = rmfield( taus_ms, taufields{ij} );
		end
	end
	%taus_ms
	if ~isempty( fieldnames( taus_ms ) )
		varargout{1} = taus_ms;
	end
end

%%% PLOT %%%
if exist( 'toplot', 'var' ) & doplot
	if size( varargin, 2 ) >= 2 && ~isempty( varargin{2} )
		addname = [ '_' varargin{2} ];
		addname2 = [ ' (' varargin{2} ')' ];
	else
		addname = '';
		addname2 = '';
	end
	figure;
	set( gca, 'fontsize', 16 );
	plot( t, toplot, cp );%, ##, 'markersize', 15, 'linewidth', 2 );
	xlabel( 't [s]' );
	legend( lqq, 'Location', 'NorthEast' );
	grid( 'on' );
	zoom( 'on' );
	if strcmp( dofit, 'man' )
		hold on;
		plot( [t0bis t0bis+range],[0.01 0.01], '--k' );
		hold off;
	end
	axis( [ t0bis t0bis+range -0.1 1.03 ] );
	if length( rho0 ) > 1
		rho2 = rho0;
		qq2 = qqs;
		titstr = '';
		while ~isempty( rho2 )
			jk = find( rho2 == rho2(1) );
			titstr = [ titstr '\rho_{V}=' num2str(rho2(1)) ];
			for pl = jk
				titstr = [ titstr ' ' qq2{pl} ];
			end
			nextrho = find( rho2 ~= rho2(1) );
			wq = 0;
			qq3 = {};
			for ws = nextrho
				wq = wq + 1;
				qq3{wq} = qq2{ws};
			end
			qq2 = qq3;
			clear qq3;
			rho2 = rho2( nextrho );
			titstr = [ titstr ', ' ];
		end
		titstr = titstr( 1:end-2 );
	else
		titstr = [ '\rho_{V}=' num2str( rho0 ) ' for all' ];
	end
	titstr = [ titstr addname2 ];
	title( titstr );
	print( '-dpsc', [ 'pics/' int2str( shot ) '_taus' caps addname '.ps' ] );

	figure;
	set( gca, 'fontsize', 16 );
	plot( [0 1], toplot2 );%, ##, 'markersize', 15, 'linewidth', 2 );
	%hold on;
	%xlabel( '##' );
	%ylabel( '##' );
	legend( lqq, 'Location', 'Best' );
	grid( 'on' );
	zoom( 'on' );
	title( [ 'max is __ times steady-state value' addname2 ] );
	print( '-dpsc', [ 'pics/' int2str( shot ) '_taus' caps addname '_max.ps' ] );

else
	if doplot
		disp( ' # Sorry, no data to plot...' );
	end
end

end
