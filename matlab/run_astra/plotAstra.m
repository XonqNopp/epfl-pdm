function plotAstra( astra_args, plot_opt )
% plotAstra( astra_args, plot_opt )
%
%   The following variables has to be a structure and MUST have the (*) fields:
%     astra_args : shot (*)
%                  t0 (*)
%                  ECH (*)     : 1 if ECH, 0 if ohmic-only
%                  equ (*)     : 0 to have TeExp, 1 to have TE:EQ, 2 for twin shot 
%                                8 for H-mode
%                  ntimes (*)  : number of times to start the run
%                  delta_t     : time step in conf nodes (default: 50ms)
%                  HD          : suffix for model file for HD run
%                  rerun       : 1 to force rerun (0 default)
%                  store       : 1 to store the results (default)
%                  rewrite     : 1 to force rewrite the EXP file (0 default)
%                  t_offset    : t_offset
%                  TeDivider   : initial divider for the H-mode model file
%                  mix         : structure for twin shot run
%                               struct( shot, what, ntimes, t0, zeff )
%
%     plot_opt  :  q (*)      : quantity you want to plot
%                  name       : label for plot (default is the quantity name)
%                  linespace
%                  conf       : non-0 to have the plot from the conf nodes too
%                               the value is used to scale conf to ASTRA. Ex for ne: 1e19
%                  rho_t      : 0 to plot on rho (default), 1 to plot on time
%                  which_t    : time index of the simulation (default: 'end')
%                               if numel=2, range of time. For X:end, put which_t=[X Inf].
%                  which_rho  : string of rho index (default is 1(center), not used if rho_t==0)
%                  double     : 1 to have the same graph displayed 2 times (not with conf)
%                  nofig      : put 1 if you want the plot to happen in the background
% 
% standard use: plotAstra(astra_args,plot_opt);

astra_args_required_fields = { 'shot', 't0' };
for ii = 1:length( astra_args_required_fields )
	if ~isfield( astra_args, astra_args_required_fields{ ii } )
		error( [ 'astra_args must contain fields ''' astra_args_required_fields{ii} ''', see help plotAstra.' ] );
	end
	eval( [ astra_args_required_fields{ii} ' = astra_args.' astra_args_required_fields{ii} ';' ] );
end
plot_opt_required_fields = {'q'};
for ii = 1:length( plot_opt_required_fields )
	if ~isfield( plot_opt, plot_opt_required_fields{ ii } )
		error( [ 'plot_opt must contain fields ''' plot_opt_required_fields{ii} ''', see help plotAstra.' ] );
	end
	eval( [ plot_opt_required_fields{ii} ' = plot_opt.' plot_opt_required_fields{ii} ';' ] );
end
quantity = q;
clear q;

%%%%% Fetching the variables from astra_args %%%%%
if isfield( astra_args, 'store' ) && astra_args.store == 0
	error( ' If you want to plot, you have to store the results...' );
end
if isfield( astra_args, 'HD' )
	GG = [ '_' astra_args.HD ];
else
	GG = '';
end
%%%%% Fetching the variables from plot_opt %%%%%
% X-option
if isfield( plot_opt, 'rho_t' ) && plot_opt.rho_t == 1
	xlab = 't';
else
	xlab = 'rhovol';
end
% conf plot
if isfield( plot_opt, 'conf' )
	conf_plot = plot_opt.conf;
else
	conf_plot = 0;
end
% double plot
if isfield( plot_opt, 'double' )
	double_plot = plot_opt.double;
else
	double_plot = 0;
end
% linspec
if isfield( plot_opt, 'linespec' )
	linespec = plot_opt.linespec;
else
	linespec = '-b';
end
% name for plot
if isfield( plot_opt, 'name' )
	ylab = plot_opt.name;
else
	ylab = quantity;
end
% nofig
nofig = 0;
if isfield(plot_opt,'nofig')
	nofig = plot_opt.nofig;
end

%%% Run ASTRA %%%
% prepare run astra arguments
% run astra
astra_out = runAstra( astra_args );
% rho index
if isfield( plot_opt, 'which_rho' ) & strcmp( xlab, 't' )
	s_irho = plot_opt.which_rho;
	if ~ischar( s_irho )
		s_irho = int2str( iround( astra_out.rhovol(:,end), plot_opt.which_rho ) );
	end
else
	s_irho = '1';
end
% time index
if isfield( plot_opt, 'which_t' )
	if numel( plot_opt.which_t ) == 1
		which_t = iround( astra_out.t, plot_opt.which_t );
		if abs( astra_out.t( which_t ) - plot_opt.which_t ) > 0.1
			disp( [ ' Time index out of range, will be taken at ' num2str(astra_out.t(which_t)) ] );
		end
		s_it = int2str( which_t );
	else
	%keyboard
		%it_start = iround( astra_out.t, plot_opt.which_t(1) );
		%it_stop  = iround( astra_out.t, plot_opt.which_t(2) );
		%if abs( astra_out.t(it_start) - plot_opt.which_t(1) ) > 0.1
			%disp( [ ' Start time index out of the range, will be taken at ' num2str(astra_out.t(it_start)) ] );
		%end
		%if abs( astra_out.t(it_start) - plot_opt.which_t(1) ) > 0.1
			%disp( [ ' Stop time index out of the range, will be taken at ' num2str(astra_out.t(it_stop)) ] );
		%end
		if plot_opt.which_t(1) < 1
			it_start = 1;
		else
			it_start = plot_opt.which_t(1);
		end
		if plot_opt.which_t(2) > size( astra_out.t, 2 )
			it_stop = size( astra_out.t, 2 );
		else
			it_stop = plot_opt.which_t(2);
		end
		if it_start == it_stop
			disp( ' Time interval too short, taking only one index of time...' );
			s_it = int2str( it_start );
		else
			s_it = [ int2str( it_start ) ':' int2str( it_stop ) ];
		end
	end
else
	if strcmp( xlab, 'rhovol' )
		s_it = 'end';
	else
		s_it = ':';
	end
end
%%% Get results %%%
% CAUTION: For a better understanding, I should change the variable name rho
% Check if field name is allowed
title_first = '';
%keyboard
if ~isfield( astra_out, quantity )
	maybe = '';
	suggest = struct( 'chie', 'he', 'chii', 'hi', 'taue', 'tau_e', 'taue100', 'tau_e', 'taui', 'tau_i' );
	if isfield( suggest, quantity )
		maybe = [ 'You may want to call the quantity "' eval( [ 'suggest.' quantity ] ) '"' ];
	end
	stderr = [ 'The field ' quantity ' is unknown in astra_out...   ' maybe ];
	astra_out_fields = fieldnames( astra_out )'
	error( stderr );
end
q = eval( [ 'astra_out.' quantity ] );
if size( q, 1 ) ~= 1
	if strcmp( xlab, 'rhovol' )
		qu = eval( [ 'q( :, ' s_it ')' ] );
		rho = eval( [ 'astra_out.rhovol( :, ' s_it ' )' ] );
	else
		qu = eval( [ 'q( ' s_irho ', ' s_it ')' ] );
		rho = eval( [ 'astra_out.t( ' s_it ' )' ] );
	end
else
	qu = eval( [ 'q( 1, ' s_it ' )' ] );
	rho = eval( [ 'astra_out.rhovol( end, ' s_it ' )' ] );
end
%keyboard
% Fetch from conf
if conf_plot
	mdsopen( shot );
	astra2conf = struct( 'he', 'chie', 'hi', 'chii', 'tau_e', 'taue100', 'tau_i', 'taui', 'lte', 'r_lte', 'lne', 'r_lne' );
	if isfield( astra2conf, quantity )
		quantity_bis = eval( [ 'astra2conf.' quantity ] );
	else
		quantity_bis = quantity;
	end
	q_tdi = eval( [ 'tdi( ''\results::conf:' quantity_bis ''');' ] );
	rhovol_tdi = tdi('\results::conf:rhovol');
	it = iround( q_tdi.dim{2}, t0 );
end
% Plot %
figpos = [ 3 132 1265 820 ];
figure('name',['plotASTRA: ' quantity]);
if nofig > 0
	set(gcf,'visible','off');
end
set( gca, 'fontsize', 16 );
if double_plot
	subplot(2,1,1);
	set( gca, 'fontsize', 16 );
end
plot( rho, qu, linespec, 'markersize', 15, 'linewidth', 2 );
if double_plot
	if strcmp( xlab, 'rhovol' )
		xlabel( '\rho_V' );
	else
		xlabel( 't' );
	end
	ylabel( ylab );
	if strcmp( xlab, 'rhovol' )
		if numel(t0) == 1 & strcmp( s_it, 'end' )
			title( [ 't0=' num2str( t0 ) ', end of sim' ] );
		elseif numel(t0) == 1 & ~strcmp( s_it, 'end' )
			title( [ 't0=' num2str( t0 ) ', sim time=' num2str( eval( [ 'astra_out.t(' s_it ')' ] ) ) ] );
		elseif numel(t0) ~= 1 & strcmp( s_it, 'end' )
			title( [ 't0=' num2str( t0(1) ) ' to ' num2str( t0(end) ) ', end of sim' ] );
		else
			title( [ 't0=' num2str( t0(1) ) ' to ' num2str( t0(end) ) ', sim time=' num2str( eval( [ 'astra_out.t(' s_it ')' ] ) ) ] );
		end
	else
		ij = iround( astra_out.t, t0 );
		if numel(t0) == 1
			title( [ 't0=' num2str( t0 ) ', \rho_V = ' eval( [ 'num2str( astra_out.rhovol( ' s_irho ', ij ) )' ] ) ] );
		else
			title( [ 't0=' num2str( t0(1) ) ' to ' num2str( t0(end) ) ', \rho_V = ' eval( [ 'num2str( astra_out.rhovol( ' s_irho ', ij ) )' ] ) ] );
		end
	end
	grid( 'on' );
	subplot(2,1,2);
	set( gca, 'fontsize', 16 );
	plot( rho, qu, linespec, 'markersize', 15, 'linewidth', 2 );
end
if conf_plot > 0
	hold on;
	if numel(it) == 1
		plot( rhovol_tdi.data(:,it), q_tdi.data(:,it)./conf_plot, '--r', 'linewidth', 2 );
		legend( 'ASTRA', 'conf', 'Location', 'Best' );
	else
		plot( rhovol_tdi.data(:,it), q_tdi.data(:,it(1))./conf_plot, '--r', 'linewidth', 2 );
		plot( rhovol_tdi.data(:,it), q_tdi.data(:,it(end))./conf_plot, '--g', 'linewidth', 2 );
		legend( 'ASTRA', [ 'conf at t0=' num2str( t0(1) ) ], [ 'conf at t0=' num2str( t0(end) ) ], 'Location', 'Best' );
	end
elseif conf_plot < 0 % needs plotyy
	clf;
	set( gca, 'fontsize', 16 );
	if numel(it) == 1
		[ pyy astra_p conf_p ] = plotyy( rho, qu, rhovol_tdi.data(:,it), q_tdi.data(:,it)./abs(conf_plot) );%, '--r', 'linewidth', 2 );
		% Change display of ASTRA plot
		set( pyy( 1 ), 'fontsize', 16 );
		set( astra_p, 'linewidth', 2 );
		set( astra_p, 'color', 'b' );
		set( pyy( 1 ), 'ycolor', 'b' );
		set( get( pyy( 1 ), 'ylabel' ), 'string', 'ASTRA', 'fontsize', 16 );
		% Change display of nodes plot
		set( pyy( 2 ), 'fontsize', 16 );
		set( conf_p, 'linewidth', 2 );
		set( conf_p, 'linestyle', '--' );
		set( pyy( 2 ), 'ycolor', 'r' );
		set( conf_p, 'color', 'r' );
		set( get( pyy( 2 ), 'ylabel' ), 'string', 'From nodes', 'fontsize', 16 );
		%legend( 'ASTRA', 'conf', 'Location', 'Best' );
	else
		%plot( rhovol_tdi.data(:,it), q_tdi.data(:,it(1))./abs(conf_plot), '--r', 'linewidth', 2 );
		%plot( rhovol_tdi.data(:,it), q_tdi.data(:,it(end))./abs(conf_plot), '--g', 'linewidth', 2 );
		%legend( 'ASTRA', [ 'conf at t0=' num2str( t0(1) ) ], [ 'conf at t0=' num2str( t0(end) ) ], 'Location', 'Best' );
	end
	title_first = [ ylab ', ' ];
	grid( 'on' );
	zoom( 'on' );
end
if strcmp( xlab, 'rhovol' )
	xlabel( '\rho_V' );
else
	xlabel( 't' );
end
if conf_plot >= 0
	ylabel( ylab );
end
title_text = '';
if strcmp( xlab, 'rhovol' )
	if numel(t0) == 1 & strcmp( s_it, 'end' )
		title_text = [ 't0=' num2str( t0 ) ', end of sim' ];
	elseif numel(t0) == 1 & ~strcmp( s_it, 'end' )
		title_text = [ 't0=' num2str( t0 ) ', sim time=' num2str( eval( [ 'astra_out.t(' s_it ')' ] ) ) ];
	elseif numel(t0) ~= 1 & strcmp( s_it, 'end' )
		title_text = [ 't0=' num2str( t0(1) ) ' to ' num2str( t0(end) ) ', end of sim' ];
	else
		title_text = [ 't0=' num2str( t0(1) ) ' to ' num2str( t0(end) ) ', sim time=' num2str( eval( [ 'astra_out.t(' s_it ')' ] ) ) ];
	end
else
	ij = iround( astra_out.t, t0 );
	if numel(t0) == 1 & strcmp( s_irho, '1' )
		title_text = [ 't0=' num2str( t0 ) ', in center' ];
	elseif numel(t0) == 1 & ~strcmp( s_irho, '1' )
		title_text = [ 't0=' num2str( t0 ) ', at \rho_V = ' eval( [ 'num2str( astra_out.rhovol( ' s_irho ', ij ) )' ] ) ];
	elseif numel(t0) ~= 1 & strcmp( s_irho, '1' )
		title_text = [ 't0=' num2str( t0(1) ) ' to ' num2str( t0(end) ) ', in center' ];
	else
		title_text = [ 't0=' num2str( t0(1) ) ' to ' num2str( t0(end) ) ', at \rho_V = ' eval( [ 'num2str( astra_out.rhovol( ' s_irho ', ij ) )' ] ) ];
	end
end
title( [ title_first title_text ] );
grid( 'on' );
zoom( 'on' );
hold off;
if strcmp( xlab, 't' )
	xlabval = num2str( eval( [ 'astra_out.rhovol( ' s_irho ', end )' ] ) );
else
	xlabval = num2str( eval( [ 'astra_out.t( ' s_it ')' ] ) );
end
if numel(t0) == 1
	set(gcf,'paperpositionmode','auto');
	print( '-dpsc', [ 'pics/' int2str( shot ) '_' quantity GG '_' num2str( t0 ) '_' xlab '_' xlabval '_ppm.ps' ] );
	set(gcf,'paperpositionmode','manual','outerposition',figpos);
	print( '-dpsc', [ 'pics/' int2str( shot ) '_' quantity GG '_' num2str( t0 ) '_' xlab '_' xlabval '.ps' ] );
else
	set(gcf,'paperpositionmode','auto');
	print( '-dpsc', [ 'pics/' int2str( shot ) '_' quantity GG '_' num2str( t0(1) ) '_' num2str( t0(end) ) '_' xlab '_' xlabval '_ppm.ps' ] );
	set(gcf,'paperpositionmode','manual','outerposition',figpos);
	print( '-dpsc', [ 'pics/' int2str( shot ) '_' quantity GG '_' num2str( t0(1) ) '_' num2str( t0(end) ) '_' xlab '_' xlabval '.ps' ] );
end
end
