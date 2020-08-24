function figs = plot_chosen_profiles( astra_out, quantities, t_i, t_crash, deltaELM, varargin )
% figs = plot_chosen_profiles( astra_out, quantities, t_i, t_crash, deltaELM, varargin )
%   quantities is a structure which fieldnames are the quantities to plot
%      the value of each field defines how to plot it
%        0 : ask the user
%        1 : max of qty within range given by user
%        2 : same but max of gradient of qty
%   t_i : times from plot_choose_t
%   t_crash : time at which the considered crash occurs
%   deltaELM : time interval between 2 ELMs [ms]
%   varargin{ 1 } : additional name for picture saving
%           { 2 } : 1 to save pictures (default)
%
%   figs : figure handlers
%

figpos = [ 3 132 1265 820 ];
default_lw = 1;
figs = [];

if size( varargin, 2 ) > 0 && ~isempty( varargin{1} ) && ~strcmp( varargin{1}, '' )
	addname = [ '_' varargin{1} ];
else
	addname = '';
end
if size( varargin, 2 ) > 1 && ~isempty( varargin{2} ) && varargin{2} == 0
	savepics = 0;
else
	savepics = 1;
end
% Global needed parameters
shot   = astra_out.shot;
t0     = astra_out.t0(1);
rhovol = astra_out.rhovol(:,end);% Assuming rhovol is almost not varying
volum  = astra_out.volum;
t      = astra_out.t;
if size(t_i,1)>size(t_i,2)
	t_i = t_i';
end
%t_i = t(its);
%for ist=1:length(t_i)
	%its(ist) = iround( t, t_i(ist) );
%end
R0 = 0.88;

qqs = fieldnames( quantities );
for ii = 1:length(qqs)
	this_crash = t_crash;
	its = iround( t, this_crash+t_i );
	back = check_compute_needed( astra_out, qqs{ii} );
	qq = back.data;
	for jj = 1:length(its)
		toplot(:,jj) = qq(:,its(jj));
	end
	f0 = figure;
	set(f0,'position',figpos);
	figs = [ figs f0 ];
	set( gca, 'fontsize', 16 );
	plot( rhovol, toplot, 'linewidth', default_lw );
	%hold on;
	xlabel( '\rho_V' );
	ylabel( [ back.name ' ' back.units ] );
	legend( [ repmat( 't=', length(t_i), 1) num2str( ( t_i' ) .* 1000,'%0.3f' ) repmat('ms', length(t_i), 1) ], 'location', 'eastoutside' );% 1us resolution
	grid( 'on' );
	zoom( 'on' );
	title( [ 't_0(crash)=' num2str(this_crash,'%0.6f') ', \Delta t_{ELM}=' num2str(deltaELM) 'ms' ] );
	if savepics
		%disp( ' Prepare figure for a good display to be printed...' );
		%pause;
		print( f0, '-dpsc', [ 'pics/' int2str( shot ) '_' num2str( t0 ) '_choose_' qqs{ii} addname '.ps' ] );
	end

	clear f0 qq toplot;
end

end
