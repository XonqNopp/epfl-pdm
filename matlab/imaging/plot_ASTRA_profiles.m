function plot_ASTRA_profiles( filenames, quantities, varargin )
% plot_ASTRA_profiles( filenames, quantities, varargin )
%  To plot equilibrium profiles (taking end of sim)
%   varargin{ 1 } : additional name for pics
%           { 2 } : put 1 if you want this to happen in the background
%           { 3 } : 1 to plot exp data too (default 0)
%

nofig = 0;
if size(varargin,2)>1 && ~isempty(varargin{2})
	nofig = varargin{2};
end

figpos = [ 3 132 1265 820 ];

if size( varargin, 2 ) > 0 && ~isempty( varargin{1} ) && ~strcmp( varargin{1},'' );
	addname = [ '_' varargin{1} ];
else
	addname = '';
end

plot_conf = 0;
if size(varargin,2)>2 && ~isempty(varargin{3})
	plot_conf = varargin{3};
end

if ~isfield(filenames,'std') && ~isfield( filenames, 'stdNoST' ) & ~isfield(filenames,'stdST')
	error( ' std field is missing in filenames...' );
end
if isfield( filenames, 'dir' )
	astradir = filenames.dir;
	filenames = rmfield( filenames, 'dir' );
else
	astradir = 'datafiles';
end
fn = fieldnames( filenames );
if plot_conf & length(fn) > 1
	error( 'Cannot plot exp data with more than one case' );
end
for ii = 1:length(fn)
	fn_leg{ii} = strrep( fn{ii}, '_', '\_' );
	this_fn = eval( [ 'filenames.' fn{ii} ] );
	disp( [ '  * Loading ' fullfile(astradir,this_fn) '...' ] );
	astra_out = loadastraHD(this_fn,astradir);
	global_t{ii} = astra_out.t;
	eval(['astra_out_' fn{ii} ' = astra_out;']);
	clear astra_out;
end
if strcmp( addname, '' )
	if length(fn) == 2
		addname = [ '_' fn{2} ];
	end
end
if exist('astra_out_stdST','var')
	shot = astra_out_stdST.shot;
	t0   = astra_out_stdST.t0(1);
	rhovol = astra_out_stdST.rhovol(:,end);
elseif exist('astra_out_stdNoST','var')
	shot = astra_out_stdNoST.shot;
	t0   = astra_out_stdNoST.t0(1);
	rhovol = astra_out_stdNoST.rhovol(:,end);
else
	shot = astra_out_std.shot;
	t0   = astra_out_std.t0(1);
	rhovol = astra_out_std.rhovol(:,end);
end
disp( [ '    shot #' int2str(shot) ] );
disp( [ '    t0 = ' num2str(t0) ] );
%t    = astra_out_std.t;


qqs = fieldnames(quantities);
for ii = 1:length(qqs)
	figure('name',qqs{ii});
	if nofig > 0
		set(gcf,'visible','off');
	end
	set( gca, 'fontsize', 16 );
	hold on;
	for kk = 1:length(fn)
		eval( [ 'stored.' fn{kk} '.exist = 1;' ] );
		%rhovol = eval( [ 'astra_out_' fn{kk} '.rhovol(:,end)' ] );
		if eval( [ 'isfield( stored.' fn{kk} ', qqs{ii} )' ] )
			back = eval( [ 'stored.' fn{kk} '.' qqs{ii} ] );
		else
			back = eval( [ 'check_compute_needed( astra_out_' fn{kk} ', qqs{ii}, plot_conf );' ] );
			%back = eval( [ 'check_compute_needed( astra_out_' fn{kk} ', qqs{ii}, 1 );' ] );
			eval( [ 'stored.' fn{kk} '.' qqs{ii} '=back;' ] );
		end
		y_plot = back.data;
		%keyboard
		if eval( [ 'isfield(stored.' fn{kk} '.' qqs{ii} ',''nodes'')' ] )
			y_plot2(:,kk) = y_plot(:,end);
			if kk == 1
				y_plot2(:,kk+1) = eval( [ 'stored.' fn{kk} '.' qqs{ii} '.nodes' ] );
			end
			%this_fn_leg{1} = fn{kk};
			%this_fn_leg{2} = 'exp data';
			%y_plot2(:,kk+1) = y_plot(:,end);
		else
			y_plot2(:,kk) = y_plot(:,end);
		end
	end
	if isfield(back,'nodes')
		this_fn_leg{1} = 'nodes';
		for ij = 1:length(fn_leg)
			this_fn_leg{ij+1} = fn_leg{ij};
		end
	else
		this_fn_leg = fn_leg;
	end
	pt = plot( rhovol, y_plot2 );
	xlabel( '\rho_V' );
	ylabel( [ back.name ' ' back.units ] );
	if length(pt) == 2
		set(pt(2),'linestyle','--');
	else
		legend( this_fn_leg, 'location','eastoutside');
	end
	grid( 'on' );
	zoom( 'on' );
	set(gcf,'paperpositionmode','auto');
	figname = [ 'pics/' int2str(shot) '_' num2str(t0) '_' qqs{ii} '_equil'  addname '_ppm.ps' ];
	print( '-dpsc', figname );
	set(gcf,'paperpositionmode','manual','outerposition',figpos);
	figname = [ 'pics/' int2str(shot) '_' num2str(t0) '_' qqs{ii} '_equil'  addname '.ps' ];
	print( '-dpsc', figname );
	clear y_plot y_plot2 this_fn_leg;
end

end
