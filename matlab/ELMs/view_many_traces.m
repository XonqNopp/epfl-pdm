function view_many_traces( results_args, varargin )
% view_many_traces( results_args, varargin )
%   results_args is a structure with the following fields
%        (starred field are necessary)
%     astradir (*)   : directory where the files are
%     quantities (*) : quantities of interest (structure)
%                      if value == 1, will set y_pre = 0, if 0 not, if neither used as rhoped
%     tis (*)        : times of interest (can be empty if rhos provided)
%     rhos0 (*)      : rhos of interest (can be empty)
%     study (*)      : which case you are studying: crash/recovery
%     dont_close     : 1 to keep the figures opened
%     addname        : additional name for savings
%     addnote        : additional note for legend (cell the size of rhos0)
%     save_pics      : 1 to save the pictures (default)
%     nonew_rhos     : 1 if you don't want to choose new rhos
%  All other fields are used to fetch data and must be a structure with
%   the fields fn (filename), t (ELM time), deltaELM (ELM interval [ms]), tauELM (ELM duration [us])
%
%     varargin{ 1 } : put 1 if you want this to happen in the background
%

nofig = 0;
if size(varargin,2)>0 && ~isempty(varargin{1})
	nofig = varargin{1};
end

required_fields={'astradir','quantities','tis','rhos0','study'};
for ii = 1:length(required_fields)
	if ~isfield(results_args,required_fields{ii})
		error( [ ' Missing field ' required_fields{ii} ] );
	end
	eval( [ required_fields{ii} ' = results_args.' required_fields{ii} ';' ] );
	results_args = rmfield(results_args,required_fields{ii});
end
t_i = tis;
if strcmp(astradir,'')
	astradir = 'datafiles';
end

locleg = 'eastoutside';

optional_fields.dont_close = 0;
optional_fields.addname = '';
optional_fields.nonew_rhos = 0;
optional_fields.save_pics = 1;
optional_fields.addnote = {};
of = fieldnames(optional_fields);
for ii = 1:length(of)
	if isfield(results_args,of{ii})
		eval( [ of{ii} ' = results_args.' of{ii} ';' ] );
		results_args = rmfield( results_args, of{ii} );
	else
		eval( [ of{ii} ' = optional_fields.' of{ii} ';' ] );
	end
end
if ~strcmp(addname,'')
	addname = ['_' addname];
end
if length(addnote) ~= length(rhos0)
	addnote = {};
end

%if ~isfield(results_args,'stdST') & ~isfield(results_args,'stdNoST')
	%error( ' std(No)ST field is missing in filenames...' );
%end
deltaELM = 0;
tauELM = 0;
deltaELM_OK = 0;
tauELM_OK = 0;
allcrashes = [];
fn = fieldnames( results_args );
only_two = 0;
if length(fn) == 2
	only_two = 1;
end
for ii = 1:length(fn)
	if eval( [ '~isfield(results_args.' fn{ii} ',''fn'') | ~isfield(results_args.' fn{ii} ',''t'') | ~isfield(results_args.' fn{ii} ', ''deltaELM'' ) | ~isfield(results_args.' fn{ii} ', ''tauELM'' )' ] )
		error( [ ' Missing field for case ' fn{ii} ] );
	end
	this_arg = eval(['results_args.' fn{ii}]);
	if deltaELM == 0
		deltaELM_OK = 1;
		deltaELM = this_arg.deltaELM;
	elseif deltaELM_OK && deltaELM ~= this_arg.deltaELM
		deltaELM_OK = 0;
	end
	if tauELM == 0
		tauELM_OK = 1;
		tauELM = this_arg.tauELM;
	elseif tauELM_OK && tauELM ~= this_arg.tauELM
		tauELM_OK = 0;
	end
	allcrashes(ii) = this_arg.t;
	fn_leg{ii} = regexprep( strrep( fn{ii}, '_', '\_' ), '(No)?ST', '' );
	this_fn = this_arg.fn;
	disp( [ '  * Loading ' fullfile(astradir,this_fn) '...' ] );
	load(fullfile(astradir,this_fn));
	global_t{ii} = astra_out.t;
	eval(['astra_out_' fn{ii} ' = astra_out;']);
	clear astra_out;
end
if strcmp( addname, '' )
	if length(fn) == 2
		addname = [ '_' fn{2} ];
	end
end
tauELM = tauELM / 1000;

astra_out_tmp = eval( [ 'astra_out_' fn{1} ] );
t_crash_tmp   = eval( [ 'results_args.' fn{1} '.t' ] );
deltaELM_tmp  = eval( [ 'results_args.' fn{1} '.deltaELM']);
tauELM_tmp    = eval( [ 'results_args.' fn{1} '.tauELM']);

disp( ' Storing rhos of interest...' );
if isempty( rhos0 )
	rhos = choose_rhos(astra_out_tmp,quantities,t_i,t_crash_tmp,deltaELM_tmp,'');
else
	if nonew_rhos
		rhos = rhos0;
	else
		answer = input( ' rhos provided, do you want to rechoose them anyway ? [1/0] : ' );
		if answer
			rhos = choose_rhos(astra_out_tmp,quantities,t_i,t_crash_tmp,deltaELM_tmp,'',rhos0);
		else
			rhos = rhos0;
		end
	end
end
if size(rhos,1) < size(rhos,2)
	rhos = rhos';
end
clear t_i; % no more needed variables

shot = astra_out_tmp.shot;
t0   = astra_out_tmp.t0(1);
disp( [ '    shot #' int2str(shot) ] );
disp( [ '    t0 = ' num2str(t0) ] );

if save_pics
	filename_rhos = [ 'datafiles/rhos_' int2str(shot) '_' num2str(t0) '.mat' ];
	fprintf( [ '  Saving rhos in ASCII in ' filename_rhos '...' ] );
	save( filename_rhos, 'rhos', '-ascii' );
	fprintf( '  Done !\n' );
end

% Using common time vector
%disp( ' Looking for a common time vector, taking the one with the less elements...' );
%common_t = [];
%common_crash = 0;
%which_t = '';
%for ii = 1:length(global_t)
	%if isempty(common_t) || numel(global_t{ii}) < numel(common_t)
		%common_t = global_t{ii} - allcrashes(ii);
		%which_t = fn{ii};
	%end
%end
%clear global_t;
%disp( [ ' Quantities mapped on t from ' which_t ' case' ] );

qqs = fieldnames( quantities );
% Make one figure for each quantity for each rho
figpos = [ 3 132 1265 820 ];
xunit = 'ms';
for ii = 1:length(qqs)
	q_val = eval( [ 'quantities.' qqs{ii} ] );
	set_zero = 0;
	if q_val >= 1
		set_zero = q_val;
		q_val = q_val - 1;
	end
	for jj = 1:length(rhos)
		figure('name',qqs{ii});
		if nofig > 0
			set(gcf,'visible','off');
		end
		set(gca,'fontsize',16);
		hold on;
		title_str = '';
		title_str2 = '';
		for kk = 1:length(fn)
			this_crash    = eval( [ 'results_args.' fn{kk} '.t' ] );
			this_deltaELM = eval( [ 'results_args.' fn{kk} '.deltaELM' ] );
			this_tauELM   = eval( [ 'results_args.' fn{kk} '.tauELM' ] );
			this_t        = eval( [ 'astra_out_' fn{kk} '.t' ] );
			this_t        = this_t - this_crash;
			rhovol        = eval( [ 'astra_out_' fn{kk} '.rhovol(:,end)' ] ); % assuming rhovol almost constant
			it_crash = iround( this_t, 0 );
			delta_it = 1;% ms % check if OK for every cases/quantities
			it_2     = iround( this_t, delta_it / 1000. );
			eval( [ 'stored.' fn{kk} '.exist = 1;' ] );
			if eval( [ 'isfield( stored.' fn{kk} ', qqs{ii} )' ] )
				back = eval( [ 'stored.' fn{kk} '.' qqs{ii} ] );
			else
				back = eval( [ 'check_compute_needed( astra_out_' fn{kk} ', qqs{ii}, 0, q_val );' ] );
				eval( [ 'stored.' fn{kk} '.' qqs{ii} '=back;' ] );
			end
			if strcmp(study,'recovery') & ~deltaELM_OK
				xplotdiv(kk) = this_deltaELM;
				%it_end   = iround( this_t, this_deltaELM / 1000 );
				xunit = '\Delta t_{ELM}';
			elseif strcmp(study,'crash') & ~tauELM_OK
				xplotdiv(kk) = this_tauELM / 1000;
				%it_end   = iround( this_t, this_tauELM / 1000000 );
				xunit = '\tau_{ELM}';
			else
				xplotdiv(kk) = 1;
			end
			if strcmp(qqs{ii},'ibsped') | strcmp(qqs{ii},'ipl')
				y_plot = back.data;
			else
				irho = iround( rhovol, rhos(jj) );
				y_plot = back.data(irho,:);% Single coordinate: t
				if strcmp(fn{kk},'longELM')
					y0 = y_plot(:,1);% Not the better solution...
				else
				%keyboard
					if ~exist('y00','var')
						y00 = y_plot(:,max(it_crash-100,1));
					end
					y0 = y_plot(:,max(it_crash-100,1));% q(1) not always == q(it-1) (timestep 1us)
				end
				if set_zero == 1
					if y0 ~= y00
						y_plot = y_plot - y0 + y00;
					end
					%title_str2 = [ title_str2 ', ' fn{kk} '=' num2str(y0,'%0.2g') ];
				end
			end
			x_plot{kk}  = this_t .* ( 1000 / xplotdiv(kk) );
			y_plot2{kk} = y_plot(:,:);
		end
		%title_str = [ 't_{crash}=' num2str(this_crash) ];
		if deltaELM_OK
			title_str = [ title_str '\Delta t_{ELM}=' num2str(deltaELM) 'ms, '];
		end
		if tauELM_OK
			title_str = [ title_str '\tau_{ELM}=' num2str(tauELM) 'ms, '];
		end
		if ~strcmp(qqs{ii},'ibsped')
			if isempty(addnote)
				title_str = [ title_str '\rho_V=' num2str(rhos(jj),'%5.3f') ', ' ];
			else
				title_str = [ title_str '\rho=' addnote{jj} ', ' ];
			end
		end
		dispname = strrep(back.name,'grad','\nabla ');
		ylabel_str = [ dispname ' ' back.units ];
		%if set_zero == 1
			%ylabel_str = [ back.name '-(' back.name ')_0 ' back.units ];
			%title_str = [ title_str ', ' back.name '_0 ' back.units ':' title_str2(2:end) ];
		%end
		if length(back.name) > 3 && strcmp(back.name(1:4),'grad')
			ylabel_str = [ '-' ylabel_str ];
		end
		ylabel( ylabel_str );
		if ~strcmp(title_str,'') & title_str(end-1:end) == ', '
			title_str = title_str(1:end-2);
		end
		the_plot = [ x_plot; y_plot2 ];
		%keyboard
		pt = plot( the_plot{:} );
		xlabel( [ 't-t_{crash} [' xunit ']' ] );
		if only_two
			set(pt(2),'linestyle','--');
		else
			if length(pt) == 4
				set(pt(2),'linestyle','--');
				set(pt(3),'linestyle','-.');
				set(pt(4),'linestyle',':','color','k');
			else
				if length(fn_leg) > 1
					legend( fn_leg, 'location',locleg);
				end
			end
		end
		grid( 'on' );
		zoom( 'on' );
		if strcmp(study,'recovery')
			if deltaELM_OK
				xlim( [ -0.21 deltaELM+0.1 ] );% 0.21ms before and 0.1ms after
			else
				xlim( [ -0.01 1.01 ] );
			end
		else
			if tauELM_OK
				xlim( [ -0.021 tauELM+0.021 ] );% 0.21ms before and after
			else
				xlim( [ -0.01 1.01 ] );
			end
		end
		ylim('auto');
		if ~strcmp(addname,'') & save_pics
			set(gcf,'paperpositionmode','auto');
			figname = [ 'pics/' int2str(shot) '_' num2str(t0) '_' strrep(back.name,'R_0 / ','') '_' num2str(rhos(jj),'%5.3f') '_results' addname '_ppm.ps' ];
			print( '-dpsc', figname );
			set(gcf,'paperpositionmode','manual','outerposition',figpos);
			figname = [ 'pics/' int2str(shot) '_' num2str(t0) '_' strrep(back.name,'R_0 / ','') '_' num2str(rhos(jj),'%5.3f') '_results' addname '.ps' ];
			print( '-dpsc', figname );
		end
		clear y_plot y_plot2 y0 y00;
		title( title_str );
	end
	if ~dont_close
		close all;
	end
end

end
