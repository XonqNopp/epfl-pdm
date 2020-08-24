function plot_rhoOK(results_args,varargin)
% plot_rhoOK(results_args,varargin)
%   results_args is a structure with the following fields
%        (starred field are necessary)
%     astradir (*)   : directory where the files are
%     quantities (*) : quantities of interest (structure)
%                      if value == 1, will set y_pre = 0, if 0 not, if neither used as rhoped
%     tis (*)        : times of interest
%     rhos0 (*)      : rhos of interest (can be empty)
%     study (*)      : which case you are studying: crash/recovery
%     dont_close     : 1 to keep the figures opened
%     addname        : additional name for savings
%     save_pics      : 1 to save the pictures (default)
%  All other fields are used to fetch data and must be a structure with
%   the fields fn (filename), t (ELM time), deltaELM (ELM interval [ms]), tauELM (ELM duration [us])
%
%      varargin{ 1 } : put 1 if you want this to happen in the background
%

nofig = 0;
if size(varargin,2) > 0 && ~isempty(varargin{1})
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

fn = fieldnames( results_args );
for ii = 1:length(fn)
	if eval( [ '~isfield(results_args.' fn{ii} ',''fn'') | ~isfield(results_args.' fn{ii} ',''t'') | ~isfield(results_args.' fn{ii} ', ''deltaELM'' ) | ~isfield(results_args.' fn{ii} ', ''tauELM'' )' ] )
		error( [ ' Missing field for case ' fn{ii} ] );
	end
	this_fn = eval( [ 'results_args.' fn{ii} '.fn' ] );
	disp( [ '  * Loading ' fullfile(astradir,this_fn) '...' ] );
	load(fullfile(astradir,this_fn));
	eval(['astra_out_' fn{ii} ' = astra_out;']);
	clear astra_out;
	eval(['astra_out_global(ii) = astra_out_' fn{ii} ';']);
	astra_out_global(ii).name = regexprep( strrep( fn{ii}, '_', '\_' ), '(No)?ST', '' );
	astra_out_global(ii).this_t_crash = eval(['results_args.' fn{ii} '.t']);
	astra_out_global(ii).this_deltaELM = eval(['results_args.' fn{ii} '.deltaELM']);
	astra_out_global(ii).this_tauELM   = eval(['results_args.' fn{ii} '.tauELM']);
end
astra_out = astra_out_global;
clear astra_out_global;
if strcmp( addname, '' )
	if length(fn) == 1
		addname = [ '_' fn{1} ];
	else
		if save_pics
			error( ' addname field not provided, cannot save without overwriting.' );
		end
	end
end

figpos = [ 3 132 1265 820 ];

% Check if there is ibsped and remove it (no profile)
if isfield(quantities,'ibsped')
	quantities = rmfield(quantities,'ibsped');
	if ~isfield(quantities,'jbs')
		quantities.ibs = 0;
	end
end

qqs = fieldnames(quantities);
for ii = 1:length(qqs)
	shot = astra_out(1).shot;
	t0   = astra_out(1).t0(1);
	toplot = [];
	leg_str = {};
	deltaELM = 0;
	deltaELM_OK = 0;
	tauELM = 0;
	tauELM_OK = 0;
	figure('name',qqs{ii});
	if nofig > 0
		set(gcf,'visible','off');
	end
	set(gca,'fontsize',16);
	hold on;
	for jj = 1:length(astra_out)
		this_crash = astra_out(jj).this_t_crash;
		this_deltaELM = astra_out(jj).this_deltaELM;
		this_tauELM = astra_out(jj).this_tauELM;
		if deltaELM == 0
			deltaELM = this_deltaELM;
			deltaELM_OK = 1;
		elseif deltaELM_OK & deltaELM ~= this_deltaELM
			deltaELM_OK = 0;
		end
		if tauELM == 0
			tauELM = this_tauELM;
			tauELM_OK = 1;
		elseif tauELM_OK & tauELM ~= this_tauELM
			tauELM_OK = 0;
		end
		t = astra_out(jj).t;
		rhovol = astra_out(jj).rhovol(:,end);
		ai = find( max(t) < this_crash+t_i );
		if ~isempty(ai)
			its = iround( t, this_crash+t_i(1:ai(1)-1) );
			leg_len = ai(1)-1;
		else
			its = iround( t, this_crash+t_i );
			leg_len = length(t_i);
		end
		back = check_compute_needed(astra_out(jj),qqs{ii});
		qq = back.data(:,its);
		prev_size = size( toplot, 2 );
		for ijk = 1:size(qq,2)
			toplot(:,prev_size+ijk) = qq(:,ijk);
		end
		p_s = length(leg_str);
		for lo = 1:leg_len
			leg_str{p_s+lo} = [ 't=' num2str(t_i(lo)*1000) 'ms' ];
			if max(size(astra_out))>1 & isfield(astra_out(jj),'name') && ~strcmp(astra_out(jj).name,'')
				leg_str{p_s+lo} = [ leg_str{p_s+lo} ' (' astra_out(jj).name ')' ];
			end
		end
	end
	pp=plot(rhovol,toplot);
	set(pp(end),'linestyle','--');
	xlabel( '\rho_V' );
	ylabel( [ strrep(back.name,'grad','-\nabla ') ' ' back.units ] );
	title_str = '';
	if deltaELM_OK
		title_str = [ '\Delta t_{ELM} = ' num2str(deltaELM) 'ms' ];
	end
	if tauELM_OK
		if ~strcmp(title_str,'')
			title_str = [ title_str ', ' ];
		end
		title_str = [ title_str '\tau_{ELM} = ' num2str(tauELM) '\mu s' ];
	end
	legend( leg_str, 'location', 'eastoutside' );% 1us resolution
	grid( 'off' );
	set(gca,'ygrid','on');
	y_range = ylim;
	for kk = 1:numel(rhos0)
		plot([rhos0(kk) rhos0(kk)],y_range,':k');
	end
	ylim(y_range);
	zoom( 'on' );
	xlabs(1,:) = '0  ';
	xlabs(2,:) = '0.2';
	xlabs(3,:) = '0.4';
	xlabs(4,:) = '0.6';
	xlabs(5,:) = '0.8';
	xlabs(6,:) = '1  ';
	set(gca,'xtick',[0 0.2 0.4 0.6 0.8 1],'xticklabel',xlabs);
	hold off;
	if save_pics
		set(gcf,'paperpositionmode','auto');
		print( '-dpsc', [ 'pics/' int2str(shot) '_' num2str(t0) '_' qqs{ii} '_rhosOK' addname '_ppm.ps' ] );
		set(gcf,'paperpositionmode','manual','outerposition',figpos);
		print( '-dpsc', [ 'pics/' int2str(shot) '_' num2str(t0) '_' qqs{ii} '_rhosOK' addname '.ps' ] );
	end
	if ~strcmp(title_str,'')
		title( title_str );
	end
	if ~dont_close
		close all;
	end
end

end
