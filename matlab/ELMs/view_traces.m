function view_traces( results_args, varargin )
% view_traces( results_args, varargin )
%   results_args is a structure with the following fields
%        (starred field are necessary)
%     astradir (*)   : directory where the files are
%     quantities (*) : quantities of interest (structure)
%                      if value == 1, will set y_pre = 0, if 0 not, if neither used as rhoped
%     tis (*)        : times of interest (can be empty if rhos provided)
%     rhos0 (*)      : rhos of interest (can be empty)
%     dont_close     : 1 to keep the figures opened
%     addname        : additional name for savings
%     addnote        : additional note for legend (cell the size of rhos0)
%     save_pics      : 1 to save the pictures (default)
%     nonew_rhos     : 1 if you don't want to choose new rhos
%     ??? (*)        : struct with fn (filename), t (ELM crash), deltaELM (ELM interval [ms]), tauELM (ELM duration [us])
%
%     varargin{ 1 } : put 1 if you want this to happen in the background
%

nofig = 0;
if size(varargin,2) > 0 && ~isempty(varargin{1})
	nofig = varargin{1};
end

required_fields={'astradir','quantities','tis','rhos0'};
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
optional_fields.addname = 'cycle';
optional_fields.nonew_rhos = 0;
optional_fields.save_pics = 1;
optional_fields.addnote = {};
optional_fields.study = '';
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
ff = fieldnames(results_args);
if length(ff) > 1
	error( ' Can only treat one case...' );
end
this_std = eval(['results_args.' ff{1}]);
deltaELM = this_std.deltaELM;
tauELM   = this_std.tauELM;
% Loading astra_out
disp( [ '  * Loading ' fullfile(astradir,this_std.fn) '...' ] );
load(fullfile(astradir,this_std.fn));

% Storing rhos of interest
disp( ' Storing rhos of interest...' );
if isempty( rhos0 )
	rhos = choose_rhos(astra_out,quantities,t_i,this_std.t,deltaELM,'');
else
	if nonew_rhos
		rhos = rhos0;
	else
		answer = input( ' rhos provided, do you want to rechoose them anyway ? [1/0] : ' );
		if answer
			rhos = choose_rhos(astra_out,quantities,t_i,this_std.t,deltaELM,'',rhos0);
		else
			rhos = rhos0;
		end
	end
end
if size(rhos,1) < size(rhos,2)
	rhos = rhos';
end
clear t_i; % no more needed variables
shot = astra_out.shot;
t0   = astra_out.t0(1);
disp( [ '    shot #' int2str(shot) ] );
disp( [ '    t0 = ' num2str(t0) ] );

if save_pics
	filename_rhos = [ 'datafiles/rhos_' int2str(shot) '_' num2str(t0) '.mat' ];
	fprintf( [ '  Saving rhos in ASCII in ' filename_rhos '...' ] );
	save( filename_rhos, 'rhos', '-ascii' );
	fprintf( '  Done !\n' );
end

qqs = fieldnames( quantities );
% Make one figure for each quantity
figpos = [ 3 132 1265 820 ];
for ii = 1:length(qqs)
	q_val = eval( [ 'quantities.' qqs{ii} ] );
	if q_val == 0 | q_val == 1
		set_zero = q_val;
	else
		set_zero = 0;
	end
	figure('name',qqs{ii});
	if nofig > 0
		set(gcf,'visible','off');
	end
	set(gca,'fontsize',16);
	hold on;
	title_str = '';
	title_str2 = '';
	this_crash = this_std.t;
	stored.exist = 1;
	this_t = astra_out.t;
	rhovol = astra_out.rhovol(:,end); % assuming rhovol almost constant
	it_crash = iround( this_t, this_crash );
	delta_it = 1;% ms % check if OK for every cases/quantities
	it_2     = iround( this_t, this_crash + delta_it / 1000. );
	it_ELM   = iround( this_t, this_crash + deltaELM / 1000 );
	if isfield( stored, qqs{ii} )
		back = eval( [ 'stored.' qqs{ii} ] );
	else
		back = check_compute_needed( astra_out, qqs{ii}, 0, q_val );
		eval( [ 'stored.' qqs{ii} '=back;' ] );
	end
	fn_leg = {};
	if strcmp(qqs{ii},'ibsped') | strcmp(qqs{ii},'ipl')
		y_plot(1,:) = back.data;
	else
		for jj = 1:length(rhos)
			fn_leg{jj} = [ '\rho_V=' num2str(rhos(jj),'%0.2g') ];
			if ~isempty(addnote) && length(addnote) == length(rhos) && ~strcmp(addnote{jj},'')
				fn_leg{jj} = [ fn_leg{jj} ' ' addnote{jj} ];
			end
			irho = iround( rhovol, rhos(jj) );
			y_plot(jj,:) = back.data(irho,:);% Single coordinate: t
			y0(jj) = y_plot(jj,max(it_crash-100,1));% qq(1) not always == qq(it-1) (timestep 1us)
			if set_zero == 1
				y_plot(jj,:) = y_plot(jj,:) - y0(jj);
				title_str2 = [ title_str2 ', ' num2str(rhos(jj),'%0.2g') '=' num2str(y0(jj),'%0.2g') ];
			end
		end
	end
	title_str = [ '\Delta t_{ELM}=' num2str(deltaELM) 'ms, \tau_{ELM}=' num2str(tauELM) '\mu s' ];
	if set_zero == 1
		ylabel( [ back.name '-' back.name '_0 ' back.units ] );
		title_str = [ title_str ', ' back.name '_0 ' back.units ':' title_str2(2:end) ];
	else
		ylabel( [ back.name ' ' back.units ] );
	end
	pp = plot( ( this_t - this_crash ) .* 1000, y_plot );
	if length(pp)>1
		set(pp(2),'linestyle','--');
	end
	if length(pp)>2
		%set(pp(2),'linestyle','--','color','r');
		set(pp(3),'linestyle','-.','color','k');
	end
	xlabel( 't-t_{crash} [ms]' );
	%if ~isempty(fn_leg)
		%legend( fn_leg, 'location',locleg);
	%end
	grid( 'on' );
	zoom( 'on' );
	xlim( [ -0.21 deltaELM+0.1 ] );% 0.21ms before and 0.1ms after
	ylim('auto');
	qname = strrep(back.name,'R_0 / ','');
	figname = [ 'pics/' int2str(shot) '_' num2str(t0) '_' qname '_results' addname '.ps' ];
	if save_pics
		set(gcf,'paperpositionmode','auto');
		figname = [ 'pics/' int2str(shot) '_' num2str(t0) '_' qname '_results' addname '_ppm.ps' ];
		print( '-dpsc', figname );
		set(gcf,'paperpositionmode','manual','outerposition',figpos);
		figname = [ 'pics/' int2str(shot) '_' num2str(t0) '_' qname '_results' addname '.ps' ];
		print( '-dpsc', figname );
	end
	title( title_str );
	if ~dont_close
		close all;
	end
	clear y_plot fn_leg;
end

end
