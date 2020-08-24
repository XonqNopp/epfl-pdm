function save_ELM_profiles(filenames,raargs,varargin)
% save_ELM_profiles(filenames,raargs,varargin)
%

raargs.dont_close = 0;
raargs.save_pics  = 1;
raargs.nonew_rhos = 1;

%if size(varargin,2)>0 && ~isempty(varargin{1})
	%cases = varargin{1};
%else
	%%cases.special{1}.data = {'std','Dn10', 'Dn01'};
	%%cases.special{1}.name = 'Dn';
	%%cases.special{3}.data = {'std','smallELM', 'wideELM'};
	%%cases.special{3}.name = 'rhoELM';
	%%cases.special{2}.data = {'Dn01','Dn01first'};
	%%cases.special{2}.name = 'Dn01evolution';
	%%cases.special{3}.data = {'VnDn','VnDnCore0'};
	%%cases.special{3}.name = 'Core0';
	cases.special = {};
%end

not_used = { 'nex','Ware','neTeELM','TIEB','VnDn','VnDnCore0'};
for ii = 1:length(not_used)
	if isfield(filenames,not_used{ii})
		filenames = rmfield(filenames,not_used{ii});
	end
end

the_dir = filenames.dir;
filenames = rmfield(filenames,'dir');
fn = fieldnames(filenames);
filenames.dir = the_dir;
for ii = 1:length(fn)
	if ~strcmp(eval(['filenames.' fn{ii}]),'')
		fprintf( [ '\n   > Running case ' fn{ii} '...\n' ] );
		fina = change_fns( filenames, fn{ii} );
		raargs.filenames = fina;
		ra = resultsargs(raargs);
		ra.addname = fn{ii};
		if ~isempty(strfind(fn{ii},'delta'))
			%ra.tis = [ ra.tis(1:end-1); 0.0099 ];
			ra.tis = [ra.tis(1:5); 0.0007; ra.tis(7:end) ./ 2];
		end
		plot_rhoOK(ra,1);
		if strcmp(fn{ii},'width2NoST')
			disp('Check another way to implement this...');
			ra.dont_close = 1;
			ra.quantities = struct('te',0,'ne',0);
			close all;
			plot_rhoOK(ra);
			figure(1);
			title('');
			axis([0.6 1 0.2 1.4]);
			print('-dpsc','pics/40080_0.8_te_rhosOK_width2NoST_zoom.ps');
			set(gcf,'paperpositionmode','auto','outerposition',[354 498 572 508]);
			print('-dpsc','pics/40080_0.8_te_rhosOK_width2NoST_zoom_ppm.ps');
			figure(2);
			title('');
			axis([0.6 1 2.5 4.5]);
			print('-dpsc','pics/40080_0.8_ne_rhosOK_width2NoST_zoom.ps');
			set(gcf,'paperpositionmode','auto','outerposition',[354 498 572 508]);
			print('-dpsc','pics/40080_0.8_ne_rhosOK_width2NoST_zoom_ppm.ps');
		end
		pause(0.001);
	end
end

for ii = 1:length(cases.special)
	fprintf( [ '\n   > Running case ' cases.special{ii}.name '...\n' ] );
	fina = change_fns( filenames, cases.special{ii}.data );
	raargs.filenames = fina;
	ra = resultsargs(raargs);
	ra.addname = cases.special{ii}.name;
	plot_rhoOK(ra);
	pause(0.001);
end

end
