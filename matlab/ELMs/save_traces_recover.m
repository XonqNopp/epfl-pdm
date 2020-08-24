function save_traces_recover( filenames, raargs, varargin )
% save_traces_recover( filenames, raargs, varargin )
%   raargs: see help resultsargs
%   varargin{ 1 } : choice for single case (struct with required fields simple, double and special)
%

raargs.dont_close = 0;
raargs.save_pics  = 1;
raargs.nonew_rhos = 1;
raargs.study      = 'recovery';
raargs.addname    = '';
ST = 'NoST';
q_val = 0;

if size(varargin,2)>0 && ~isempty(varargin{1})
	cases = varargin{1};
else
	cases.simple = { 'stdNoSTfirst', 'X3onlyNoST', 'width2NoST', 'delta05NoST' };
	cases.special{1}.data = {'stdNoST','Dn10NoST', 'Dn05NoST', 'Dn01NoST'};
	cases.special{1}.name = 'DnNoST';
	%cases.special{3}.data = {'std','smallELM', 'wideELM'};
	%cases.special{3}.name = 'rhoELM';
	cases.special{2}.data = {'Dn01NoST','Dn01NoSTfirst'};
	cases.special{2}.name = '';
	cases.special{3}.data = {'Dn10NoST','Dn10NoSTfirst'};
	cases.special{3}.name = '';
	cases.special{4}.data = {'Dn05NoST','Dn05NoSTfirst'};
	cases.special{4}.name = '';
	cases.special{5}.data = {'width2NoST','width2NoSTfirst'};
	cases.special{5}.name = '';
	cases.special{6}.data = {'stdNoST','Dn05NoST','Dn01NoST','delta05NoST'};
	cases.special{6}.name = 'DnVSdeltaNoST';
	%cases.simple = {};
	%cases.special{1}.data = {'stdNoST','Dn05NoST','Dn01NoST','delta05NoST'};
	%cases.special{1}.name = 'DnVSdeltaNoST';
end

% Running ELM cycle without another than std
fprintf( [ '\n   > Running case cycle...\n' ] );
fina = change_fns( filenames, {'stdNoST'} );
raargs.filenames = fina;
ra = resultsargs(raargs);
ra.addname = 'cycle';
qq_tmp = rmfield(ra.quantities,{'te','lte'});
rhos = ra.rhos0;
ra.rhos0 = [rhos;0.8];
ra.quantities = struct('te',q_val,'lte',q_val);
view_traces(ra,1);
ra.rhos0 = rhos;
ra.quantities = qq_tmp;
view_traces(ra,1);
pause(0.001);
ra = rmfield(ra,'addname');

%disp(' Cycle finished');
%keyboard

for ii = 1:length(cases.simple)
	fprintf( [ '\n   > Running case ' cases.simple{ii} '...\n' ] );
	fina = change_fns( filenames, {'stdNoST',cases.simple{ii}} );
	raargs.filenames = fina;
	if strcmp(cases.simple{ii},'X3onlyNoST')% | strcmp(cases.simple{ii},'width2NoST')
		q_val = 1;
	else
		q_val = 0;
	end
	ra = resultsargs(raargs,q_val);
	view_many_traces(ra,1);
	close all;
	pause(0.001);
end
for ii = 1:length(cases.special)
	if isfield(cases.special{ii},'name')
		casename = cases.special{ii}.name;
	else
		if iscell(cases.special{ii}.data)
			casename = cases.special{ii}.data{1};
		else
			casename = cases.special{ii}.data;
		end
	end
	fprintf( [ '\n   > Running case ' casename '...\n' ] );
	fina = change_fns( filenames, cases.special{ii}.data );
	raargs.filenames = fina;
	%if ~isempty(strfind(cases.special{ii}.data{1},'Dn')) | ~isempty(strfind(cases.special{ii}.data{1},'width')
		%q_val = 1;
	%else
		q_val = 0;
	%end
	ra = resultsargs(raargs,q_val);
	ra.addname = cases.special{ii}.name;
	%if strcmp(cases.special{ii}.name,'DnVSdeltaNoST')
		%qqs = rmfield(ra.quantities,{'ne','lne'});
		%ra.quantities = struct('ne',q_val,'lne',q_val);
		%view_many_traces(ra,1);
		%fina = change_fns( filenames, {'stdNoST','delta05NoST'} );
		%raargs.filenames = fina;
		%ra = resultsargs(raargs,q_val);
		%ra.quantities=qqs;
		%view_many_traces(ra,1);
	%else
		view_many_traces(ra,1);
		close all;
	%end
	pause(0.001);
end

end
