function save_traces_ELM( filenames, raargs, varargin )
% save_traces_ELM( filenames, raargs, varargin )
%   raargs: see help resultsargs
%   varargin{ 1 } : choice for single case (struct with required fields simple and special)

raargs.dont_close = 0;
raargs.save_pics  = 1;
raargs.nonew_rhos = 1;
raargs.study      = 'crash';
raargs.addname    = '';

if size(varargin,2)>0 && ~isempty(varargin{1})
	cases = varargin{1};
else
	cases.simple = {'ELM2NoST'};
	cases.special{1}.data = {'stdELMNoST','D01ELMNoST','D2ELMNoST','D10ELMNoST'};
	cases.special{1}.name = 'DELMNoST';
	cases.special{2}.data = {'stdELMNoST','tau2ELMNoST','tau10ELMNoST'};
	cases.special{2}.name = 'tauELMNoST';
	%cases.special{3}.data = {'stdELMNoST','D2ELMNoST','D10ELMNoST','ELM2NoST','tau2ELMNoST','tau2ELMNoST','tau10ELMNoST'};
	cases.special{3}.data = {'stdELMNoST','D2ELMNoST','D10ELMNoST','tau2ELMNoST','tau2ELMNoST','tau10ELMNoST'};
	cases.special{3}.name = 'DVStauNoST';
end

for ii = 1:length(cases.simple)
	fprintf( [ '\n   > Running case ' cases.simple{ii} '...\n' ] );
	fina = change_fns( filenames, {'stdELMNoST',cases.simple{ii}} );
	raargs.filenames = fina;
	ra = resultsargs(raargs);
	view_many_traces(ra,1);
	pause(0.001);
end
for ii = 1:length(cases.special)
	fprintf( [ '\n   > Running case ' cases.special{ii}.name '...\n' ] );
	fina = change_fns( filenames, cases.special{ii}.data );
	raargs.filenames = fina;
	ra = resultsargs(raargs);
	ra.addname = cases.special{ii}.name;
	view_many_traces(ra,1);
	pause(0.001);
end

end

