function out = all_out(filenames)
% out = all_out(filenames)
%

dir = filenames.dir;
filenames = rmfield(filenames,'dir');
fn = fieldnames(filenames);
for ii = 1:length(fn)
	fnii = eval(['filenames.' fn{ii}]);
	if ~strcmp(fnii,'')
		thi = fullfile(dir,fnii);
		disp(['Loading ' fn{ii} '...' ]);
		load(thi);
		eval(['out.' fn{ii} ' = astra_out;']);
		clear astra_out;
	end
end

end
