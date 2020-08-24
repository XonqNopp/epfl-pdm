function save_jalpha(filenames,raargs)
% save_jalpha(filenames,raargs)
%

rhoJ = [0.75 0.79 0.81 0.86];

dir = filenames.dir;
filenames = rmfield(filenames,'dir');
fn = fieldnames(filenames);
for ii = 1:length(fn)
	addname = fn{ii};
	disp( [ '   > Computing case ' addname '...' ] );
	eval(['load([dir ''/'' filenames.' fn{ii} ']);']);
	tELM = eval(['raargs.tELM.' fn{ii}]);
	tauELM = eval(['raargs.tauELM.' fn{ii}]) / 1000;
	deltaELM = eval(['raargs.deltaELM.' fn{ii}]);
	disp( [ '     * jtot' ] );
	jalpha(astra_out,rhoJ,tELM,tauELM,deltaELM,addname,1);
	disp( [ '     * johm' ] );
	johmalpha(astra_out,rhoJ,tELM,tauELM,deltaELM,addname,1);
	disp( [ '     * jbs' ] );
	jbsalpha(astra_out,rhoJ,tELM,tauELM,deltaELM,addname,1);
	close all;
end


end
