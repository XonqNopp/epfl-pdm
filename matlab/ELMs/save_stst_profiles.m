function save_stst_profiles( filenames_equil, quantities, astra_args, plot_opt )
% save_stst_profiles( filenames_equil, quantities, astra_args, plot_opt )
%   astra_args and plot_opt: see 'help plotAstra'
%   filenames_equil and quantities : see 'help plot_ASTRA_profiles'
%
% Cases to be saved:
%   nodes vs std (only for te, ne, he)
%   std vs nex, Ware
%   std vs X3only
%   std vs TIEB
%

if isfield(quantities,'ibsped')
	quantities = rmfield(quantities,'ibsped');
end
astra_args.rerun = 0;
astra_args.rewrite = 0;
astra_args.store = 1;
astra_args.HD='LD_std';
astra_args.resdir = 'datafiles';

plot_opt.nofig = 1;

%cases{1} = '';
%cases{2} = {'nex','Ware'};
%cases{3} = 'X3only';
%cases{4} = 'TIEB';

%labels{1} = [];
%labels{2} = 'density';
%labels{3} = [];
%labels{4} = [];

cases{1} = 'X3only';
labels{1} = '';

fina=change_fns(filenames_equil,'std');
qq2=struct('te',0,'ne',0,'lte',0,'lne',0);
plot_ASTRA_profiles(fina,qq2,[],1,1);
%plot_opt.q = 'te';
%plot_opt.name = 'T_e [keV]';
%plot_opt.conf = 1e3;
%plotAstra(astra_args,plot_opt);
%plot_opt.q = 'ne';
%plot_opt.name = 'n_e [10^{19} m^{-3}]';
%plot_opt.conf = 1e19;
%plotAstra(astra_args,plot_opt);
plot_opt.q = 'he';
plot_opt.name = '\chi_e [m^2 s^{-1}]';
plot_opt.conf = -1;
plotAstra(astra_args,plot_opt);
close all;

delete_it = 0;
for ii = 1:length(cases)
	fina = change_fns(filenames_equil,{'std',cases{ii}});
	if strcmp( cases{ii}, 'X3only' ) & ~isfield(quantities,'ecrh')
		quantities.ecrh = 0;
		quantities.qe   = 0;
		delete_it = 1;
	end
	plot_ASTRA_profiles(fina,quantities,labels{ii},1);
	if delete_it
		quantities = rmfield( quantities, {'ecrh','qe'} );
		delete_it = 0;
	end
end

end
