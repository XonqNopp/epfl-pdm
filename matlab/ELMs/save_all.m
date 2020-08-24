function save_all(raargs,astra_args,plot_opt)
% save_all :
%    save_stst_profiles
%    save_ELM_profiles
%    save_traces_recover
%%    save_traces_ELM
%    save_jalpha
%

fns;
fns_equil;
rhos = loadascii('datafiles/rhos_40080_0.8.mat');
t_i  = loadascii('datafiles/ti_40080_0.8.mat');
if isfield(raargs,'quantities')
	quantities = raargs.quantities;
else
	if isfield(raargs,'filename')
		ra = resultsargs(raargs);
	else
		raa2 = raargs;
		raa2.filenames=struct('dir','');
		ra=resultsargs(raa2);
		clear raa2;
	end
	quantities = ra.quantities;
	clear ra;
end

disp('    # Saving steady profiles...');
save_stst_profiles(filenames_equil,quantities,astra_args,plot_opt);

disp('    # Saving ELM profiles...');
save_ELM_profiles(filenames,raargs);

disp('    # Saving ELM time traces...');
save_traces_recover(filenames,raargs);
%save_traces_ELM(filenames,raargs);

disp('    # Saving ELM j-alpha diagrams...');
save_jalpha(filenames,raargs);

end
