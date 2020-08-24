function do_save_all()
% do save_all (yes, simply)

plot_opt = struct();
astra_args = struct( 'shot', 40080, 't0', 0.8, 'ECH', 1, 'equ', 8, 'TeDivider', 3, 'resdir', 'datafiles', 'ntimes', 1 );
reloadTs;
raargs.study = 'recovery';
raargs.rhos = loadascii('datafiles/rhos_40080_0.8.mat');
raargs.tis  = loadascii('datafiles/ti_40080_0.8.mat');
raargs.save_pics = 1;
raargs.dont_close = 0;

save_all(raargs,astra_args,plot_opt);
end
