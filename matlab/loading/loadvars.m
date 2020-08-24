% Computational constants
ev2j = 1.602e-19;
kev2j = 1.602e-16;
R0 = 0.88;

shot = 40080;
astra_args.shot=shot;
t0 = 0.8;
astra_args.t0=0.8;
astra_args.ECH=1;
astra_args.equ = 8;
astra_args.ntimes = 1;
astra_args.HD = '';
astra_args.rerun = 1;
astra_args.store = 0;
astra_args.rewrite   = 0;
astra_args.TeDivider = 3;
astra_args.resdir = 'datafiles';

%% plot opt %%
plot_opt.conf = 1e3;
plot_opt.q = 'te';
plot_opt.name = 'T_e [keV]';


%%% Some stuff %%%
%shotA = 29892;
%itA = [ 19 27 39 46 ];
%shotF1 = 39874;
%shotF2 = 40307;
%shotF3 = 40308;

qq.te    = 0;
qq.lte   = 0;
qq.ne    = 0;
qq.lne   = 0;
qq.p_e   = 0;
qq.ti    = 0;
qq.itot  = 0;
qq.ibs   = 0;
qq.ibsped = 0.78;
qq.shear = 0;
qq.q     = 0;
qq.upl   = 0;

% For proffit run
zeff=3;
params.shot_type='hmode';
params.zeff=zeff;

tcvviewargs='pvt';

rhos=loadascii('datafiles/rhos_40080_0.8.mat');
rho1=rhos(1);
rho2=rhos(2);
t_i=loadascii('datafiles/ti_40080_0.8.mat');
astradir='datafiles';

todo

fns;
fns_equil;
tcrash;
delta_ELM;
tau_ELM;
raargs.tELM = tELM;
raargs.deltaELM=deltaELM;
raargs.tauELM=tauELM;
raargs.study='recovery';
raargs.rhos = rhos;
raargs.tis = t_i;
raargs.save_pics = 1;
raargs.dont_close = 1;
raargs.filenames=struct();
fina=change_fns(filenames,{'stdNoST'});%,'X3onlyNoST'});
%raargs.filenames=fina;

tST=1.92420421;%it=1051;
