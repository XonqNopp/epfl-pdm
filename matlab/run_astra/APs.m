function APs( shot, t0, ECH, quantities, basic_info, astra_run, exp_write, plot_opt )
% APs( shot, t0, ECH, quantities, basic_info, astra_run, exp_write, plot_opt )
%   same as plotAstra but for many quantities...
%
%     shot
%     t0
%     ECH : 0 for ohmic shot, 1 for ECH
%     quantities : what you want to plot (CELL)
%   The following variable has to be a structure and MUST have the (*) fields:
%     basic_info : chietcv (*) : 0 to have TeExp, 1 to have TE:EQ, 2 for twin shot
%                                8 for H-mode
%                  ntimes (*)  : number of times to start the run
%                  HD          : suffix for model file for HD run
%   The 3 following variables have to be structure and can have the following fields:
%     astra_run : rerun     : 1 to force rerun - HERE FORCED TO 0
%                 store     : 1 to store the results (default) - HERE FORCED TO 1
%     exp_write : rewrite   : 1 to force rewrite the EXP file - HERE FORCED TO 0
%                 t_offset  : t_offset
%                 TeDivider : initial divider for the H-mode model file
%                 mix       : structure for twin shot run
%                             struct( shot, what, ntimes, t0, zeff )
%     plot_opt  : name      : label for plot (default is the quantity name)
%                 linespace
%                 conf      : non-0 to have the plot from the conf nodes too
%                             the value is used to scale conf to ASTRA. Ex for ne: 1e19
%                 rho_t     : 0 to plot on rho (default), 1 to plot on time
%                 which_t   : time index of the simulation (default: 'end')
%                             if numel=2, range of time. For X:end, put which_t=[X Inf].
%                 which_rho : string of rho index (default is 1(center), not used if rho_t==0)
%                 double    : 1 to have the same graph displayed 2 times (not with conf)
% 
% standard use: astra_out=plotAstra(shot,t0,ECH,quantities,basic_info,astra_run,exp_write,plot_opt);
astra_run.rerun = 0;
astra_run.store = 1;
exp_write.rewrite = 0;
for ii = 1:length(quantities)
	plotAstra(shot,t0,ECH,quantities{ii},basic_info,astra_run,exp_write,plot_opt);
end

end
