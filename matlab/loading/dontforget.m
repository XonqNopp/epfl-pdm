error( ' The purpose of this file is not to be executed but to be looked at...' );

runAstra(shot,t0,1,basic_info,astra_run,exp_write);
plotAstra(shot,t0,1,'ne',basic_info,astra_run,exp_write,plot_opt);
astra_run.store=1;basic_info.HD='_HD';
astra_run.store=0;basic_info.HD='';
plotCH(astra_out,qq,its,1.601,5);

% vim: nonumber:
