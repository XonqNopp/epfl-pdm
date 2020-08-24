function redo_pics()
% redo_pics()
%   Reprints all figs for master thesis

shot = 40080;
t0 = 0.8;

tcvview('pvt',shot,t0);
print('-dps',['pics/' int2str(shot) '.ps']);
set(gcf,'paperpositionmode','auto');
print('-dps',['pics/' int2str(shot) '_ppm.ps']);
close all;
% H-mode chie
a_he;
% find where Wcore = 3.5Wped
rhoped_scal(shot,t0);
% H-mode description
Hmode_plot(40346,1.255,5,5);
% ECH profile
ech_plot(shot,t0);

% L- and H-mode comparison
LHcomp;

load('datafiles/40080_0.8_LD_std.mat');
astra_out_std=astra_out;
astra_out_std.name = 'std';
clear astra_out;
load('datafiles/40080_0.8_LD_X3only.mat');
astra_out_X3=astra_out;
astra_out_X3.name = 'X3only';
clear astra_out;
astra_out(1) = astra_out_std;
astra_out(2) = astra_out_X3;

% Dalpha trace
get_Dalpha(shot,0.7847,0.0002);
% Find max of grad pressure
rhoMaxDpe(astra_out);% needs interaction
% Compare Ti to exp results
ax = plotTiNodes(astra_out_std);% needs interaction
hold on;
clear astra_out;
load('datafiles/40080_0.8_LD_TIB2.mat');
rhopsi = astra_out.rhopsi(:,end);
ti = astra_out.ti(:,end) .* 1000;
ax(3) = plot(rhopsi,ti,'-k');
legend(ax,'Fitted profile','ASTRA','TIB=TEB \chi_i=\chi_e','location','northeast');
print('-dpsc',['pics/' int2str(shot) '_' num2str(t0) '_TIEB.ps']);
set(gcf,'paperpositionmode','auto');
print('-dpsc',['pics/' int2str(shot) '_' num2str(t0) '_TIEB_ppm.ps']);

clear astra_out;
load('datafiles/40080_0.8_HD_stdNoST.mat');
chie_trace(astra_out,0.85,[-0.1 0.3],2.201);
close all;
do_save_all;% background run

end
