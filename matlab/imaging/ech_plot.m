function ech_plot(shot,t0)
% ech_plot(shot,t0)
%

mdsopen(shot);
p_tdi = tdi('\results::toray.output_x:pdens');
vol_toray = tdi('\results::toray.output_tot:vol');
%rhovol_tdi = tdi('\results::conf:rhovol');
mdsclose;

it = iround(p_tdi.dim{3},t0);
pdens = p_tdi.data(:,10,it);
vol = vol_toray.data(:,it);
rhovol_toray = sqrt(vol/vol(end));

%it2 = iround(rhovol_tdi.dim{2},t0);
%rhovol = rhovol_tdi.data(:,it2);

figure;
set(gca,'fontsize',16);
set(gcf,'name','ECH profile');
plot(rhovol_toray,pdens);
%hold on;
xlabel('\rho_V');
ylabel('ECH density power [W m^{-3} W^{-1}]');
%legend(##,'location','best');
grid('on');
zoom('on');
print('-dpsc',['pics/' int2str(shot) '_' num2str(t0) '_ECHprofile.ps']);
set(gcf,'paperpositionmode','auto');
print('-dpsc',['pics/' int2str(shot) '_' num2str(t0) '_ECHprofile_ppm.ps']);

end
