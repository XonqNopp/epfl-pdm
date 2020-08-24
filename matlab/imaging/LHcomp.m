function LHcomp()
% LHcomp()
%

mdsopen(39319);
rhovolL = tdi('\results::conf:rhovol');
neL = tdi('\results::conf:ne');
teL = tdi('\results::conf:te');
mdsclose;
mdsopen(40346);
rhovolH = tdi('\results::conf:rhovol');
neH = tdi('\results::conf:ne');
teH = tdi('\results::conf:te');
mdsclose;
i1 = iround(rhovolL.dim{2},0.4);
i2 = iround(rhovolH.dim{2},1.255);
figure;
set(gca,'fontsize',16);
plot(rhovolL.data(:,i1),neL.data(:,i1));
hold on;
plot(rhovolH.data(:,i2),neH.data(:,i2),'--r');
xlabel('\rho_V');
ylabel(neH.units);
grid('on');
zoom('on');
print('-dpsc',['pics/LH_ne.ps']);
set(gcf,'paperpositionmode','auto');
print('-dpsc',['pics/LH_ne_ppm.ps']);
figure;
set(gca,'fontsize',16);
plot(rhovolL.data(:,i1),teL.data(:,i1));
hold on;
plot(rhovolH.data(:,i2),teH.data(:,i2),'--r');
xlabel('\rho_V');
ylabel(teH.units);
grid('on');
zoom('on');
print('-dpsc',['pics/LH_te.ps']);
set(gcf,'paperpositionmode','auto');
print('-dpsc',['pics/LH_te_ppm.ps']);

end
