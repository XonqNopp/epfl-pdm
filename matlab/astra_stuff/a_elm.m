function a_elm()
% a_elm()
%

rhovol = [0:0.01:1];
chie = ones(size(rhovol));
Dn = chie;
chieELM = 1e4;
DnELM = 20;
rhoELM = 0.8;
chieOK = chie + heaviside(rhovol-rhoELM) .* chieELM;
DnOK   = Dn   + heaviside(rhovol-rhoELM) .* DnELM;

figure;
set(gca,'fontsize',16);
set(gcf,'name','chie ELM');
plot(rhovol,chieOK,'linewidth',3);
xlabel('\rho_V');
ylabel('\chi_e ELM');
grid('on');
zoom('on');
print('-dpsc',['pics/a_he_ELM.ps']);
set(gcf,'paperpositionmode','auto');
print('-dpsc',['pics/a_he_ELM_ppm.ps']);

figure;
set(gca,'fontsize',16);
set(gcf,'name','Dn ELM');
plot(rhovol,DnOK,'linewidth',3);
xlabel('\rho_V');
ylabel('D_n ELM');
grid('on');
zoom('on');
print('-dpsc',['pics/a_dn_ELM.ps']);
set(gcf,'paperpositionmode','auto');
print('-dpsc',['pics/a_dn_ELM_ppm.ps']);

end
