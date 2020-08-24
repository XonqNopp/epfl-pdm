function chie_trace(astra_out,rho,tis,tcrash)
% chie_trace(astra_out,rho,tis,tcrash)
%

t = ( astra_out.t - tcrash ) .* 1000;
rhovol = astra_out.rhovol(:,end);
irho = iround(rhovol,rho);
chie = astra_out.he(irho,:);

figure;
set(gca,'fontsize',16);
set(gcf,'name','chie');
plot(t,chie,'linewidth',3);
%hold on;
xlabel('t [ms]');
ylabel(['\chi_e [m^2/s] at \rho_V = ' num2str(rhovol(irho),'%0.2g')]);
%legend(##,'location','best');
xlim(tis);
grid('on');
zoom('on');
print('-dpsc',['pics/40080_0.8_chie_trace.ps']);
set(gcf,'paperpositionmode','auto');
print('-dpsc',['pics/40080_0.8_chie_trace_ppm.ps']);


end
