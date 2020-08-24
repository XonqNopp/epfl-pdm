function show_twist( astra_out )
% show_twist( astra_out )
%

shot = astra_out.shot;
t0   = astra_out.t0(1);

rhovol = astra_out.rhovol(:,end);
rhopsi = astra_out.rhopsi(:,end);
rhotor = astra_out.rhotor(:,end);
rminor = astra_out.ametr(:,end);
vol = astra_out.volum(:,end);
last = size(vol,1);

%chosen = rhopsi;
chosen = rminor;

figure;
%keyboard
set(gca,'fontsize',36);
set(gcf,'name','volume');
plot(chosen,vol,'x-b','markersize',25,'linewidth',5);
hold on;
plot(chosen(last,1),vol(last,1),'or','markersize',25,'markerfacecolor','r');
%xlim([0.96 1.001]);
xlim([0.21 0.223]);
%ylim([1.2 1.4]);
ylim([1.25 1.4]);
%xlabel('\rho_{\psi}');
xlabel('r');
ylabel('volume');
grid('on');
zoom('on');
aa = get(gca,'xticklabel');
for ii = 1:size(aa,1)
	xtick{ii} = aa(ii,:);
end
xtick{1} = '';
set(gca,'xticklabel',xtick);
print('-dpsc',['pics/weird_spot.ps']);


end
