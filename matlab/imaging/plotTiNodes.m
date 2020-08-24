function ax = plotTiNodes(astra_out,varargin)
% ax = plotTiNodes(astra_out,varargin)
%    varargin{ 1 } : addname for picture
%

addname = '';
if size(varargin,2)>0 && ~isempty(varargin{1})
	addname = ['_' varargin{1}];
end

shot = astra_out.shot;
t0   = astra_out.t0(1);
pp.dY_max = 100;% Must change this to allow user choice

lincol = '-b';

oo=Ti2EXP(shot,t0,pp);
ax(1)=oo.ax;
set(findall(gca),'color','r');
set(gca,'color','w');
rhopsi=astra_out.rhopsi(:,end);
ti=astra_out.ti(:,end).*1000;% keV -> eV
ax(2)=plot(rhopsi,ti,lincol);
hold off;
legend(ax,'Fitted profile','ASTRA','location','northeast');
set(gca,'fontsize',16);
xlabel('\rho_{\psi}','color','k');
ylabel('T_i [eV]','color','k');
yl = ylim;
%ylim([0 yl(2)]);
ylim([0 1600]);
xlim([0 1.3]);
title(['CXRS results for TCV shot ' int2str(shot) ', t=0.75-0.8s'],'color','k');% Watch the time interval
print('-dpsc',['pics/' int2str(shot) '_' num2str(t0) '_ti_nodesVSsim' addname '.ps']);
set(gcf,'paperpositionmode','auto');
print('-dpsc',['pics/' int2str(shot) '_' num2str(t0) '_ti_nodesVSsim' addname '_ppm.ps']);
set(gcf,'paperpositionmode','manual');

end
