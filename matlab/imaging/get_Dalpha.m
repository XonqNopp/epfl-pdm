function get_Dalpha(shot,t0,plusminus)
% get_Dalpha(shot,t0,plusminus)
%

mdsopen(shot);
Da_tdi = tdi('\base::pd:pd_001');
mdsclose;

figure;
set( gca, 'fontsize', 16 );
plot( Da_tdi.dim{1}, Da_tdi.data );
xlabel( 't [s]' );
ylabel( 'D_{\alpha} [a.u.]' );
xlim([t0-plusminus t0+plusminus]);
grid( 'on' );
zoom( 'on' );
title(['#' int2str(shot)]);
xt=get(gca,'xtick');
xtl=get(gca,'xticklabel');
yt=get(gca,'ytick');
ytl=get(gca,'yticklabel');
set(gca,'xtick',xt);
set(gca,'xticklabel',xtl);
%set(gca,'ytick',yt);
%set(gca,'yticklabel',ytl);
print( '-dpsc', [ 'pics/' int2str(shot) '_Da.ps' ] );
set(gcf,'paperpositionmode','auto');
print( '-dpsc', [ 'pics/' int2str(shot) '_Da_ppm.ps' ] );

end
