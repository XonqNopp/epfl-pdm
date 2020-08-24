function rho = rhoped_scal( shot, t0 )
% rho = rhoped_scal( shot, t0 )
%

mdsopen(shot);
te_tdi=tdi('\results::conf:te');
ne_tdi=tdi('\results::conf:ne');
rhovol_tdi=tdi('\results::conf:rhovol');
volum_tdi=tdi('\results::conf:vol');
mdsclose;

t=rhovol_tdi.dim{2};
it=iround(t,t0);
rhovol=rhovol_tdi.data(:,it);
te=te_tdi.data(:,it);
ne=ne_tdi.data(:,it);
volum=volum_tdi.data(:,it);
pe=ne.*te;
[a1 a2 a3 we] = interpos( volum, pe );
clear a1 a2 a3;
for ii=1:length(rhovol)
	we_ped(ii)=we(ii)/(we(end)-we(ii));
end

figure;
set(gca,'fontsize',18);
plot( rhovol, we_ped );
xlabel( 'position of \rho_{core-edge} on \rho_V' );
ylabel( 'w\_core / w\_ped' );
grid( 'on' );
title(['#' int2str(shot) ', t0=' num2str(t0) ])

irho = iround( we_ped, 3.5 );
hold on;
plot( rhovol(irho), we_ped(irho), 'or', 'markersize', 15 );
plot( [ 0 rhovol(irho) ], [ we_ped(irho) we_ped(irho) ], '-r' );
plot( [ rhovol(irho) rhovol(irho) ], [ 0 we_ped(irho) ], '-r' );
aj=text(0.702, 3.6, ['w\_core / w\_ped = 3.5 @ \rho_V=' num2str(rhovol(irho),'%5.3f')]);
set(aj,'color','red','fontsize',16);
axis([0.7 0.75 2.5 4]);

zoom( 'on' );
print( '-dpsc', [ 'pics/' int2str(shot) '_' num2str(t0) '_wcore_wped_zoom.ps' ] );
set(gcf,'paperpositionmode','auto');
print( '-dpsc', [ 'pics/' int2str(shot) '_' num2str(t0) '_wcore_wped_zoom_ppm.ps' ] );

rho = rhovol( irho );

end
