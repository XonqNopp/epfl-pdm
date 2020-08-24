function plot_VnDn(astra_out)
% plot_VnDn(astra_out)
%

shot = astra_out.shot;
t0   = astra_out.t0;
R0 = 0.88;
rhovol = astra_out.rhovol(:,end);
vn = astra_out.c(:,end);
dn = astra_out.dn(:,end);
vndn = vn ./ dn;
te = astra_out.te(:,end);
[ aa dte ] = interpos(rhovol,te);
gradro = astra_out.gradro(:,end);
rlt = R0 .* dte ./ te .* gradro;

figure;
set( gca, 'fontsize', 16 );
plot( rhovol, vndn );
hold on;
plot( rhovol, rlt, '--r' );
xlabel( '\rho_V' );
grid( 'on' );
xlim([0.8 1.0]);
axis('ij');
legend( 'V_n / D_n', 'R / L_T', 'Location', 'Best' );
zoom( 'on' );
print( '-dpsc', [ 'pics/' int2str(shot) '_' num2str(t0) '_VnDn.ps' ] );


end
