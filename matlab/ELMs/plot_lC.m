function plot_lC(astra_out,varargin)
% plot_lC(astra_out,varargin)
%    varargin{ 1 } : factor
%

fact = 0;
if size(varargin,2) > 0 && ~isempty(varargin{1})
	fact = varargin{1};
end
R0 = 0.88;
if isstruct(astra_out)
	shot = astra_out.shot;
	t0 = astra_out.t0;
	rhovol = astra_out.rhovol(:,end);
	lne    = astra_out.lne(:,end);
	lte    = astra_out.lte(:,end);
	rlne = R0 ./ lne;
	rlte = R0 ./ lte;
	from = 'ASTRA';
else
	shot = astra_out(1);
	t0 = astra_out(2);
	mdsopen(shot);
	rhovol_tdi = tdi('\results::conf:rhovol');
	rlne_tdi = tdi('\results::conf:r_lne');
	rlte_tdi = tdi('\results::conf:r_lte');
	mdsclose;
	it = iround(rhovol_tdi.dim{2},t0);
	rhovol = rhovol_tdi.data(:,it);
	rlne = rlne_tdi.data(:,it);
	rlte = rlte_tdi.data(:,it);
	from = 'nodes';
end

figure;
set( gca, 'fontsize', 16 );
plot( rhovol, rlne );
hold on;
plot( rhovol, rlte, '--r' );
plot( rhovol, 0.5 .* rlte, '-.r' );
if fact > 0
	plot( rhovol, fact .* rlte, ':r' );
end
hold off;
xlabel( '\rho_V' );
if fact > 0
	legend( 'R/L_n', 'R/L_T', '1/2 R/L_T', [ num2str(fact) ' R/L_T' ], 'Location', 'Best' );
else
	legend( 'R/L_n', 'R/L_T', '1/2 R/L_T', 'Location', 'Best' );
end
grid( 'on' );
xlim( [ 0.8 1.0 ] );
zoom( 'on' );
print( '-dpsc', [ 'pics/' int2str(shot) '_' num2str(t0) '_' from '_RLnT.ps' ] );

figure;
set( gca, 'fontsize', 16 );
plot(rhovol,rlne./rlte);
xlim([0.8 1.0]);
xlabel('\rho_V');
ylabel('R/L_n / R/L_T');
title( [ 'From ' from ] );

end
