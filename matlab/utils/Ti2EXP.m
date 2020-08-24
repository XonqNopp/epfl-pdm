function output = Ti2EXP(shot,t0,params)
% output = Ti2EXP(shot,t0,params)
%

lincol = '-r';
% P.dY_max = 35 % #40103
% P.dY_max = 50 % #40080

t0 = round( t0 * 10 ) / 10;
rho_psi_200 = linspace(0,1,200);
rho_psi_51 = linspace(0,1,51);
O=CXRS_read_results(shot);
ts = O.result.Ti_times;
ts = ts - 0.025;
ts = round( ts .* 1000 ) ./ 1000;
for ij = length(ts):-1:1
	ts(2*ij)   = ts(ij) + 0.05;
	ts(2*ij-1) = ts(ij);
end
wanted_ts = ts( find( abs( ts - t0 ) < 0.2 ) );
G=CXRS_interp_results(O,'whatplot','Ti','timeplot',wanted_ts,'param',params);
axis auto;
if length(G.prof)>1
	itime = input( ' Choose which time index is the most accurate : ' );%(0 to average all) : ' );
	%if itime == 0
		%rho_psi = G.prof{floor(length(G.prof))}.X
		%ti_tmp = [];
		%for ii = 1:length(G.prof)
			%if isempty( ti_tmp )
				%ti_tmp = G.prof{ii}.Y;
			%else
				%ti_tmp = [ ti_tmp; G.prof{ii}.Y ];
			%end
		%end
		%ti_cxrs = mean( ti_tmp, 1 );
	%else
		%rho_psi = G.prof{itime}.X;
		%ti_cxrs = G.prof{itime}.Y;
		wanted_ts = [ wanted_ts( 2*itime-1 ) wanted_ts(2*itime) ];
		G=CXRS_interp_results(O,'whatplot','Ti','timeplot',wanted_ts,'param',params);
		axis auto;
		%legend off;
		%hold on;
		%title( [ 'CXRS results for TCV shot ' int2str(shot) ', t=' num2str(wanted_ts(1)) '-' num2str(wanted_ts(2)) 's' ] );
		%rho_psi = G.prof{1}.X;
		%ti_cxrs = G.prof{1}.Y;
		%ax=plot(rho_psi,ti_cxrs,lincol);
		%output.ax=ax;
	%end
else
	error( [ ' There are no data in CXRS for shot #' int2str(shot) '...' ] );
end
legend off;
hold on;
title( [ 'CXRS results for TCV shot ' int2str(shot) ', t=' num2str(wanted_ts(1)) '-' num2str(wanted_ts(2)) 's' ] );
rho_psi = G.prof{1}.X;
ti_cxrs = G.prof{1}.Y;
ax=plot(rho_psi,ti_cxrs,lincol);
output.ax=ax;

%ti_ok = interpos( rho_psi, ti_cxrs, rho_psi_200 );
ti_ok = interpos( rho_psi, ti_cxrs, rho_psi_51 );

rho_psi_51 = rho_psi_51';
ti_ok = ti_ok';

output.rhopsi=rho_psi_51;
output.ti    =ti_ok;

ti_ok = ti_ok ./ 1000;

save('tmp.rhopsi.mat','rho_psi_51','-ascii');
save('tmp.ti.mat','ti_ok','-ascii');

end
