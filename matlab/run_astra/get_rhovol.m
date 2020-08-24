function rhovol = get_rhovol(shot,t0,N)
% rhovol = get_rhovol(shot,t0,N)
%

mdsopen(shot);
%rhovol_tdi = tdi('\results::conf:rhovol');
vol_tdi = tdi('\results::conf:vol');
mdsclose;
%it=iround(rhovol_tdi.dim{2},t0);
it=iround(vol_tdi.dim{2},t0);
vol = vol_tdi.data(:,it);
rhovol_conf = ( vol - vol(1) ) ./ vol(end);
rho_au = linspace(0,1,length(rhovol_conf));
rho_auN = linspace(0,1,N);
rhovol = interpos(rho_au,rhovol_conf,rho_auN);
if size(rhovol,2)>size(rhovol,1)
	rhovol = rhovol';
end
if nargout == 0
	save('tmp.rhovol','rhovol','-ascii');
end

end
