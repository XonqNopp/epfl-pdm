function rminor = get_rminor(shot,t0,N)
% rminor = get_rminor(shot,t0,N)
%

mdsopen(shot);
rminor_tdi = tdi('\results::conf:rminor');
mdsclose;
it=iround(rminor_tdi.dim{2},t0);
rminor_conf = rminor_tdi.data(:,it);
rho_au = linspace(0,1,length(rminor_conf));
rho_auN = linspace(0,1,N);
rminor = interpos(rho_au,rminor_conf,rho_auN);
if size(rminor,2)>size(rminor,1)
	rminor = rminor';
end
if nargout == 0
	save('tmp.rminor','rminor','-ascii');
end

end

