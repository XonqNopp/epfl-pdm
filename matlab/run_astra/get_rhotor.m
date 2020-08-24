function rhotor = get_rhotor(shot,t0,N)
% rhotor = get_rhotor(shot,t0,N)
%

mdsopen(shot);
rhotor_tdi = tdi('\results::conf:rhotor');
mdsclose;
it=iround(rhotor_tdi.dim{2},t0);
rhotor_conf = rhotor_tdi.data(:,it);
rho_au = linspace(0,1,length(rhotor_conf));
rho_auN = linspace(0,1,N);
rhotor = interpos(rho_au,rhotor_conf,rho_auN);
if size(rhotor,2)>size(rhotor,1)
	rhotor = rhotor';
end
if nargout == 0
	save('tmp.rhotor','rhotor','-ascii');
end

end

