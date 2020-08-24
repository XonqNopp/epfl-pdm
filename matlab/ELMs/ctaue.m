function taus = ctaue(astra_out,rhovolped)
% taus = ctaue(astra_out,rhovolped)
%

rhos = load('datafiles/rhos_40080_0.8.mat','-ascii');
rhovol = astra_out.rhovol(:,end);
i1 = iround(rhovol,rhos(1));
i2 = iround(rhovol,rhos(2));
iped = iround(rhovol,rhovolped);
rminor = astra_out.ametr(:,end);
rped = rminor(iped);
ra = rminor(end);
volume = astra_out.volum(:,end);
chie   = astra_out.he(:,end);
c1 = chie(i1);
c2 = chie(i2);

vagain = volume(1:iped);
chieagain = chie(1:iped);
[aa aaa aaaa chietmp] = interpos(vagain,chieagain);
chieavg = chietmp(end) / vagain(end);

taus.rho1 = rped^2 / c1;
taus.avg1 = rped^2 / chieavg;
taus.rho2 = (ra - rped)^2 / c2;

taus.rho1rho2 = taus.rho1 / taus.rho2;
taus.avg1rho2 = taus.avg1 / taus.rho2;

end
