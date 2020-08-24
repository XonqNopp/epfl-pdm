function EF(varargin)
if size(varargin,2)>0
	noname=varargin{1};
else
	noname='';
end
printname = [ 'tmp/ST' noname '_' ];
load(['datafiles/39874_0.5_EF' noname '.mat']);
rhovol = astra_out.rhovol(:,end);
te     = astra_out.te;
ne     = astra_out.ne;
fp     = astra_out.fp;
mu     = astra_out.muu;
q      = astra_out.q;
q0 = q(1,:);
dq0 = diff(q0);
icrash = find( dq0 > 0.01 );
for ii=1:length(icrash)
	% TE plot %
	figure;
	plot(rhovol,te(:,icrash(ii)-1));
	hold on;
	plot(rhovol,te(:,icrash(ii)+1),'-r');
	legend('before','after','location','southwest');
	xlabel('\rho_V');
	ylabel('te');
	title(['#39874, t0=0.5, sawtooth #' int2str(ii)]);
	print('-dpsc',[printname int2str(ii) '_te.ps']);
	% NE plot %
	figure;
	plot(rhovol,ne(:,icrash(ii)-1));
	hold on;
	plot(rhovol,ne(:,icrash(ii)+1),'-r');
	legend('before','after','location','southwest');
	xlabel('\rho_V');
	ylabel('ne');
	title(['#39874, t0=0.5, ST #' int2str(ii)]);
	print('-dpsc',[printname int2str(ii) '_ne.ps']);
	% q plot %
	figure;
	plot(rhovol,q(:,icrash(ii)-1));
	hold on;
	plot(rhovol,q(:,icrash(ii)+1),'-r');
	legend('before','after','location','northwest');
	xlabel('\rho_V');
	ylabel('q');
	title(['#39874, t0=0.5, ST #' int2str(ii)]);
	print('-dpsc',[printname int2str(ii) '_q.ps']);
	% 1/mu plot %
	figure;
	plot(rhovol,1./mu(:,icrash(ii)-1));
	hold on;
	plot(rhovol,1./mu(:,icrash(ii)+1),'-r');
	legend('before','after','location','northwest');
	xlabel('\rho_V');
	ylabel('1/mu');
	title(['#39874, t0=0.5, ST #' int2str(ii)]);
	print('-dpsc',[printname int2str(ii) '_1overMu.ps']);
	% fp plot %
	figure;
	plot(rhovol,fp(:,icrash(ii)-1));
	hold on;
	plot(rhovol,fp(:,icrash(ii)+1),'-r');
	legend('before','after','location','northwest');
	xlabel('\rho_V');
	ylabel('fp');
	title(['#39874, t0=0.5, ST #' int2str(ii)]);
	print('-dpsc',[printname int2str(ii) '_fp.ps']);
end
end
