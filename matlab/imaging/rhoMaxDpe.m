function rho = rhoMaxDpe(astra_out,varargin)
% rho = rhoMaxDpe(astra_out,varargin)
%

leg = {};
shot = astra_out(1).shot;
t0   = astra_out(1).t0(1);

for ii = 1:length(astra_out)
	ne = 1e19 .* astra_out(ii).ne(:,end);
	te = 1.602e-16 .* astra_out(ii).te(:,end);
	rminor = astra_out(ii).ametr(:,end);
	rhovol = astra_out(ii).rhovol(:,end);
	toplotx(:,ii) = rhovol;
	gradro = astra_out(ii).gradro(:,end);
	if length(astra_out)>1 & isfield(astra_out(ii),'name')
		leg{ii} = astra_out(ii).name;
	end

	pe = ne .* te ./1000;
	[aa dpe]=interpos(rminor,pe);
	dpe = -dpe;% .* gradro;
	toploty(:,ii) = dpe;

	f0 = figure;
	plot( rhovol, dpe );
	xlabel( '\rho_V' );
	ylabel( '- \nabla p_e' );
	grid( 'on' );
	zoom( 'on' );
	title('Press enter when ready');
	pause;

	title('Choose boundaries to find max');
	[xx yy]=ginput(2);
	irho1 = iround(rhovol,xx(1));
	irho2 = iround(rhovol,xx(2));
	new_dpe = dpe(irho1:irho2);
	ipe = find(new_dpe==max(new_dpe));
	ipe = irho1 - 1 + ipe;
	rho(ii) = rhovol(ipe);
	hold on;
	plot(rho,dpe(ipe),'or','markersize',15);
	pause;
	close(f0);
end

%keyboard

figpos = [ 3 132 1265 820 ];
figure('name','- grad pe');
set(gca,'fontsize',16);
pp = plot( toplotx, toploty );
if length(pp) == 2
	set(pp(2),'linestyle','--');
end
hold on;
xlabel('\rho_V');
ylabel('- \nabla p_e [kPa m^{-1}]');
addname = '';
if ~isempty(leg)
	%legend(leg,'Location','NorthEast');
	if isempty(strfind(leg{end},'std'))
		addname = ['_' leg{end}];
	end
end
for ii = 1:length(astra_out)
	irho = iround(toplotx(:,ii),rho(ii));
	rhoplotx(1,ii) = rho(ii);
	rhoploty(1,ii) = toploty(irho,ii);
	rhoploty(2,ii) = -toploty(irho,ii);
end
yl = ylim;
plot(rhoplotx,rhoploty,'x','markersize',15);
ylim(yl);
grid('on');
zoom('on');
set(gcf,'paperpositionmode','auto');
print('-dpsc',['pics/' int2str(shot) '_' num2str(t0) '_maxGradPe' addname '_ppm.ps']);
set(gcf,'paperpositionmode','manual','outerposition',figpos);
print('-dpsc',['pics/' int2str(shot) '_' num2str(t0) '_maxGradPe' addname '.ps']);

end
