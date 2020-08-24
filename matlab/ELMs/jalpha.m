function jalpha(astra_out,rhos,tcrash,tauELM,deltaELM,varargin)
% jalpha(astra_out,rhos,tcrash,tauELM,deltaELM,varargin)
%   varargin{ 1 } : addname for picture
%           { 2 } : 1 for background run
%           { 3 } : 1 for 3D plot
%

is3d = 0;
if size(varargin,2)>2 && ~isempty(varargin{3})
	is3d = varargin{3};
end
addname = '';
addname2='';
if size(varargin,2) > 0 && ~isempty(varargin{1})
	addname = [ '_' varargin{1} ];
	addname2 = [ ' (' addname(2:end) ')'];
end
hidden = 0;
if size(varargin,2)>1 && ~isempty(varargin{2})
	hidden = varargin{2};
end

R0 = 0.88;%            [m]
mu0 = 4e-7 * pi;%      [T m A^-1]
shot = astra_out.shot;
t0 = astra_out.t0(1);% [s]

t = ( astra_out.t - tcrash ) .* 1000;% [10^-3 s]
t_neg = -t( t < 0 );
it1 = find(t_neg==min(t_neg));
it2 = iround(t,tauELM);
itend = iround(t,deltaELM) - 1;
%t_elm = t(itend)
%t_0 = t(it1)
it3 = iround(t,0.5);
it4 = iround(t,1);
%keyboard
%its = itend - it2;
%it3 = round(its / 10);
%it4 = round(2 * its / 10);

rminor = astra_out.ametr;%            [m]
rhovol = astra_out.rhovol;%           []
rhostep = rhovol(2,1) - rhovol(1,1);% []
volum = astra_out.volum;%             [m^3]
q = astra_out.q;%                     []
ne = astra_out.ne .* 1e19;%           [m^-3]
Te = astra_out.te .* 1.602e-16;%      [J]
Bt = astra_out.btor;%                 [T]
jtot = astra_out.cu .* 1e6;%          [A m^-2]
itot = astra_out.itot .* 1e6;%        [A]
elon = astra_out.elon(end,:);% []
ra = rminor(end,:);
% tor flux divided by B0??
% CHEASE?? 0.32 or 0.25?
surface = pi .* ra.^2 .* elon;% What to put?
%%% All are in derived SI units by now %%%
% J = N m
% T = N / (A m)

if length(rhos)==2
	rhos = [rhos(1):rhostep:rhos(end)+rhostep];
end
if size(rhos,2) > size(rhos,1)
	rhos = rhos';
end
irhos = iround(rhovol,rhos);

pe = ne .* Te;%                                            [J m^-3]
q2 = q.^2;%                                                []
pmag = Bt.^2 ./ ( 2 * mu0 );%                              [T A m^-1]
for ii = 1:size(pe,2)
	beta(:,ii) = pe(:,ii) ./ pmag(ii);%                    [J m^-2 T^-1 A^-1] = []
	[aa alpha(:,ii)] = interpos(rminor(:,ii),beta(:,ii));% [m^-1]
	% J
	jtot(:,ii) = jtot(:,ii) ./ itot(end,ii);%           [m^-2]
	jtot(:,ii) = jtot(:,ii) .* surface(:,ii);%          [] is ok???
end
% ALPHA
alpha = - alpha .* R0 .* q2;%                              [] OK

min_alpha = floor(10*min(min(alpha(irhos,:))))/10;
max_alpha = ceil(5*max(max(alpha(irhos,:))))/5;
min_j     = min(floor(5*min(min(jtot(irhos,:)))),0)/5;
max_j     = ceil(5*max(max(jtot(irhos,:))))/5;

%keyboard

toplot_x_1 = zeros(it2-it1,length(irhos));
toplot_y_1 = zeros(it2-it1,length(irhos));
toplot_x_2 = zeros(it3-it2,length(irhos));
toplot_y_2 = zeros(it3-it2,length(irhos));
toplot_x_3 = zeros(it4-it3,length(irhos));
toplot_y_3 = zeros(it4-it3,length(irhos));
toplot_x_4 = zeros(itend-it4+1,length(irhos));
toplot_y_4 = zeros(itend-it4+1,length(irhos));
for jj = 1:length(irhos)
	toplot_x_1(:,jj) = alpha(irhos(jj),it1:it2-1);
	toplot_y_1(:,jj) = jtot(irhos(jj),it1:it2-1);
	toplot_x_2(:,jj) = alpha(irhos(jj),it2:it3-1);
	toplot_y_2(:,jj) = jtot(irhos(jj),it2:it3-1);
	toplot_x_3(:,jj) = alpha(irhos(jj),it3:it4-1);
	toplot_y_3(:,jj) = jtot(irhos(jj),it3:it4-1);
	toplot_x_4(:,jj) = alpha(irhos(jj),it4:itend);
	toplot_y_4(:,jj) = jtot(irhos(jj),it4:itend);
end
%keyboard

lnstyle={':','-.', '-', '--'};

figure('name',['j-alpha' addname2]);
if hidden > 0
	set(gcf,'visible','off');
end
set(gca,'fontsize',16);
hold on;
colos = get(0,'defaultaxescolororder');
for ai = 1:size(toplot_x_1,2)
	start_x = toplot_x_1(1,ai);
	start_y = toplot_y_1(1,ai);

	p0 = plot(start_x,start_y,'o','color',colos(ai,:));
end

p1 = plot(toplot_x_1,toplot_y_1,lnstyle{1});
p2 = plot(toplot_x_2,toplot_y_2,lnstyle{2});
p3 = plot(toplot_x_3,toplot_y_3,lnstyle{3});
p4 = plot(toplot_x_4,toplot_y_4,lnstyle{4});
hold off;
legend(p3,[repmat('\rho_v=',size(rhos)) num2str(rhos,'%0.2g')],'location','eastoutside');
xlabel('\alpha');
ylabel('j_{//} / <j_{//}>');
%legend(##,'location','best');
grid('on');
zoom('on');
axis([min_alpha max_alpha min_j max_j]);
print('-dpsc',['pics/' int2str(shot) '_' num2str(t0) '_jalpha' addname '.ps']);
set(gcf,'paperpositionmode','auto');
print('-dpsc',['pics/' int2str(shot) '_' num2str(t0) '_jalpha' addname '_ppm.ps'])
refax = [-0.1 3.6 0 1.4];
axis(refax);
print('-dpsc',['pics/' int2str(shot) '_' num2str(t0) '_jalpha' addname '_scaledppm.ps'])
title( [ lnstyle{1} ' 0.0-' num2str(t(it2),'%0.2g') ', ' lnstyle{2} ' ' num2str(t(it2),'%0.2g') '-' num2str(t(it3),'%0.2g') ', ' lnstyle{3} ' ' num2str(t(it3),'%0.2g') '-' num2str(t(it4),'%0.2g') ', ' lnstyle{4} ' ' num2str(t(it4),'%0.2g') '-' num2str(t(itend),'%0.2g') ' [ms]'] );

if is3d == 1
	%keyboard
	figure;
	p1 = plot3(toplot_x_1,toplot_y_1,t(it1:it2-1),lnstyle{1});
	hold on;
	p2 = plot3(toplot_x_2,toplot_y_2,t(it2:it3-1),lnstyle{2});
	p3 = plot3(toplot_x_3,toplot_y_3,t(it3:it4-1),lnstyle{3});
	p4 = plot3(toplot_x_4,toplot_y_4,t(it4:itend),lnstyle{4});
	hold off;
	legend(p3,[repmat('\rho_v=',size(rhos)) num2str(rhos,'%0.2g')],'location','eastoutside');
	xlabel('\alpha');
	ylabel('j_{//} / <j_{//}>');
	zlabel('t [ms]');
	grid('on');
	zoom('on');
	title( [ lnstyle{1} ' 0.0-' num2str(t(it2),'%0.2g') ', ' lnstyle{2} ' ' num2str(t(it2),'%0.2g') '-' num2str(t(it3),'%0.2g') ', ' lnstyle{3} ' ' num2str(t(it3),'%0.2g') '-' num2str(t(it4),'%0.2g') ', ' lnstyle{4} ' ' num2str(t(it4),'%0.2g') '-' num2str(t(itend),'%0.2g') ' [ms]'] );

end
end
