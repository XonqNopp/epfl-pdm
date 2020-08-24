function print3ps(fig,name,ax,restore,varargin)
%% print3ps(fig,name,ax,restore,varargin)
%%

figpos = [ 3 132 1265 820 ];
if isempty(restore)
	restore = 1;
end
if isempty(fig) || fig == 0
	fig = gcf;
end

%% PPM (beamer)
set(fig,'paperpositionmode','auto');
print(fig,'-dpsc',[name '_ppm.ps']);
%% Scaled ppm
if ~isempty(ax)
	ax_old = axis;
	axis(ax);
	print(fig,'-dpsc',[name '_scaledppm.ps']);
	axis(ax_old);
end
%% Outerposition (report)
set(fig,'paperpositionmode','manual','outerposition',figpos);
print(fig,'-dpsc',[name '.ps']);

end
