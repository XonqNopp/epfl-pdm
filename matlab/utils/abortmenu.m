function abortmenu()
% abortmenu()
% See GUIastra store_astra_tcv and abort_storing
%

do_abort = 0;
fi = figure;
set(fi,'menubar','none');
set(fi,'units','normalized');
set(fi,'outerposition',[0.5 0.2 .1 .05]);
set(fi,'numbertitle','off');
uic = uicontrol('units','normalized',...
				'position',[0.1 .1 .8 .8],...
				'style','push',...
				'SelectionHighlight','off',...
				'string','abort',...
				'fontweight','bold',...
				'fontsize',10,...
				'fontname','arial',...
				'foregroundcolor',[0 0 0],...
				'callback',{@abort_storing},...
				'tag','abort_button',...
				'userdata',0);

pause(0.1);
fo = findobj('tag','abort_button');
while get(fo,'userdata') == 0
	fprintf('0');
	pause(0.01);
end
fprintf('\n');


function abort_storing(hload,eventdata)
	fo = findobj('tag','abort_button');
	set(fo,'userdata',1);
end

end
