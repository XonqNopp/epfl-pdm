% Added pathes:
addp = { '/home/induni/matlab/Hmode/run_astra'
         '/home/induni/matlab/Hmode/sawteeth'
         '/home/induni/matlab/Hmode/utils'
         '/home/induni/matlab/Hmode/miscellaneous'
         '/home/induni/matlab/Hmode/heat_transport'
         '/home/induni/matlab/Hmode/imaging'
         '/home/induni/matlab/Hmode/astra_stuff'
         '/home/induni/matlab/Hmode/ELMs'
         '/home/induni/matlab/GUIastra/crpptbx'
         '/home/karpusho/public/NPAs'
         '/macii/karpusho/Work/CXRS/results/'
         '/macii/cxrs/matlab/analysis'
};
for ii = 1:length(addp)
	eval( [ 'addpath ' addp{ii} ' -end;' ] );
end
clear ii addp;

%%% Display %%%
%newpath = path;
%ips = strfind( newpath, ':' );
%disp( ' * Added paths:' );
%Nnew = (length(addp)-1) + 2;
%for ii = Nnew:-1:1
	%disp( [ '  > ' newpath( ips( end - ii )+1 : ips( end - ii + 1 )-1 ) ] );
%end
%disp( [ '  > ' newpath( ips( end )+1 : end ) ] );
%clear newpath addp;
