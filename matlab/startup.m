% Loading paths...
format longG;
[ sysback, sysres ] = system( 'echo $HOSTNAME' );
clear sysback;
if sysres(2:4) ~= 'lac' & sysres(1:3) ~= 'lac'
	fprintf( 'WARNING : You won t have access to the CRPP toolbox (scripts in here need interpos for instance).\n\n' );
end
clear sysres;
addpath /home/induni/matlab/Hmode -end
addpath /home/induni/matlab/Hmode/loading -end
fulfillpath;

%set_defaults_matlab;
%set(0,'DefaultlineLineWidth',1);
col_1 = [0 0 1]; % blue
col_2 = [1. 0. 0.]; % red
col_3 = [0. 0.4 0.]; % dark green
col_4 = [0.7 0.4 0.9]; % purple
col_5 = [0. 0.9 0.8]; % cyan
col_6 = [1 0.85 0.3]; % yellow
col_7 = [0. 1. 0.]; % light green
col_8 = [1.0 0.1 0.7]; % reddish magenta
col_9= [1. 0.7 0];
col_10 = [0 0.52 0.93];
col_11= [0. 0. 0.]; % black
col_12= [0.075 0.933 0.676];
col_13 = [0.8 0.1 0.95];
% using uisetcolor
typemk={'*', 'o', 'p', 'd', 's', '^', 'v'};
lnstyle={'-', '--', '-.', ':'};
colos=[col_1; col_2; col_3; col_4; col_5; col_6; col_7; col_8; col_9; col_10; col_11; col_12; col_13];
for ii = 1:13
	eval( [ 'clear col_' int2str(ii) ';' ] );
end
clear ii;
set(0,'DefaultAxesColorOrder',colos);
clear colos;
set(0,'DefaultaxesXGrid','on');
set(0,'DefaultaxesYGrid','on');
set(0,'DefaultlineLineWidth',2);
set(0,'DefaultaxesFontSize',12);
set(0,'DefaultaxesFontWeight','bold');
set(0,'DefaulttextFontSize',12);
set(0,'DefaulttextFontWeight','bold');
set(0,'DefaultlineMarkerSize',7);
set(0,'DefaultsurfaceMarkerSize',7);
set(0,'DefaultsurfaceMarkerFaceColor','auto');
set(0,'defaultaxesfontname','helvetica');
set(0,'defaulttextfontname','helvetica');
set(0,'defaultuicontrolfontname','helvetica');
set(0,'fixedwidthfontname','helvetica');

loadvars;

