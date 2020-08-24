function t_i = plotCH( astra_out, quantities, t_i0, t_crash, deltaELM, addname0, varargin )
% t_i = plotCH( astra_out, quantities, t_i0, t_crash, deltaELM, addname0 )
%   Combines the following functions (see their helps):
%      plot_choose_t
%      plot_chosen_profiles
%

if ~strcmp( addname0, '' )
	addname = [ '_' addname0 ];
else
	addname = '';
end

if size( varargin, 2 ) > 0 && ~isempty( varargin{1} )
	savepics = varargin{1};
else
	savepics = 1;
end

shot = astra_out.shot;
t0 = astra_out.t0(1);
filename = [ 'datafiles/ti_' int2str( shot ) '_' num2str(t0) addname '.mat' ];

if ~isempty( t_i0 )
	rerun = input( ' Do you want to rerun plot_choose_t to check if there are more times needed ? (1/0) ' );
else
	rerun = 1;
end
if rerun
	t_i = plot_choose_t( astra_out, quantities, t_crash, t_i0 );
	save( filename, 't_i', '-ascii' );
else
	t_i = t_i0;
end
if size(t_i,1)<size(t_i,2)
	t_i=t_i';
end
%if exist( 'addname0', 'var' )
	plot_chosen_profiles( astra_out, quantities, t_i, t_crash, deltaELM, addname0, savepics );
%else
	%plot_chosen_profiles( astra_out, quantities, t_i, t_crash, deltaELM );
%end

disp( [ '  Saving times into ' filename ] );
save( filename, 't_i', '-ascii' );
end
