function plotdd( astra_out, quantity, varargin )
% plotdd( astra_out, quantity, varargin )
%    Plots the quantity together with its spatial derivative (using rhovol)
%      quantity : the field of astra_out you want to be plotted
%                 if you want a product of fields (e. g. the pressure),
%                 you must provide an input variable as structure with the fields cell and lab.
%                 cell will be cell that contains all the fields which must be multiplied together,
%                 lab is the ylabel of the plot
%                 For the pressure example: quantity.cell={'ne','te'}; quantity.lab='pe';
%      varargin{ 1 } : time of simulation (default is end)
%              { 2 } : factor of multiplication for multiple quantities
%              { 3 } : 1 to reverse the axis of the derivative

shot = astra_out.shot;
t = astra_out.t;
% Varargin
if size( varargin, 2 ) >= 1 && ~isempty( varargin{1} ) && varargin{1} >= 0
	it = int2str( iround( t, varargin{1} ) );
else
	it = 'end';
end
if size( varargin, 2 ) >= 2 && ~isempty( varargin{2} ) && varargin{2} ~= 0
	factor = varargin{2};
else
	factor = 1;
end

rhovol = eval( [ 'astra_out.rhovol(:,' it ')' ] );
if ~isstruct( quantity )
	qq = eval( [ 'astra_out.' quantity '(:,' it ')' ] );
	ylab = quantity;
else
	qq = factor .* ones( size( rhovol ) );
	for ii = 1:numel(quantity.cell)
		qq = qq .* eval( [ 'astra_out.' quantity.cell{ii} '(:,' it ')' ] );
	end
	ylab = quantity.lab;
end

plotdd( rhovol, qq, ylab );
if size(varargin,2)>2 && ~isempty(varargin{3}) && varargin{3}==-1
	subplot(2,1,2);
	axis('ij');
end
print( '-dpsc', [ 'pics/' int2str( shot ) '_plotd_' ylab eval( [ 'num2str( t(' it ') )' ] ) '.ps' ] );

end
