function we = weAstra( astra_out, varargin )
% we = weAstra( astra_out, varargin )
%   Computes the energy of the electrons [W]
%     varargin{ 1 } : time of simulation (default is end)
%             { 2 } : range of rhovol [rho_start rho_end]
%

if size( varargin, 2 ) > 0 && ~isempty( varargin{1} ) && varargin{1} > 0
	t_sim = varargin{1};
else
	t_sim = 100;
end
if size( varargin, 2 ) > 1 && ~isempty( varargin{2} ) && numel(varargin{2}) == 2
	range = varargin{2};
else
	range = [-1 2];
end

we = weAstra( astra_out, t_sim, range );
wi = wiAstra( astra_out, t_sim, range );
wtot = we + wi;

end
