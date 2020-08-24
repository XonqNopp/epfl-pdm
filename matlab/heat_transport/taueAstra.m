function taue = taueAstra( astra_out, varargin )
% taue = taueAstra( astra_out, varargin )
%   Computes the taue [s]
%     varargin{ 1 } : time of simulation (default is end)
%             { 2 } : range of rhovol [rho_start rho_end]
%

error( 'Are they summable? Is the denominator always integrated on the whole volum??' );

if size( varargin, 2 ) >= 1 && ~isempty( varargin{1} )
	it = varargin{ 1 };
else
	it = [];
end
if size( varargin, 2 ) >= 2 && ~isempty( varargin{2} )
	range = varargin{ 2 };
else
	range = [];
end

sources = sourcesAstra( astra_out, it );%, range );
we = weAstra( astra_out, it, range );
%keyboard
taue = we / sources;

end
