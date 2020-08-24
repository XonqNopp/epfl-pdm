function y = a_fheH( x, x0, dx, min_req, ampli )
% y = a_fheH( x, x0, dx, min_req, ampli )
%   warning: min_req must be same size as x
	y = ampli .* max( exp( -( x0 - x ).^2 ./ ( ( dx / 2 ).^2 ) ), min_req );
end
