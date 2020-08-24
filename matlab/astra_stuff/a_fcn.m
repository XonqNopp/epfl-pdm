function y = a_fcn( x, x0, dx, alpha, min_req )
	y = alpha .* max( exp( -( x0 - x ).^2 ./ ( ( dx / 2 ).^2 ) ), min_req );
end
