m = 9.3e-31;
nu = astra_out.nuee( 1, it1 );
e = 1.6e-19;
ne = 1e19 .* astra_out.ne( 1, it1 );
eta = m * nu / ( e^2 * ne );
mu0 = 4e-7 * pi;
