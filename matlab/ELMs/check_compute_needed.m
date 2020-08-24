function out = check_compute_needed( astra_out, quantity, varargin )
% out = check_compute_needed( astra_out, quantity, varargin )
%   varargin{ 1 } : 1 for nodes data to be returned too
%           { 2 } : rhoped (needed for ibsped, ignored for others)
%

R0 = 0.88;
rhovol_astra = astra_out.rhovol(:,end);

% To be computed :
mustcompute.p_e    = 1;
mustcompute.gradp  = 1;
mustcompute.gradte = 1;
mustcompute.gradne = 1;
mustcompute.ibsped = 1;
mustcompute.lne    = 1;
mustcompute.lte    = 1;
mustcompute.itot   = 1;
mustcompute.ibs    = 1;

% To be derived :
%mustderive.ibs  = 1;
mustderive.iohm = 1;
%mustderive.itot = 1;
mustderive.icd  = 1;
%mustderive.ipol = 1;
%mustderive.ipl  = 1;

% Units
astra_units.te       = '[keV]';
astra_units.ti       = '[keV]';
astra_units.ne       = '[10^{19} m^{-3}]';
astra_units.p_e      = '[kPa]';
astra_units.jtot     = '[MA m^{-2}]';
astra_units.johm     = '[MA m^{-2}]';
astra_units.jbs      = '[MA m^{-2}]';
astra_units.ibsped   = '[MA]';
astra_units.q        = '';
astra_units.shear    = '';
astra_units.upl      = '[V]';
astra_units.ecrh     = '[MW m^{-3}]';
astra_units.LTe      = '';
astra_units.Lne      = '';
astra_units.he       = '[m^2/s]';
astra_units.xi       = '[m^2/s]';
astra_units.qe       = '[MW]';

%%% Fetching nodes data %%%
if size(varargin,2)>0 && ~isempty(varargin{1}) && varargin{1} == 1
	% SI units
	astra2conf.te = 1e3;
	astra2conf.he = 1;
	astra2conf.ti = 1e3;
	astra2conf.ne = 1e19;
	astra2conf.p_e = 1;
	astra2conf.jtot = 1e6;
	astra2conf.jbs = 1e6;
	astra2conf.q = 1;
	astra2conf.shear = 1;
	astra2conf.upl = 1;
	astra2conf.lte = 1;
	astra2conf.lne = 1;

	% nodes
	nodes.te = '\results::conf:te';
	%nodes.ti = '\results::conf:ti';% Hem, see rather CXRS
	nodes.ne = '\results::conf:ne';
	%nodes.p_e = '\results::conf:pe';
	%nodes.jtot = '\results::i_p';
	%nodes.jbs = '';
	%nodes.q = '\results::q_psi';
	%nodes.shear = '';
	%nodes.upl = '\magnetics::vloop[*,"001"]';
	nodes.lte = '\results::conf:r_lte';
	nodes.lne = '\results::conf:r_lne';

	if isfield( nodes, quantity )
		shot = astra_out.shot;
		t0 = astra_out.t0(1);
		mdsopen(shot);
		rhovol_tdi = tdi('\results::conf:rhovol');
		t_nodes = rhovol_tdi.dim{2};
		it_nodes = iround( t_nodes, t0 );
		rhovol_nodes = rhovol_tdi.data(:,it_nodes);
		q_tdi = tdi( eval( [ 'nodes.' quantity ] ) );
		if all(all(isempty(q_tdi.data)))
			error( [ ' Nodes seem empty for ' quantity '...' ] );
		end
		q_nodes = q_tdi.data(:,it_nodes);
		q_nodes = eval( [ 'q_nodes ./ astra2conf.' quantity ] );
		q_mapped = interpos(rhovol_nodes,q_nodes,rhovol_astra);

		out.nodes = q_mapped;
	%else
		%disp( [ ' Sorry, case ' quantity ' not implemented to fetch data from nodes...' ] );
	end
end

if isfield( mustcompute, quantity )
	switch quantity
		case 'p_e'
			qq = 1e19 .* astra_out.ne .* 1.602e-16 .* astra_out.te ./ 1000;
		case {'gradte','gradne'}
			disp( [ '  Computing ' quantity '...' ] );
			qtmp = eval( [ 'astra_out.' quantity(5:end) ] );
			rminor = astra_out.ametr;
			for ij = 1:size(qtmp,2)
				[aa dq(:,ij)] = interpos(rminor(:,ij),qtmp(:,ij));
			end
			qq = - dq;
		case 'gradp'
			disp( [ '  Computing ' quantity '...' ] );
			pe = 1e19 .* astra_out.ne .* 1.602e-16 .* astra_out.te ./ 1000;
			rminor = astra_out.ametr;
			for ij = 1:size(pe,2)
				[aa dpe(:,ij)] = interpos(rminor(:,ij),pe(:,ij));
			end
			qq = -dpe;
		case 'ibsped'
			if size(varargin,2)<2 || isempty(varargin{2})
				error( ' varargin{2} is needed (rhoped) to compute ibsped' );
			end
			rhoped = varargin{2};
			irhoped = iround( rhovol_astra, rhoped );
			qq = astra_out.ibs(end,:) - astra_out.ibs(irhoped,:);
		case {'lte','lne'}
			qq = R0 ./ eval( [ 'astra_out.' quantity ] );
			quantity = [ 'R_0 / ' strrep(quantity,'l','L') ];
			if ~isempty(strfind(quantity,'t'))
				quantity=strrep(quantity,'t','T');
			end
		case 'itot'
			qq = astra_out.cu;
			quantity = 'jtot';
		case 'ibs'
			qq = astra_out.cubs;
			quantity = 'jbs';
		otherwise
			error( [ ' Please implement the case ' quantity ] );
	end
elseif isfield( mustderive, quantity )
	disp( [ '  Deriving ' quantity '...' ] );
	volum = astra_out.volum;
	t     = astra_out.t;
	qp = eval( [ 'astra_out.' quantity ] );
	for kk = 1:length(t)
		[ aa qo(:,kk) ] = interpos( volum(:,kk), qp(:,kk), 3e-4 );% is it OK???
	end
	clear aa;
	qq = ( 2 * pi * R0 ) .* qo;
	% Changing name...
	quantity = strrep( quantity, 'i', 'j' );
else
	if ~isfield(astra_out,quantity)
		error( [ ' There is no field ' quantity ' in astra_out.' ] );
	end
	qq = eval( [ 'astra_out.' quantity ] );
end

out.name = quantity;
out.data = qq;
check_u = strrep(quantity,'R_0 / ','');
if isfield( astra_units, check_u )
	out.units = eval( [ 'astra_units.' check_u ] );
else
	out.units = '[a.u.]';
end

end
