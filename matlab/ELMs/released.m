function deltaWoverW = released( results_args )
% deltaWoverW = released( results_args )
%   results_args is a structure with the following fields
%        (starred field are necessary)
%     astradir (*)          : directory where the files are
%     'case considered' (*) : struct(fn,t,deltaELM,tauELM)
%

required_fields = {'astradir'};
for ii = 1:length(required_fields)
	if ~isfield(results_args,required_fields{ii})
		error( [ ' Missing field ' required_fields{ii} ] );
	end
	eval( [ required_fields{ii} ' = results_args.' required_fields{ii} ';' ] );
	results_args = rmfield(results_args,required_fields{ii});
end
optional_fields.quantities='';
optional_fields.tis='';
optional_fields.rhos0='';
optional_fields.study='';
optional_fields.dont_close = 0;
optional_fields.addname = '';
optional_fields.nonew_rhos = 0;
optional_fields.save_pics = 1;
optional_fields.addnote = {};
of = fieldnames(optional_fields);
for ii = 1:length(of)
	if isfield(results_args,of{ii})
		eval( [ of{ii} ' = results_args.' of{ii} ';' ] );
		results_args = rmfield( results_args, of{ii} );
	else
		eval( [ of{ii} ' = optional_fields.' of{ii} ';' ] );
	end
end
clear required_fields optional_fields of quantities tis rhos study dont_close save_pics nonew_rhos addnote addname;
fn = fieldnames( results_args );
for ii = 1:length(fn)
	if eval( [ '~isfield(results_args.' fn{ii} ',''fn'') | ~isfield(results_args.' fn{ii} ',''t'') | ~isfield(results_args.' fn{ii} ', ''deltaELM'' ) | ~isfield(results_args.' fn{ii} ', ''tauELM'' )' ] )
		error( [ ' Missing field for case ' fn{ii} ] );
	end
	this_arg = eval(['results_args.' fn{ii}]);
	tauELM = this_arg.tauELM;
	this_fn = this_arg.fn;
	t_crash = this_arg.t;
	disp( [ '  * Loading ' fullfile(astradir,this_fn) '...' ] );
	load(fullfile(astradir,this_fn));
	fn_leg{ii} = regexprep( strrep( fn{ii}, '_', '\_' ), '(No)?ST', '' );

	t = ( astra_out.t-t_crash ) .* 1000;% [ms]
	it1 = iround(t,0)-10;
	it2 = iround(t,tauELM/1e3)+10;
	Te = 1.602e-16 .* astra_out.te;
	ne = 1e19      .* astra_out.ne;
	volum = astra_out.volum;
	pe = Te .* ne;
	%keyboard
	%[aa aaa aaaa W1] = interpos(volum(:,it1),pe(:,it1));
	%[aa aaa aaaa W2] = interpos(volum(:,it2),pe(:,it2));
	clear aa aaa aaaa;
	for jj = 1:size(pe,2)
		[aa aaa aaaa Ws(:,jj)] = interpos(volum(:,jj),pe(:,jj));
	end
	WsOK = Ws(end,:);
	clear aa aaa aaaa;

	deltaW = WsOK(it1) - WsOK(it2);
	deltaWoverW(ii) = deltaW / WsOK(it1);

	figure;
	set(gca,'fontsize',16);
	set(gcf,'name',['W: ' fn_leg{ii} ]);
	plot(t,WsOK./1e3);
	%hold on;
	xlabel('t [ms]');
	ylabel('W [kJ]');
	grid('on');
	zoom('on');
	xlim([-0.02 tauELM/1e3+0.02]);
	title( [ 'W = ' num2str(WsOK(it1)) ', \Delta W / W = ' num2str(deltaWoverW(ii)) ' (' fn_leg{ii} ')' ] );
	%print('-dpsc',['##.ps']);
	clear astra_out t it1 it2 t_crash tauELM Te ne volum pe W1 W2 Ws WsOK deltaW;
	pause(0.01);
end

end
