-xml(apns_register_token,
     #elem{name = <<"token">>,
	   xmlns = <<"https://apple.com/push">>,
	   module = mod_apns_codec,
	   result = '$cdata',
	   cdata = #cdata{label = '$cdata', required = true}}).

-xml(apns_register,
     #elem{name = <<"register">>,
	   xmlns = <<"https://apple.com/push">>,
	   module = mod_apns_codec,
	   result = {apns_register, '$token'},
	   refs = [#ref{name = apns_register_token,
			label = '$token',
			min = 1, max = 1}]}).
	   
%% Local Variables:
%% mode: erlang
%% End:
%% vim: set filetype=erlang tabstop=8:
