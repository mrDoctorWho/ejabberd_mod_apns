%% Created automatically by XML generator (fxml_gen.erl)
%% Source: xmpp_codec.spec

-module(mod_apns_codec).

-compile(export_all).

do_decode(<<"register">>, <<"https://apple.com/push">>,
	  El, Opts) ->
    decode_apns_register(<<"https://apple.com/push">>, Opts,
			 El);
do_decode(<<"token">>, <<"https://apple.com/push">>, El,
	  Opts) ->
    decode_apns_register_token(<<"https://apple.com/push">>,
			       Opts, El);
do_decode(Name, <<>>, _, _) ->
    erlang:error({xmpp_codec, {missing_tag_xmlns, Name}});
do_decode(Name, XMLNS, _, _) ->
    erlang:error({xmpp_codec, {unknown_tag, Name, XMLNS}}).

tags() ->
    [{<<"register">>, <<"https://apple.com/push">>},
     {<<"token">>, <<"https://apple.com/push">>}].

do_encode({apns_register, _} = Register, TopXMLNS) ->
    encode_apns_register(Register, TopXMLNS).

do_get_name({apns_register, _}) -> <<"register">>.

do_get_ns({apns_register, _}) ->
    <<"https://apple.com/push">>.

pp(apns_register, 1) -> [token];
pp(_, _) -> no.

records() -> [{apns_register, 1}].

decode_apns_register(__TopXMLNS, __Opts,
		     {xmlel, <<"register">>, _attrs, _els}) ->
    Token = decode_apns_register_els(__TopXMLNS, __Opts,
				     _els, error),
    {apns_register, Token}.

decode_apns_register_els(__TopXMLNS, __Opts, [],
			 Token) ->
    case Token of
      error ->
	  erlang:error({xmpp_codec,
			{missing_tag, <<"token">>, __TopXMLNS}});
      {value, Token1} -> Token1
    end;
decode_apns_register_els(__TopXMLNS, __Opts,
			 [{xmlel, <<"token">>, _attrs, _} = _el | _els],
			 Token) ->
    case xmpp_codec:get_attr(<<"xmlns">>, _attrs,
			     __TopXMLNS)
	of
      <<"https://apple.com/push">> ->
	  decode_apns_register_els(__TopXMLNS, __Opts, _els,
				   {value,
				    decode_apns_register_token(<<"https://apple.com/push">>,
							       __Opts, _el)});
      _ ->
	  decode_apns_register_els(__TopXMLNS, __Opts, _els,
				   Token)
    end;
decode_apns_register_els(__TopXMLNS, __Opts, [_ | _els],
			 Token) ->
    decode_apns_register_els(__TopXMLNS, __Opts, _els,
			     Token).

encode_apns_register({apns_register, Token},
		     __TopXMLNS) ->
    __NewTopXMLNS =
	xmpp_codec:choose_top_xmlns(<<"https://apple.com/push">>,
				    [], __TopXMLNS),
    _els =
	lists:reverse('encode_apns_register_$token'(Token,
						    __NewTopXMLNS, [])),
    _attrs = xmpp_codec:enc_xmlns_attrs(__NewTopXMLNS,
					__TopXMLNS),
    {xmlel, <<"register">>, _attrs, _els}.

'encode_apns_register_$token'(Token, __TopXMLNS,
			      _acc) ->
    [encode_apns_register_token(Token, __TopXMLNS) | _acc].

decode_apns_register_token(__TopXMLNS, __Opts,
			   {xmlel, <<"token">>, _attrs, _els}) ->
    Cdata = decode_apns_register_token_els(__TopXMLNS,
					   __Opts, _els, <<>>),
    Cdata.

decode_apns_register_token_els(__TopXMLNS, __Opts, [],
			       Cdata) ->
    decode_apns_register_token_cdata(__TopXMLNS, Cdata);
decode_apns_register_token_els(__TopXMLNS, __Opts,
			       [{xmlcdata, _data} | _els], Cdata) ->
    decode_apns_register_token_els(__TopXMLNS, __Opts, _els,
				   <<Cdata/binary, _data/binary>>);
decode_apns_register_token_els(__TopXMLNS, __Opts,
			       [_ | _els], Cdata) ->
    decode_apns_register_token_els(__TopXMLNS, __Opts, _els,
				   Cdata).

encode_apns_register_token(Cdata, __TopXMLNS) ->
    __NewTopXMLNS =
	xmpp_codec:choose_top_xmlns(<<"https://apple.com/push">>,
				    [], __TopXMLNS),
    _els = encode_apns_register_token_cdata(Cdata, []),
    _attrs = xmpp_codec:enc_xmlns_attrs(__NewTopXMLNS,
					__TopXMLNS),
    {xmlel, <<"token">>, _attrs, _els}.

decode_apns_register_token_cdata(__TopXMLNS, <<>>) ->
    erlang:error({xmpp_codec,
		  {missing_cdata, <<>>, <<"token">>, __TopXMLNS}});
decode_apns_register_token_cdata(__TopXMLNS, _val) ->
    _val.

encode_apns_register_token_cdata(_val, _acc) ->
    [{xmlcdata, _val} | _acc].
