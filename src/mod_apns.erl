%% Ejabberd module for the Apple Push Notification Service
%% Created: 07/09/2015 by mrDoctorWho
%% License: MIT/X11

-module(mod_apns).
-author("mrDoctorWho").

-include("ejabberd.hrl").
-include("logger.hrl").
-include("jlib.hrl").

-behaviour(gen_mod).

-record(apns_users, {user, token, last_seen}).

-define(NS_APNS, "https://apple.com/push"). %% I hope Apple doesn't mind.

-export([start/2, stop/1, message/3, iq/3, mod_opt_type/1]).

-define(Timeout, 10000).

% partially done by uwe-arzt.de
send_payload(Host, Payload, Token) ->
	Address = gen_mod:get_module_opt(Host, ?MODULE, address, fun(V) -> binary_to_list(V) end, undefined),
	Port = gen_mod:get_module_opt(Host, ?MODULE, port, fun(V) -> V end, undefined),
	Cert = gen_mod:get_module_opt(Host, ?MODULE, certfile, fun(V) -> binary_to_list(V) end, undefined),
	Keyfile = gen_mod:get_module_opt(Host, ?MODULE, keyfile, fun(V) -> binary_to_list(V) end, undefined),
	Password = gen_mod:get_module_opt(Host, ?MODULE, password, fun(V) -> binary_to_list(V) end, undefined),

	?DEBUG("mod_apns: trying to send payload with these parameters: Address: ~s Port: ~s Cert: ~s Keyfile: ~s Password ~s",
		[Address, Port, Cert, Keyfile, Password]),

	case Password of
		undefined ->
			Options = [{certfile, Cert}, {keyfile, Keyfile}, {mode, binary}]; %, {verify, verify_none}
		_ ->
			Options = [{certfile, Cert}, {keyfile, Keyfile}, {password, Password}, {mode, binary}]
	end,

	case ssl:connect(Address, Port, Options, ?Timeout) of
		{ok, Socket} ->
			PayloadBin = list_to_binary(Payload),
			PayloadLength = size(PayloadBin),
			TokenNum = erlang:binary_to_integer(Token, 16),
			TokenBin = <<TokenNum:32/integer-unit:8>>,
			Packet = <<
				0:8,
				32:16/big,
				TokenBin/binary,
				PayloadLength:16/big,
				PayloadBin/binary
			>>,
			ssl:send(Socket, Packet),
			ssl:close(Socket),
			?DEBUG("mod_apns: Successfully sent payload to the APNS server", []),
			ok;
		{error, Reason} ->
			?ERROR_MSG("mod_apns: Unable to connect to the APNS server: ~p", [Reason]),
			Reason
	end.


create_json(List1, List2) ->
	lists:append(["{\"aps\":{", create_keyvalue(List1), "}, ", create_keyvalue(List2), "}"]).

create_keyvalue([Head]) ->
	create_pair(Head);
create_keyvalue([Head|Tail]) ->
	lists:append([create_pair(Head), ",", create_keyvalue(Tail)]).
 
create_pair({Key, Value}) ->
	lists:append([add_quotes(atom_to_list(Key)), ":", add_quotes(Value)]).
add_quotes(String) ->
	lists:append(["\"", String, "\""]).


message(From, To, Packet) ->
	Type = fxml:get_tag_attr_s(<<"type">>, Packet),
	?DEBUG("Offline message", []),
	case Type of 
		"normal" -> ok;
		_ ->
			%% Strings
			JFrom = jlib:jid_to_string(From#jid{user = From#jid.user, server = From#jid.server, resource = <<"">>}),
			JTo = jlib:jid_to_string(To#jid{user = To#jid.user, server = To#jid.server, resource = <<"">>}),
			ToUser = To#jid.user,
			ToServer = To#jid.server,
			Body = fxml:get_path_s(Packet, [{elem, <<"body">>}, cdata]),

			%% Checking subscription
			{Subscription, _Groups} = 
				ejabberd_hooks:run_fold(roster_get_jid_info, ToServer, {none, []}, [ToUser, ToServer, From]),
			case Subscription of
				both ->
					case Body of
						<<>> -> ok;
						_ ->
							Result = mnesia:dirty_read(apns_users, {ToUser, ToServer}),
							case Result of 
								[] ->
									?DEBUG("mod_apns: No such record found for ~s", [JTo]);

								[#apns_users{token = Token}] ->
									Sound = "default",
									%% TODO: Move binary_to_list to create_pair?
									%% Badges?
									Msg = [{alert, binary_to_list(Body)}, {sound, Sound}],
									Args = [{source, binary_to_list(JFrom)}, {destination, binary_to_list(JTo)}],
									JSON = create_json(Msg, Args),
									send_payload(ToServer, JSON, Token)
							end
						end;
					_ -> ok
			end
	end.


iq(#jid{user = User, server = Server}, _, #iq{sub_el = SubEl} = IQ) ->
	LUser = jlib:nodeprep(User),
	LServer = jlib:nameprep(Server),

	{MegaSecs, Secs, _MicroSecs} = erlang:timestamp(),
	TimeStamp = MegaSecs * 1000000 + Secs,

	Token = fxml:get_tag_cdata(fxml:get_subtag(SubEl, <<"token">>)),

	F = fun() -> mnesia:write(#apns_users{user={LUser, LServer}, token=Token, last_seen=TimeStamp}) end,

	case mnesia:dirty_read(apns_users, {LUser, LServer}) of
		[] ->
			mnesia:transaction(F),
			?DEBUG("mod_apns: New user registered ~s@~s", [LUser, LServer]);

		%% Record exists, the key is equal to the one we know
		[#apns_users{user={LUser, LServer}, token=Token}] ->
			mnesia:transaction(F),
			?DEBUG("mod_apns: Updating last_seen for user ~s@~s", [LUser, LServer]);

		%% Record for this key has been found, but with another token
		[#apns_users{user={LUser, LServer}, token=_}] ->
			mnesia:transaction(F),
			?DEBUG("mod_apns: Updating token for user ~s@~s", [LUser, LServer])
		end,
	
	IQ#iq{type=result, sub_el=[]}. %% We don't need the result, but the handler has to send something.


start(Host, _) -> 
	crypto:start(),
	ssl:start(),
	mnesia:create_table(apns_users, [{disc_copies, [node()]}, {attributes, record_info(fields, apns_users)}]),
	gen_iq_handler:add_iq_handler(ejabberd_local, Host, <<?NS_APNS>>, ?MODULE, iq, no_queue),
	ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, message, 49),
	?INFO_MSG("mod_apns Has started successfully!", []),
	ok.

stop(_) -> ok.


mod_opt_type(address) -> fun iolist_to_binary/1; %binary_to_list?
mod_opt_type(port) -> fun(I) when is_integer(I) -> I end;
mod_opt_type(certfile) -> fun iolist_to_binary/1;
mod_opt_type(keyfile) -> fun iolist_to_binary/1;
mod_opt_type(password) -> fun iolist_to_binary/1;
mod_opt_type(_) ->
    [address, port, certfile, keyfile, password].

