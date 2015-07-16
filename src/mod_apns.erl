%% Google Cloud Messaging for Ejabberd
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

-export([start/2, stop/1, message/3, iq/3]).

 
% conversion
% done by bravenewmethod.com
hexstr_to_bin(S) ->
	hexstr_to_bin(S, []).
hexstr_to_bin([], Acc) ->
	list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
	{ok, [V], []} = io_lib:fread("~16u", [X,Y]),
	hexstr_to_bin(T, [V | Acc]).


% partially done by uwe-arzt.de
send_payload(Host, Payload, Token) ->
	crypto:start(),
	ssl:start(),
	Timeout = 10000,
	Address = gen_mod:get_module_opt(Host, ?MODULE, address, fun(V) -> V end, undefined),
	Port = gen_mod:get_module_opt(Host, ?MODULE, port, fun(V) -> V end, undefined),
	Cert = gen_mod:get_module_opt(Host, ?MODULE, certfile, fun(V) -> V end, undefined),
	Keyfile = gen_mod:get_module_opt(Host, ?MODULE, keyfile, fun(V) -> V end, undefined),
	Password = gen_mod:get_module_opt(Host, ?MODULE, password, fun(V) -> V end, undefined),

	case Keyfile of
		undefined ->
			Options = [{certfile, Cert}, {password, Password}, {mode, binary}]; %, {verify, verify_none}
		_ ->
			Options = [{certfile, Cert}, {keyfile, Keyfile}, {mode, binary}]
	end,

	case ssl:connect(Address, Port, Options, Timeout) of
		{ok, Socket} ->
			Payload = list_to_binary(Payload),
			PayloadLength = size(Payload),
			Packet = <<
				0:8,
				32:16/big,
				Token/binary,
				PayloadLength:16/big,
				Payload/binary
			>>,
			ssl:send(Socket, Packet),
			ssl:close(Socket),
			ok;
		{error, Reason} ->
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
	Type = xml:get_tag_attr_s(<<"type">>, Packet),
	?DEBUG("Offline message ~s", [From]),
	case Type of 
		"normal" -> ok;
		_ ->
			%% Strings
			JFrom = jlib:jid_to_string(From#jid{user = From#jid.user, server = From#jid.server, resource = <<"">>}),
			JTo = jlib:jid_to_string(To#jid{user = To#jid.user, server = To#jid.server, resource = <<"">>}),
			ToUser = To#jid.user,
			ToServer = To#jid.server,
			Body = xml:get_path_s(Packet, [{elem, <<"body">>}, cdata]),

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


iq(#jid{user = User, server = Server} = From, To, #iq{type = Type, sub_el = SubEl} = IQ) ->
	LUser = jlib:nodeprep(User),
	LServer = jlib:nameprep(Server),

	{MegaSecs, Secs, _MicroSecs} = now(),
	TimeStamp = MegaSecs * 1000000 + Secs,

	Token = xml:get_tag_cdata(xml:get_subtag(SubEl, <<"token">>)),

	F = fun() -> mnesia:write(#apns_users{user={LUser, LServer}, token=Token, last_seen=TimeStamp}) end,

	case mnesia:dirty_read(apns_users, {LUser, LServer}) of
		[] ->
			mnesia:transaction(F),
			?DEBUG("mod_apns: New user registered ~s@~s", [LUser, LServer]);

		%% Record exists, the key is equal to the one we know
		[#apns_users{user={LUser, LServer}, token=Token}] ->
			mnesia:transaction(F),
			?DEBUG("mod_apns: Updating last_seen for user ~s@~s", [LUser, LServer]);

		%% Record for this key has been found, but for another key
		[#apns_users{user={LUser, LServer}, token=Token}] ->
			mnesia:transaction(F),
			?DEBUG("mod_apns: Updating token for user ~s@~s", [LUser, LServer])
		end,
	
	IQ#iq{type=result, sub_el=[]}. %% We don't need the result, but the handler has to send something.


start(Host, Opts) -> 
	mnesia:create_table(apns_users, [{disc_copies, [node()]}, {attributes, record_info(fields, apns_users)}]),
	gen_iq_handler:add_iq_handler(ejabberd_local, Host, <<?NS_APNS>>, ?MODULE, iq, no_queue),
	ejabberd_hooks:add(offline_message_hook, Host, ?MODULE, message, 49),
	?INFO_MSG("mod_apns Has started successfully!", []),
	ok.

stop(Host) -> ok.