-module(providers).

%% API
-export([create/1,
         new/2,
         do/2,
         impl/1,
         opts/1,
         desc/1,
         process_deps/2,
         get_provider/2,
         get_provider_by_module/2,
         get_target_providers/2,
         hooks/1,
         hooks/2,
         help/1,
         help/2,
         format_error/2,
         format/1]).

-export_type([t/0]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(provider,  { name          :: atom(),               % The 'user friendly' name of the task
                     module        :: module(),             % The module implementation of the task
                     hooks         :: {list(), list()},
                     bare          :: boolean(),            % Indicates whether task can be run by user
                     deps          :: [atom()],             % The list of dependencies
                     desc          :: string(),             % The description for the task
                     short_desc    :: string(),             % A one line short description of the task
                     example       :: string() | undefined, % An example of the task usage
                     opts          :: list() }).            % The list of options that the task requires/understands

-type t() :: #provider{}.

%%%===================================================================
%%% API
%%%===================================================================

%% @doc create a new provider object from the specified module. The
%% module should implement the provider behaviour.
%%
%% @param ModuleName The module name.
%% @param State0 The current state of the system
-spec new(module(), any()) -> {ok, any()} | {error, string()}.
new(ModuleName, State) when is_atom(ModuleName) ->
    case code:which(ModuleName) of
        non_existing ->
            {error, io_lib:format("Module ~p does not exist.", [ModuleName])};
        _ ->
            ModuleName:init(State)
    end.

-spec create(list()) -> t().
create(Attrs) ->
    #provider{ name          = proplists:get_value(name, Attrs, undefined)
             , module        = proplists:get_value(module, Attrs, undefined)
             , hooks         = proplists:get_value(hooks, Attrs, {[], []})
             , bare          = proplists:get_value(bare, Attrs, false)
             , deps          = proplists:get_value(deps, Attrs, [])
             , desc          = proplists:get_value(desc, Attrs, "")
             , short_desc    = proplists:get_value(short_desc, Attrs, "")
             , example       = proplists:get_value(example, Attrs, "")
             , opts          = proplists:get_value(opts, Attrs, []) }.

%% @doc Run provider and hooks.
%%
%% @param Provider the provider object
%% @param State the current state of the system
-spec do(t(), any()) -> {ok, any()} | {error, string()}.
do(Provider, State) ->
    {PreHooks, PostHooks} = Provider#provider.hooks,
    run_all([PreHooks++Provider | PostHooks], State).

-spec run_all([t()], any()) -> {ok, any()} | {error, string()}.
run_all([], State) ->
    {ok, State};
run_all([Provider | Rest], State) ->
    case (Provider#provider.module):do(State) of
        {ok, State1} ->
            run_all(Rest, State1);
        {error, Error} ->
            {error, Error}
    end.

%%% @doc get the name of the module that implements the provider
%%% @param Provider the provider object
-spec impl(t()) -> module().
impl(Provider) ->
    Provider#provider.name.

-spec opts(t()) -> list().
opts(Provider) ->
    Provider#provider.opts.

-spec desc(t()) -> string().
desc(Provider) ->
    Provider#provider.desc.

-spec hooks(t()) -> {[t()], [t()]}.
hooks(Provider) ->
    Provider#provider.hooks.

-spec hooks(t(), {[t()], [t()]}) -> t().
hooks(Provider, Hooks) ->
    Provider#provider{hooks=Hooks}.

help(Providers) when is_list(Providers) ->
    Help = lists:sort([{ec_cnv:to_list(P#provider.name), P#provider.short_desc} || P <- Providers,
                                                                                   P#provider.bare =/= true]),
    Longest = lists:max([length(X) || {X, _} <- Help]),

    lists:foreach(fun({Name, ShortDesc}) ->
                          Length = length(Name),
                          Spacing = lists:duplicate(Longest - Length + 8, " "),
                          io:format("~s~s~s~n", [Name, Spacing, ShortDesc])
                  end, Help);
help(#provider{opts=Opts
              ,desc=Desc
              ,name=Name}) ->
    case Desc of
        Desc when length(Desc) > 0 ->
            io:format(Desc++"~n~n");
        _ ->
            ok
    end,

    case Opts of
        [] ->
            io:format("Usage: rebar ~p~n", [Name]);
        _ ->
            getopt:usage(Opts, "rebar " ++ atom_to_list(Name), "", [])
    end.

help(Name, Providers) when is_list(Name) ->
    help(list_to_atom(Name), Providers);
help(Name, Providers) when is_atom(Name) ->
    Provider = providers:get_provider(Name, Providers),
    help(Provider).

%% @doc format an error produced from a provider.
-spec format_error(t(), Reason::term()) -> iolist().
format_error(#provider{module=Mod}, Error) ->
    Mod:format_error(Error).

%% @doc print the provider module name
%%
%% @param T - The provider
%% @return An iolist describing the provider
-spec format(t()) -> iolist().
format(#provider{name=Name}) ->
    atom_to_list(Name).

get_target_providers(Target, Providers) ->
    TargetProviders = lists:filter(fun(#provider{name=T}) when T =:= Target->
                                           true;
                                      (_) ->
                                           false
                                   end, Providers),
    process_deps(TargetProviders, Providers).

-spec get_provider(atom(), [t()]) -> t() | not_found.
get_provider(ProviderName, [Provider = #provider{name = ProviderName} | _]) ->
    Provider;
get_provider(ProviderName, [_ | Rest]) ->
    get_provider(ProviderName, Rest);
get_provider(_ProviderName, _) ->
    not_found.

-spec get_provider_by_module(atom(), [t()]) -> t() | not_found.
get_provider_by_module(ProviderModule, [Provider = #provider{module = ProviderModule} | _]) ->
    Provider;
get_provider_by_module(ProviderModule, [_ | Rest]) ->
    get_provider_by_module(ProviderModule, Rest);
get_provider_by_module(_ProviderModule, _) ->
    not_found.

process_deps([], _Providers) ->
    [];
process_deps(TargetProviders, Providers) ->
    DepChain = lists:flatmap(fun(Provider) ->
                                     {DC, _, _} = process_deps(Provider, Providers, []),
                                     DC
                             end, TargetProviders),
    ['NONE' | Rest] =
        reorder_providers(lists:flatten([{'NONE', P#provider.name} || P <- TargetProviders] ++ DepChain)),
    Rest.

process_deps(Provider, Providers, Seen) ->
    case lists:member(Provider, Seen) of
        true ->
            {[], Providers, Seen};
        false ->
            Deps = Provider#provider.deps,
            DepList = lists:map(fun(Dep) ->
                                        {Dep, Provider#provider.name}
                                end, Deps),
            {NewDeps, _, NewSeen} =
                lists:foldl(fun(Arg, Acc) ->
                                    process_dep(Arg, Acc)
                            end,
                           {[], Providers, Seen}, Deps),
            {[DepList | NewDeps], Providers, NewSeen}
    end.

process_dep(ProviderName, {Deps, Providers, Seen}) ->
    Provider = get_provider(ProviderName, Providers),
    {NewDeps, _, NewSeen} = process_deps(Provider, Providers, [ProviderName | Seen]),
    {[Deps | NewDeps], Providers, NewSeen}.

%% @doc Reorder the providers according to thier dependency set.
reorder_providers(OProviderList) ->
    case providers_topo:sort(OProviderList) of
        {ok, ProviderList} ->
            ProviderList;
        {error, {cycle, _}} ->
            {error, "There was a cycle in the provider list. Unable to complete build!"}
    end.
