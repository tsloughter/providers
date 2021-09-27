-module(providers_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-compile([export_all, nowarn_export_all]).

all() -> [namespaces].

namespaces(_Config) ->
    Providers = make_providers([
        [{name, a},
         {module, mod_default_a},
         {deps, []}],
        [{name, a},
         {module, mod_ns_a},
         {deps, []},
         {namespace, ns}],
        [{name, b},
         {module, mod_default_b},
         {deps, [a]}, % mod_undef_a
         {namespace, default}],
        [{name, c},
         {module, mod_ns_c},
         {deps, [a]}, % mod_ns_a
         {namespace, ns}],
        [{name, d},
         {module, mod_default_d},
         {deps, [b, {ns, c}]}], % undef_b, undef_a, ns_c, ns_a
        [{name, '✅'},
         {module, '❌'},
         {namespace, '👌'}],
        [{name, d},
         {module, mod_ns_d},
         {deps, [{default, b}, c]}, % undef_b, undef_a, ns_c, ns_a
         {namespace, ns}]
    ]),
    [DA, NSA, DB, NSC, DD, NUP, NSD] = Providers,
    ?assertEqual(DD,  providers:get_provider(d, Providers)),
    ?assertEqual(DD, providers:get_provider(d, Providers, default)),
    ?assertEqual(NSD, providers:get_provider(d, Providers, ns)),
    ?assertEqual(NSD, providers:get_provider({ns,d}, Providers)),
    ?assertEqual(NUP, providers:get_provider({'👌','✅'}, Providers)),
    ?assertEqual(NUP, providers:get_provider('✅', Providers, '👌')),
    ?assertNotEqual(DD, NSD),
    ?assertEqual(default, providers:namespace(DA)),
    ?assertEqual(default, providers:namespace(DB)),
    ?assertEqual(default, providers:namespace(DD)),
    ?assertEqual(ns, providers:namespace(NSA)),
    ?assertEqual(ns, providers:namespace(NSC)),
    ?assertEqual(ns, providers:namespace(NSD)),
    ?assertEqual('👌', providers:namespace(NUP)),
    ?assertEqual("a", providers:format(DA)),
    ?assertEqual("a", providers:format(NSA)),
    ?assertEqual("✅", providers:format(NUP)),
    ?assertEqual([{default,a}],
                 providers:get_target_providers(a, Providers)),
    ?assertEqual([{default,a}],
                 providers:get_target_providers(a, Providers, default)),
    ?assertEqual([{ns,a}],
                 providers:get_target_providers(a, Providers, ns)),
    ?assertEqual([{default,a},{default,b}],
                 providers:get_target_providers(b, Providers)),
    ?assertEqual([{default,a},{default,b}],
                 providers:get_target_providers(b, Providers, default)),
    ?assertEqual([{ns,a},{ns,c}],
                 providers:get_target_providers(c, Providers, ns)),
    ?assertEqual([{default,a},{ns,a},{default,b},{ns,c},{default,d}],
                 providers:get_target_providers(d, Providers, default)),
    ?assertEqual([{default,a},{ns,a},{default,b},{ns,c},{ns,d}],
                 providers:get_target_providers(d, Providers, ns)),
    ?assertEqual([{default,a},{ns,a},{default,b},{ns,c},{ns,d}],
                 providers:get_target_providers({ns,d}, Providers)),
    ?assertEqual([{default,a},{ns,a},{default,b},{ns,c},{ns,d}],
                 providers:get_target_providers({ns,d}, Providers)),
    ?assertEqual([{'👌','✅'}],
                 providers:get_target_providers({'👌','✅'}, Providers)),
    ?assertEqual([DA,DB,DD],
                 providers:get_providers_by_namespace(default,Providers)),
    ?assertEqual([NSA,NSC,NSD],
                 providers:get_providers_by_namespace(ns,Providers)),
    ?assertEqual([NUP],
                 providers:get_providers_by_namespace('👌',Providers)),
    ok.

make_providers(Proplists) ->
    [providers:create(List) || List <- Proplists].


