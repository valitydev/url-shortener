-ifndef(shortener_bouncer_data_included__).
-define(shortener_bouncer_data_included__, ok).

-include_lib("bouncer_proto/include/bouncer_decisions_thrift.hrl").
-include_lib("bouncer_proto/include/bouncer_context_v1_thrift.hrl").

-define(TEST_USER_REALM, <<"external">>).
-define(TEST_RULESET_ID, <<"service/authz/api">>).

-define(JUDGEMENT(Resolution), #bdcs_Judgement{resolution = Resolution}).
-define(ALLOWED, {allowed, #bdcs_ResolutionAllowed{}}).
-define(FORBIDDEN, {forbidden, #bdcs_ResolutionForbidden{}}).

-endif.
