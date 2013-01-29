% @author Marc Worrell <marc@worrell.nl>

-module(z_email_utils_test).

-include_lib("eunit/include/eunit.hrl").

is_email_test() ->
	?assertEqual(true, z_email_utils:is_email("plop@example.com")),
	?assertEqual(true, z_email_utils:is_email("plop.blaat-hallo@ExAmple.COM")),
	?assertEqual(false, z_email_utils:is_email("x@local")),
	?assertEqual(false, z_email_utils:is_email("@@hotmail.com")),
	?assertEqual(false, z_email_utils:is_email("me@@hotmail.com")),
	ok.


extract_emails_test() ->
	?assertEqual(
		[<<"a@example.com">>],
		z_email_utils:extract_emails("a@example.com")),
	?assertEqual(
		[<<"a@example.com">>],
		z_email_utils:extract_emails("plop a@example.com pliep@x")),
	?assertEqual(
		[<<"a@example.com">>, <<"b@example.com">>],
		z_email_utils:extract_emails("plop a@example.com pliep@x@c.com,b@example.com")),
	ok.
