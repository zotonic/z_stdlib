![Test](https://github.com/zotonic/z_stdlib/workflows/Test/badge.svg)

zotonic_stdlib
==============

Zotonic standard library - a library with hopefully useful functions

Extracted from Zotonic, the Erlang CMS http://zotonic.com/


Installation
------------

Due to historical reasons the library is called `z_stdlib`.
It should be checked out as `zotonic_stdlib`:

    git clone https://github.com/zotonic/z_stdlib.git zotonic_stdlib

The library is also on Hex https://hex.pm/packages/zotonic_stdlib

In your rebar.config file, use Hex (for rebar3):

    {deps, [
        zotonic_stdlib
    ]}.

Or, directly git (rebar2):

    {deps, [
        {zotonic_stdlib, ".*", {git, "https://github.com/zotonic/z_stdlib.git", "master"}}
    ]}.


Modules
-------

The following modules are provided:


### z_convert

Conversion routines for:

 * all common data types (binary, boolean, integer, etc.).
 * dates: to_utc, to_local, iso8601 parsing
 * json, convert to simple json constructs
 * IP address conversions


### z_css

Strict CSS parser and sanitizer.

### z_cssmin

CSS minifier

### z_dateformat

Routines for formatting dates.
Example:

    z_dateformat:format({{2008,12,10},{15,30,0}}, "Y-m-d H:i:s", [ {tz, "GMT"} ]).


### z_email_dnsbl

Check IP addresses for their presence on DNS block- or allowlists.


### z_email_utils

Test if an email address is syntactically valid. Extract email addresss from a text.


### z_filelib

Convenience function to make nested directories.


### z_html

HTML routines like escape, unescape, sanitize, etc.


### z_ip_address

Match an IP address against lists, check if an IP address is a non routable LAN address.


### z_jsmin

JavaScript minimizer, removes spaces, comments and newlines.


### z_string

String routines. To uppercase, lowercase, truncate, trim, utf8 checks, and more.


### z_svg

SVG sanitizer.


### z_tempfile

Temporary file routines. Make tempfiles, including watch dog process to clean up the file.


### z_ubf

UBF-A encoding and decoding routines. Safe for atoms and maximum memory size.


### z_url

URL encode, decode and more.


### z_url_fetch

Fetch the first N bytes of an URL. Protection against too large return body.


### z_url_metadata

Fetch an URL, extract metadata like mime type, title, description, images etc.


Tests
-----

To run the test set:

    make test

All tests should pass.

