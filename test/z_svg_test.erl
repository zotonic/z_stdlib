-module(z_svg_test).

-include_lib("eunit/include/eunit.hrl").

svg_test() ->
    ?assertEqual(svg1(), z_svg:sanitize(svg1())),
    ?assertEqual(svg2_out(), z_svg:sanitize(svg2())).


svg1() ->
<<"<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" style=\"height:25px; width:300px; display:inline-block; \">",
"<circle cx=\"10\" cy=\"10\" r=\"4\" stroke=\"black\" stroke-width=\"1\" fill=\"none\"></circle>",
"<line x1=\"14\" y1=\"10\" x2=\"286\" y2=\"10\" stroke-width=\"2\" stroke=\"black\"></line>",
"<circle cx=\"290\" cy=\"10\" r=\"4\" stroke=\"black\" stroke-width=\"1\" fill=\"none\"></circle>",
"</svg>">>.

%% Filter external URL references
svg2() -> <<"
<svg width=\"80px\" height=\"30px\" viewBox=\"0 0 80 30\" xmlns=\"http://www.w3.org/2000/svg\">
  <rect x=\"10\" y=\"10\" width=\"60\" height=\"10\" 
        fill=\"url(#Gradient01)\"  />
  <rect x=\"10\" y=\"10\" width=\"60\" height=\"10\" 
        fill=\"url(http://example.com/)\"  />
</svg>
">>.

svg2_out() -> <<
"<svg width=\"80px\" height=\"30px\" viewBox=\"0 0 80 30\" xmlns=\"http://www.w3.org/2000/svg\">
  <rect x=\"10\" y=\"10\" width=\"60\" height=\"10\" fill=\"url(#Gradient01)\"></rect>
  <rect x=\"10\" y=\"10\" width=\"60\" height=\"10\" fill=\"\"></rect>
</svg>">>.

