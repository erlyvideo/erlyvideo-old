-module(iso8859_5).
-export([decode/1]).

decode(Bin) when is_binary(Bin) ->
  decode(Bin, <<>>).

decode(<<0, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 0>>);

decode(<<1, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 1>>);

decode(<<2, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 2>>);

decode(<<3, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 3>>);

decode(<<4, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 4>>);

decode(<<5, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 5>>);

decode(<<6, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 6>>);

decode(<<7, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 7>>);

decode(<<8, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 8>>);

decode(<<9, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 9>>);

decode(<<10, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 10>>);

decode(<<11, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 11>>);

decode(<<12, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 12>>);

decode(<<13, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 13>>);

decode(<<14, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 14>>);

decode(<<15, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 15>>);

decode(<<16, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 16>>);

decode(<<17, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 17>>);

decode(<<18, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 18>>);

decode(<<19, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 19>>);

decode(<<20, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 20>>);

decode(<<21, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 21>>);

decode(<<22, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 22>>);

decode(<<23, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 23>>);

decode(<<24, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 24>>);

decode(<<25, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 25>>);

decode(<<26, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 26>>);

decode(<<27, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 27>>);

decode(<<28, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 28>>);

decode(<<29, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 29>>);

decode(<<30, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 30>>);

decode(<<31, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 31>>);

decode(<<32, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 32>>);

decode(<<33, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 33>>);

decode(<<34, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 34>>);

decode(<<35, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 35>>);

decode(<<36, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 36>>);

decode(<<37, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 37>>);

decode(<<38, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 38>>);

decode(<<39, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 39>>);

decode(<<40, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 40>>);

decode(<<41, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 41>>);

decode(<<42, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 42>>);

decode(<<43, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 43>>);

decode(<<44, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 44>>);

decode(<<45, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 45>>);

decode(<<46, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 46>>);

decode(<<47, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 47>>);

decode(<<48, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 48>>);

decode(<<49, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 49>>);

decode(<<50, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 50>>);

decode(<<51, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 51>>);

decode(<<52, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 52>>);

decode(<<53, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 53>>);

decode(<<54, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 54>>);

decode(<<55, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 55>>);

decode(<<56, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 56>>);

decode(<<57, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 57>>);

decode(<<58, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 58>>);

decode(<<59, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 59>>);

decode(<<60, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 60>>);

decode(<<61, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 61>>);

decode(<<62, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 62>>);

decode(<<63, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 63>>);

decode(<<64, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 64>>);

decode(<<65, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 65>>);

decode(<<66, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 66>>);

decode(<<67, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 67>>);

decode(<<68, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 68>>);

decode(<<69, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 69>>);

decode(<<70, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 70>>);

decode(<<71, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 71>>);

decode(<<72, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 72>>);

decode(<<73, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 73>>);

decode(<<74, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 74>>);

decode(<<75, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 75>>);

decode(<<76, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 76>>);

decode(<<77, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 77>>);

decode(<<78, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 78>>);

decode(<<79, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 79>>);

decode(<<80, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 80>>);

decode(<<81, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 81>>);

decode(<<82, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 82>>);

decode(<<83, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 83>>);

decode(<<84, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 84>>);

decode(<<85, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 85>>);

decode(<<86, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 86>>);

decode(<<87, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 87>>);

decode(<<88, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 88>>);

decode(<<89, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 89>>);

decode(<<90, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 90>>);

decode(<<91, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 91>>);

decode(<<92, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 92>>);

decode(<<93, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 93>>);

decode(<<94, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 94>>);

decode(<<95, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 95>>);

decode(<<96, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 96>>);

decode(<<97, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 97>>);

decode(<<98, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 98>>);

decode(<<99, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 99>>);

decode(<<100, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 100>>);

decode(<<101, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 101>>);

decode(<<102, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 102>>);

decode(<<103, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 103>>);

decode(<<104, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 104>>);

decode(<<105, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 105>>);

decode(<<106, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 106>>);

decode(<<107, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 107>>);

decode(<<108, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 108>>);

decode(<<109, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 109>>);

decode(<<110, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 110>>);

decode(<<111, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 111>>);

decode(<<112, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 112>>);

decode(<<113, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 113>>);

decode(<<114, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 114>>);

decode(<<115, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 115>>);

decode(<<116, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 116>>);

decode(<<117, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 117>>);

decode(<<118, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 118>>);

decode(<<119, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 119>>);

decode(<<120, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 120>>);

decode(<<121, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 121>>);

decode(<<122, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 122>>);

decode(<<123, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 123>>);

decode(<<124, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 124>>);

decode(<<125, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 125>>);

decode(<<126, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 126>>);

decode(<<127, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 127>>);

decode(<<128, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,128>>);

decode(<<129, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,129>>);

decode(<<130, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,130>>);

decode(<<131, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,131>>);

decode(<<132, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,132>>);

decode(<<133, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,133>>);

decode(<<134, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,134>>);

decode(<<135, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,135>>);

decode(<<136, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,136>>);

decode(<<137, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,137>>);

decode(<<138, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,138>>);

decode(<<139, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,139>>);

decode(<<140, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,140>>);

decode(<<141, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,141>>);

decode(<<142, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,142>>);

decode(<<143, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,143>>);

decode(<<144, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,144>>);

decode(<<145, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,145>>);

decode(<<146, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,146>>);

decode(<<147, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,147>>);

decode(<<148, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,148>>);

decode(<<149, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,149>>);

decode(<<150, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,150>>);

decode(<<151, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,151>>);

decode(<<152, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,152>>);

decode(<<153, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,153>>);

decode(<<154, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,154>>);

decode(<<155, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,155>>);

decode(<<156, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,156>>);

decode(<<157, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,157>>);

decode(<<158, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,158>>);

decode(<<159, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,159>>);

decode(<<160, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,160>>);

decode(<<161, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,129>>);

decode(<<162, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,130>>);

decode(<<163, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,131>>);

decode(<<164, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,132>>);

decode(<<165, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,133>>);

decode(<<166, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,134>>);

decode(<<167, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,135>>);

decode(<<168, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,136>>);

decode(<<169, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,137>>);

decode(<<170, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,138>>);

decode(<<171, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,139>>);

decode(<<172, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,140>>);

decode(<<173, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,173>>);

decode(<<174, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,142>>);

decode(<<175, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,143>>);

decode(<<176, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,144>>);

decode(<<177, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,145>>);

decode(<<178, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,146>>);

decode(<<179, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,147>>);

decode(<<180, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,148>>);

decode(<<181, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,149>>);

decode(<<182, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,150>>);

decode(<<183, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,151>>);

decode(<<184, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,152>>);

decode(<<185, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,153>>);

decode(<<186, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,154>>);

decode(<<187, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,155>>);

decode(<<188, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,156>>);

decode(<<189, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,157>>);

decode(<<190, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,158>>);

decode(<<191, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,159>>);

decode(<<192, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,160>>);

decode(<<193, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,161>>);

decode(<<194, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,162>>);

decode(<<195, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,163>>);

decode(<<196, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,164>>);

decode(<<197, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,165>>);

decode(<<198, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,166>>);

decode(<<199, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,167>>);

decode(<<200, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,168>>);

decode(<<201, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,169>>);

decode(<<202, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,170>>);

decode(<<203, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,171>>);

decode(<<204, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,172>>);

decode(<<205, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,173>>);

decode(<<206, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,174>>);

decode(<<207, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,175>>);

decode(<<208, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,176>>);

decode(<<209, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,177>>);

decode(<<210, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,178>>);

decode(<<211, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,179>>);

decode(<<212, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,180>>);

decode(<<213, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,181>>);

decode(<<214, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,182>>);

decode(<<215, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,183>>);

decode(<<216, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,184>>);

decode(<<217, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,185>>);

decode(<<218, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,186>>);

decode(<<219, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,187>>);

decode(<<220, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,188>>);

decode(<<221, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,189>>);

decode(<<222, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,190>>);

decode(<<223, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 208,191>>);

decode(<<224, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 209,128>>);

decode(<<225, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 209,129>>);

decode(<<226, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 209,130>>);

decode(<<227, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 209,131>>);

decode(<<228, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 209,132>>);

decode(<<229, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 209,133>>);

decode(<<230, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 209,134>>);

decode(<<231, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 209,135>>);

decode(<<232, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 209,136>>);

decode(<<233, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 209,137>>);

decode(<<234, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 209,138>>);

decode(<<235, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 209,139>>);

decode(<<236, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 209,140>>);

decode(<<237, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 209,141>>);

decode(<<238, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 209,142>>);

decode(<<239, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 209,143>>);

decode(<<240, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 226,132,150>>);

decode(<<241, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 209,145>>);

decode(<<242, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 209,146>>);

decode(<<243, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 209,147>>);

decode(<<244, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 209,148>>);

decode(<<245, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 209,149>>);

decode(<<246, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 209,150>>);

decode(<<247, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 209,151>>);

decode(<<248, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 209,152>>);

decode(<<249, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 209,153>>);

decode(<<250, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 209,154>>);

decode(<<251, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 209,155>>);

decode(<<252, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 209,156>>);

decode(<<253, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 194,167>>);

decode(<<254, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 209,158>>);

decode(<<255, Bin/binary>>, Acc) ->
  decode(Bin, <<Acc/binary, 209,159>>);

decode(<<>>, Acc) -> Acc.

