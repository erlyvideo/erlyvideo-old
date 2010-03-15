{application, eamf,
  [
	  {description, "A Library for serializing and de-serializing Action Message Format (AMF)"},
	  {vsn, "0.0.1"},
	  {modules, [amf0, amf3, amf0_tests, amf3_tests]},
    {registered, []},
    {env, []},
    {applications, [kernel, stdlib]}
  ]
}.