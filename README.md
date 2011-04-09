Erlyvideo
=========

Erlyvideo is a flash streaming server, written in erlang: http://erlyvideo.org/
Source code is available at http://github.com/erlyvideo/erlyvideo

All documentation is on http://erlyvideo.org/ and in doc/html/ 

Licensing
=========

Erlyvideo is distributed under the GNU General Public License version 3 and is also available under alternative licenses negotiated directly with Erlyvideo author Max Lapshin <max@maxidoors.ru>. The GPL (version 3) is included in this source tree in the file COPYING.

Erlyvideo has runtime dependencies from other packages:

* [amf](http://github.com/maxlapshin/eamf) distributed under MIT License and packaged inside Erlyvideo
* [erlydtl](http://github.com/erlyvideo/erlydtl) distributed under MIT License and packaged inside Erlyvideo
* [log4erl](http://github.com/erlyvideo/log4erl) distributed under MIT License and packaged inside Erlyvideo
* [misultin](http://github.com/ostinelli/misultin) distributed under BSD license and packaged inside Erlyvideo
* src/mochijson2.erl distributed under MIT license and packaged inside Erlyvideo

To run tests don't forget to add test vhost:

  {test, [
    {hostname, ["test.local"]},
    {rtmp_handlers, [trusted_login, remove_useless_prefix, apps_streaming, apps_recording, apps_shared_objects]},
    {www_handlers, [ems_http_rtmpt, ems_http_templates, ems_http_erlyvideo_api, ems_http_mpegts, ems_http_flv, {ems_http_file, "wwwroot"}]},
    {file_dir, "test/files"}
  ]}

Special options
=========

* dump_traffic
* warn_bad_dts_delta
* dump_frame