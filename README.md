Erlyvideo
=========

Erlyvideo is a flash streaming server, written in erlang: http://erlyvideo.org/
Source code is available at http://github.com/erlyvideo/erlyvideo

All documentation is on http://erlyvideo.org/ and in doc/html/ 

Licensing
=========

Erlyvideo is distributed under the GNU General Public License version 3 and is also available under alternative licenses negotiated directly with Erlyvideo author Max Lapshin <info@erlyvideo.org>. The GPL (version 3) is included in this source tree in the file COPYING.

Erlyvideo has runtime dependencies from other packages:

* [amf](http://github.com/maxlapshin/eamf) distributed under MIT License and packaged inside Erlyvideo
* [erlydtl](http://github.com/erlyvideo/erlydtl) distributed under MIT License and packaged inside Erlyvideo
* [log4erl](http://github.com/erlyvideo/log4erl) distributed under MIT License and packaged inside Erlyvideo
* [misultin](http://github.com/ostinelli/misultin) distributed under BSD license and packaged inside Erlyvideo
* src/mochijson2.erl distributed under MIT license and packaged inside Erlyvideo

To run tests don't forget to add test vhost:

```erlang
  {test, [
    {hostname, ["test.local"]},
    {rtmp_handlers, [trusted_login, remove_useless_prefix, apps_streaming, apps_recording, apps_shared_objects]},
    {www_handlers, [ems_http_rtmpt, ems_http_templates, ems_http_erlyvideo_api, ems_http_mpegts, ems_http_flv, {ems_http_file, "wwwroot"}]},
    {file_dir, "test/files"}
  ]}
```

How to install plugins
=========

First you may install your own small plugin files. If you are building erlyvideo from source, you may put them into
apps/plugins/src/, than run make, make release and you should get release folder erlyvideo with lib/plugins-...ez with your
compiled files inside.

If you are compiling files outside erlyvideo, than just drop your compiled beam files into /opt/erlyvideo/plugins and erlyvideo will
load all beam files from there automatically with following message:

```
Starting ems_sup:ems_event_sup <0.111.0>
Starting ems_so_sup:shared_objects_sup <0.113.0>
Starting ems_so_sup:shared_object_sup <0.114.0>
Starting ems_sup:ems_so_sup <0.112.0>
Starting ems_http_sup:8082 <0.115.0>
Starting rtmp_sup:rtmp_listener1 <0.116.0>
Load plugin s3pool                         %%%%% This is indicator of loading plugin files
Starting rtsp_sup:rtsp_listener1 <0.117.0>
<0.88.0> {std_info,"Started Erlyvideo"}
===================== Started application: erlyvideo =====================
```

