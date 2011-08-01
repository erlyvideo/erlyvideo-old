(function($) { 

Erlyvideo = {
  flowplayer: function(element, path) {
    var server = Erlyvideo.rtmp_server;
    $(element).html('<a id="flowplayer-a-embed" href="'+path+'">video</a>');
    flowplayer(document.getElementById("flowplayer-a-embed"), "/flowplayer/flowplayer-3.2.5.swf?"+(Math.random(10000)), {
      log: {
        // level: 'debug'
        // ,filter: 'org.flowplayer.rtmp.*,org.flowplayer.captions.*,org.flowplayer.core.*,org.flowplayer.model.*'
        // ,filter: 'org.flowplayer.rtmp.*,org.flowplayer.captions.*'
      },
      // streamCallbacks: ["onCuepoint"],
  		plugins: { 
  			rtmp: { 
  				url: '/flowplayer/flowplayer.rtmp-3.1.3.swf', 
  				netConnectionUrl: server
  			},
        captions: {
        	url: '/flowplayer/flowplayer.captions-3.2.2.swf?'+(Math.random(10000)),
        	captionTarget: 'content'
        },
        // configure a content plugin to look good for our purpose
        content: {
      	  url:'/flowplayer/flowplayer.content-3.2.0.swf',
      		top: 10,
      		width: '80%',
      		height:40,
      		backgroundColor: 'black',
      		backgroundGradient: 'none',
      		border: 0,
          textDecoration: 'outline',
      		style: {
      		  'body': {
      			  fontSize: '14',
        			fontFamily: 'Arial',
        			textAlign: 'center',
        			color: '#000000'
      		  }
      		}
        }
      },
      clip: {
        provider: 'rtmp',
        bufferLength: 1,
        cuepointMultiplier: 1,
        autoPlay: true,
        live: true
        //, onCuepoint: function(clip, event) {
        //   alert("hi");
        //   // console.dir(event);
        // }
      }
  	});
  },
  
  jwplayer: function(element, path) {
    var server = Erlyvideo.rtmp_server;
    var flashvars = "provider=rtmp&bufferlength=1&autostart=true&streamer="+server+"&file="+path;
    var html = '\
    <object id="player" classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000" name="player" width="800" height="600"> \
  		<param name="movie" value="/jwplayer/player.swf" /> \
  		<param name="allowfullscreen" value="true" /> \
  		<param name="allowscriptaccess" value="always" /> \
  		<param name="flashvars" value="'+flashvars+'" /> \
  		<embed \
  			type="application/x-shockwave-flash" \
  			id="player2" \
  			name="player2" \
  			src="/jwplayer/player.swf"  \
  			width="800" \
  			height="600" \
  			allowscriptaccess="always" \
  			allowfullscreen="true" \
  			flashvars="'+flashvars+'" \
  		/> \
  	</object> ';
    
    $(element).html(html);
  },
  
  hls: function(element, stream) {
    $(element).html("<video width=640 height=480 src=\""+stream+"\" autoplay controls></video>");
  },
  
  rtmp_server: "rtmp://"+window.location.hostname+"/rtmp",
  
  load_stream_info: function() {
    $.get("/erlyvideo/api/streams", {}, function(streams) {
      Erlyvideo.draw_stream_info(streams);
    });
    // Erlyvideo.stream_load_timer = setTimeout(Erlyvideo.load_stream_info, 3000);
  },
  
  stream_template: "<p>\
  Total clients: {{total}}<br/> \
  Total file clients: {{total_file}}<br/> \
  </p>\
  <table class='table'> \
    <thead><tr><th class='first'>Name</th><th>Play</th><th width='70'>Clients</th><th width='150'>Type</th> \
    <th width='70'>Lifetime</th><th width='70'>DTS Delay</th></tr></thead> \
    <tbody> \
    {{#streams}}<tr>\
      <td class='first'>\
      <a href='#' onclick='Erlyvideo.open_stream_tab(\"{{name}}\"); return false;'>{{name}}</a> \
      </td> \
      <td class='stream-play'>\
      <a href='#' onclick='Erlyvideo.play_stream(\"{{name}}\",\"jwplayer\"); return false;'><span class='jwplayer'></span>{{name}}</a> \
      <a href='#' onclick='Erlyvideo.play_stream(\"{{name}}\",\"flowplayer\"); return false;'><span class='flowplayer'></span>{{name}}</a> \
      <a href='#' onclick='Erlyvideo.play_stream(\"{{name}}\",\"hls\"); return false;'><span class='hls'></span>{{name}}</a> \
      </td>\
      <td>{{client_count}}</td> \
      <td>{{type}}</td> \
      <td>{{lifetime}}</td> \
      <td>{{ts_delay}}</td> \
    </tr>{{/streams}} \
    </tbody></table>",
  
  draw_stream_info: function(streams) {
    var i;
    var total = 0;
    var total_file = 0;
    for(i = 0; i < streams["streams"].length; i++) {
      streams["streams"][i].lifetime = Math.round(streams["streams"][i].last_dts / 1000);
      streams["streams"][i].ts_delay = streams["streams"][i].ts_delay < 5000 ? 0 : Math.round(streams["streams"][i].ts_delay / 1000);
      if(streams["streams"][i].type == "file") {
        streams["streams"][i].ts_delay = 0;
        total_file += streams["streams"][i].client_count;
      }
      total += streams["streams"][i].client_count;
    }
    streams["total"] = total;
    streams["total_file"] = total_file;
    $("#stream-list").html(Mustache.to_html(Erlyvideo.stream_template, streams));
  },
  
  stop_periodic_stream_loader: function() {
    if(Erlyvideo.stream_load_timer) clearTimeout(Erlyvideo.stream_load_timer);
    Erlyvideo.stream_load_timer = undefined;
  },
  
  load_license_info: function() {
    $.get("/erlyvideo/api/licenses", {}, function(reply) {
      var licenses = eval('('+reply+')');
      var i,j;
      for(i = 0; i < licenses["licenses"].length; i++) {
        var vers = [];
        var name = licenses["licenses"][i].name;
        for(j = 0; j < licenses["licenses"][i].versions.length; j++) {
          var ver = licenses["licenses"][i].versions[j];
          vers[vers.length] = {
            version: ver,
            name: name,
            checked: licenses["licenses"][i].current_version == ver
          };
        }
        licenses["licenses"][i].versions = vers;
      }
      if(licenses["licenses"].count > 0) {
        $("#license-list").html(Mustache.to_html(Erlyvideo.license_template, licenses));
      }
    });
  },
  
  enable_licenses: function() {
    $("#license-save-form").submit(function() {
      $.post(this.action, $(this).serialize(), function(reply) {
        reply = eval('('+reply+')');
        if(reply) {
          alert("Licenses loaded, restart erlyvideo to see effects");
        } else {
          alert("Failed to select software versions. Consult logs for details");
        }
      });
      return false;
    });
  },
  
  license_template: "{{#licenses}}<div class=\"column\"> \
  	<div class=\"group\"> \
      <label class=\"label\">{{name}}</label> \
      {{#versions}} \
      <div> \
        <input type=\"radio\" name=\"{{name}}\" class=\"checkbox\" id=\"version_{{name}}_{{version}}\" value=\"{{version}}\" {{#checked}}checked{{/checked}}/> \
        <label for=\"version_{{name}}_{{version}}\" class=\"radio\">{{version}}</label> \
      </div> \
      {{/versions}} \
    </div> \
	</div> \
	{{/licenses}} \
	<div class=\"group navform wat-cf\"> \
    <button class=\"button\" type=\"submit\"> \
      <img src=\"images/icons/tick.png\" alt=\"Save\" /> Save \
    </button> \
    <span class=\"text_button_padding\">or</span> \
    <a class=\"text_button_padding link_button\" href=\"#license\">Cancel</a> \
  </div> \
	",
  
  open_stream_tab: function(stream) {
  },
  
  enable_play_tab: function() {
    $("#play-tab form").submit(function() {
      Erlyvideo.play_stream($("#requested-stream-name").val(), "jwplayer");
      return false;
    });
  },
  
  play_stream: function(stream, player) {
    if(player == "flowplayer") {
      Erlyvideo.flowplayer("#player-embed", stream);
    } else if(player == "jwplayer") {
      Erlyvideo.jwplayer("#player-embed", stream);
    } else if(player == "hls") {
      Erlyvideo.hls("#player-embed", "/hls/"+stream+"/index.m3u8");
    }
    $("#block-login").dialog('open');
  },
  
  activate_tab: function(tabname) {
    Erlyvideo.stop_periodic_stream_loader();
    Erlyvideo.stop_periodic_traffic_loader();
    $(".tabbed-menu li").removeClass("active");
    $("#main .content").hide();
    $("#"+tabname+"-tab").show();
    $(".tabbed-menu a[href=#"+tabname+"]").parent().addClass("active");
    
    if(tabname == "streams") Erlyvideo.load_stream_info();
    if(tabname == "license") Erlyvideo.load_license_info();
    if(tabname == "stats") Erlyvideo.load_traffic_stat();
    return false;
  },
  
  enable_tabs: function() {
    $(".tabbed-menu a, a.link-button").live('click', function() {
      Erlyvideo.activate_tab($(this).attr('href').substring(1));
    });
  },
  
  traffic_template: "<table><caption>Traffic statistics for {{iface}}</caption> \
  <thead><tr><td></td>\
  {{#traffic}}<th>{{time}}</th>{{/traffic}}\
  </tr></thead>\
  <tbody>\
  <tr><td>Input</td>\
  {{#traffic}}<td>{{input}}</td>{{/traffic}} \
  </tr>\
  <tr><td>Output</td>\
  {{#traffic}}<td>{{output}}</td>{{/traffic}} \
  </tr>\
  </tbody>\
  </table>",
  
  load_traffic_stat: function() {
    $.get("/erlyvideo/api/traffic", {}, function(traffic) {
      $("#traffic-stats").html(Mustache.to_html(Erlyvideo.traffic_template, traffic));
      $('#traffic-stats table').visualize({type: 'line', width: '800px'});
    });
    Erlyvideo.traffic_load_timer = setTimeout(Erlyvideo.load_traffic_stat, 3000);
  },
  
  stop_periodic_traffic_loader: function() {
    if(Erlyvideo.traffic_load_timer) clearTimeout(Erlyvideo.traffic_load_timer);
    Erlyvideo.traffic_load_timer = undefined;
  }
};

$.mustache = function(template, view, partials) {
  return Mustache.to_html(template, view, partials);
};


$(function() {
  Erlyvideo.enable_tabs();
  Erlyvideo.enable_licenses();
  if(window.location.hash != "") {
    Erlyvideo.activate_tab(window.location.hash.substring(1));
  } else {
    Erlyvideo.activate_tab("streams");
  }
  Erlyvideo.enable_play_tab();
  // $('#traffic-stats').visualize({type: 'line', width: '800px'});
	
  $("#block-login").dialog({autoOpen:false, title : "Play Stream", width: 840, height: 700});
  
})

})(jQuery);