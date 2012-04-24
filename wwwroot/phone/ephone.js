/*
<script type="text/javascript" src="/ephone.js"></script>

<script type="text/javascript">
  EPhone.swf_opts = {path: "/phone/PhoneButton.swf",  // Path to PhoneButton.swf
                     elid: "PhoneButton",             // DOM Id of embedded element
                     width: 150, height: 50,          // Flash geometry
                     xipath: "expressInstall.swf"};   // Path to express install widget

  EPhone.params = {server :"rtmp://"+window.location.hostname,   // RTMP server URI
                   call_number: "6001",                          // Number for call
                   speex: "8000"};                               // Speex codec bitrate
  EPhone.embed();
</script>

<a href="#" onClick="EPhone.callToggle(); return false;">Call</a>

*/


EPhone = {
    phone: undefined,
    params: {},
    swf_opts: {},
    embed: function() {
        var paramObj = {allowScriptAccess : "always", allowfullscreen : "false", allowNetworking : "all", wmode:"opaque"};
        var attrObj = {};
        swfobject.embedSWF(EPhone.swf_opts.path, EPhone.swf_opts.elid,
                           EPhone.swf_opts.width, EPhone.swf_opts.height,
                           "10.3", EPhone.swf_opts.xipath,
                           EPhone.params, paramObj, attrObj, EPhone.embedding);
    },


    GetSWF:  function (strName) {
        if (window.document[strName] != null)
            if (window.document[strName].length == null)
                return window.document[strName];
        else
            return window.document[strName][1];
        else
            if (document[strName].length == null)
                return document[strName];
        else
            return document[strName][1];

    },
    log: function(s) {
        if(window["console"]) {
            console.log(s);
        } else {
            $(".console-log").prepend("<p>"+s+"</p>");
        }
    },

    callToggle: function() {
        EPhone.log(EPhone.phone);
        EPhone.phone.callToggle()
    },
    embedding: function() {
        EPhone.phone = EPhone.GetSWF(EPhone.swf_opts.elid);
        EPhone.log(EPhone.phone);
    }
};

