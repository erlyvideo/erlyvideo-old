private var nc:NetConnection;
private var ns_in:NetStream;
private var ns_out:NetStream;

[Bindable]
private var registerEnabled:Boolean = false;

[Bindable]
private var registerLabel:String = "Register";

[Bindable]
private var callEnabled:Boolean = false;

[Bindable]
private var callLabel:String = "Call";

public function init()  : void
{
	Security.allowDomain("*");
  System.useCodePage = true;
  
  nc = new NetConnection();
  nc.client = this;
  nc.addEventListener(NetStatusEvent.NET_STATUS, handleStatus);
  nc.connect(Application.application.parameters.server+"/phone");
}

private function handleStatus(evt:NetStatusEvent) : void
{
  switch(evt.info.code) {
    case "NetConnection.Connect.Success":
      registerEnabled = true;
      break;
      
    case "NetConnection.SipCall":
      var obj:Object = evt.info.description;
      ns_out = new NetStream(nc);
      
      var m:Microphone = Microphone.getMicrophone();
			//m.rate = 44;
      m.codec = "Speex";
			m.gain = 80;
			m.rate = 8000;
      ns_out.attachAudio(m);
      ns_out.publish(obj.out_stream);
      
      ns_in = new NetStream(nc);
      ns_in.play(obj.in_stream);
      
    default:
      registerEnabled = false;
      callEnabled = false;
      break;
  }
}

public function register() : void
{
  var r:Responder = new Responder(function(reply:Boolean):void {
    if(reply) {
      registerLabel = "Registered";
      callEnabled = true;
    } else {
      registerLabel = "Failed to register";
    }
  });
  nc.call("register", r, registerNumber.text);
}

public function call() : void
{
  var r:Responder = new Responder(function(reply:Boolean):void {
    if(reply) {
      callLabel = "Calling";
    } else {
      callLabel = "Failed to call";
    }
  })
  nc.call("ring", r, callNumber.text);
}